module LaTeX where

-- | Open a report and generate its LaTeX file

import Appraisal.Config (Top(Top), Paths(images))
import Appraisal.ImageCache (runImageCacheIO)
import Appraisal.ReportAbbrevs (latexFromMarkupTest)
import Appraisal.ReportIO (reportPrinterImages, reportEnlargedImages)
import Appraisal.ReportLaTeX(reportToLaTeX)
import Appraisal.ReportMap (ReportID(ReportID))
import Appraisal.Utils.UUID (fromString)
import Base (loadReport, loadImageCache)
import Control.Exception (try)
import Control.Monad.Error
import Data.Algorithm.Diff (Diff)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.ByteString.Lazy as B (ByteString, readFile, take)
import Data.Digest.Pure.MD5 (md5)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Text (Text, split, unpack)
import Data.Text.IO as T (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (proc, CreateProcess(cwd), readProcess)
import System.Process.Text (readCreateProcessWithExitCode)
import Test.HUnit
import Text.LaTeX (render)
import Text.PrettyPrint (Doc, text)

data ReportInfo
    = ReportInfo
      { reportId :: String
      , pdfHeadLength :: Int64
      }

instance Eq Doc where
    a == b = show a == show b

tests :: Test
tests = TestList (map latexRenderTest reports ++ map latexRunTest reports)

reports :: [ReportInfo]
reports =
    [ ReportInfo "f7c5f607-ffd8-48bd-bf56-79b6708bfcd6" 1116649
    , ReportInfo "84923fa1-5ea3-47aa-ab73-d3428d98a1e2" 2539430
    , ReportInfo "cb26abd0-af8d-44a1-9176-3d4d21184bcd" 33744749
    , ReportInfo "f9e4d387-1f31-4a76-8ec5-d5a01e195a08" 8863100
    , ReportInfo "638e563e-37f3-4071-9563-246bbaf0f769" 61825500 ]

latexRenderTest :: ReportInfo -> Test
latexRenderTest r@(ReportInfo rid _) =
    TestLabel rid $ TestCase $ do
      latex <- latexRender r
      expected <- T.readFile ("testdata/latexTest" </> rid </> "expected.ltx")
      let diff :: [[Diff [Text]]]
          diff = contextDiff 2 (split (== '\n') expected) (split (== '\n') latex)
      assertEqual "LaTeX Render test" mempty (prettyDiff (text "expected") (text "but got") (text . unpack) diff)

latexRender :: ReportInfo -> IO Text
latexRender (ReportInfo rid _) = do
  Just report <- loadReport ver (ReportID (fromJust (fromString rid)))
  st <- loadImageCache ver
  Right picPrinter <- runErrorT $ runImageCacheIO (reportPrinterImages report) (images ver) st
  Right picEnlarged <- runErrorT $ runImageCacheIO (reportEnlargedImages report) (images ver) st
  return $ render (reportToLaTeX ver latexFromMarkupTest picPrinter picEnlarged report)
    where
      ver = Top "/srv/appraisalscribe-development"

latexRunTest :: ReportInfo -> Test
latexRunTest r@(ReportInfo rid len) =
    TestLabel rid $ TestCase $ do

      let deps = ["libjpeg-progs", "netpbm", "texlive-xetex",
                  "texlive-fonts-extra", "texlive-fonts-recommended", "texlive-latex-extra", "texlive-latex-recommended",
                  "latex-cjk-chinese", "latex-cjk-japanese", "latex-cjk-korean", "latex-cjk-thai", "fonts-arphic-ukai"]
      -- dpkg-query always returns ExitSuccess, but if the package is
      -- missing its version string will be empty.
      missing <- filterM (\ name -> readProcess "dpkg-query" ["-W", "-f", "${Version}", name] "" >>= return . null) deps
      when (not (null missing)) (error $ "latexRunTest - missing dependencies: " ++ show missing)

      expected <- B.readFile ("testdata/latexTest" </> rid </> "expected.pdf") >>= return . B.take len
      expectedXeTeXStdout <- T.readFile ("testdata/latexTest" </> rid </> "expectedXeTeXStdout")
      let expectedMD5 = md5 expected

      latex <- latexRender r
      writeFile ("testdata/latexTest" </> rid </> "rendered.ltx") latex
      _ <- readCreateProcessWithExitCode ((proc "xelatex" ["-interaction=nonstopmode", "rendered.ltx"]) {cwd = Just ("testdata/latexTest" </> rid)}) mempty
      _ <- readCreateProcessWithExitCode ((proc "xelatex" ["-interaction=nonstopmode", "rendered.ltx"]) {cwd = Just ("testdata/latexTest" </> rid)}) mempty
      (code, out, err) <- readCreateProcessWithExitCode ((proc "xelatex" ["-interaction=nonstopmode", "rendered.ltx"]) {cwd = Just ("testdata/latexTest" </> rid)}) mempty
      writeFile ("testdata/latexTest" </> rid </> "renderedXeTeXStdout") out
      pdf <- try (B.readFile ("testdata/latexTest" </> rid </> "rendered.pdf") >>= return . B.take len) :: IO (Either IOError ByteString)

      let pdfMD5 = either Left (Right . md5) pdf
          diff :: [[Diff [Text]]]
          diff = contextDiff 2 (split (== '\n') expectedXeTeXStdout) (split (== '\n') out)
          outDiff = show (prettyDiff (text "expected") (text "but got") (text . unpack) diff)
      assertEqual "LaTeX Run Test" ((ExitSuccess, mempty, mempty), (Right expectedMD5)) ((code, outDiff, err), pdfMD5)
