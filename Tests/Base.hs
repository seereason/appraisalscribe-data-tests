{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Base
    ( testReport
    , loadReport
    , loadImageCache
    ) where

import Appraisal.Cache
import Appraisal.Config (Top, Paths(top, state, images))
import Appraisal.File (ImageCacheTop, File(fileChksum), fileCachePath)
import Appraisal.ImageCache (ImageCacheIO, ImageCacheMap, ImageCacheState, runImageCacheIO, fileCachePath')
import Appraisal.ImageFile (ImageFile(imageFile))
import Appraisal.Report (Report)
import Appraisal.ReportAbbrevs (latexFromMarkup {-latexFromMarkupTest-})
import Appraisal.ReportImage (ReportImage(Pic, picSize, picMustEnlarge, picOriginal))
import Appraisal.ReportImageCache(printedKey, enlargedKey)
import Appraisal.ReportIO (validateImages, reportPrinterImages, reportEnlargedImages)
import Appraisal.ReportLaTeX (reportToLaTeX)
import Appraisal.ReportMap (ReportID(..), ReportMap(..))
import Appraisal.Utils.Debug (Ident(ident'), ident)
import Appraisal.Utils.ErrorWithIO (ErrorWithIO, io)
import Appraisal.Utils.Generics (gFind)
import Appraisal.Utils.UUID as UUID (UUID, fromString)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket, throw)
import Control.Monad ((=<<), foldM)
import Control.Monad.Error (ErrorT(ErrorT, runErrorT))
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.Trans (liftIO, lift)
import Data.Acid (AcidState, closeAcidState, makeAcidic, query, Query)
import Data.Acid.Local (openLocalStateFrom)
import Data.ByteString (ByteString, readFile, writeFile)
import Data.ByteString.Lazy (fromStrict)
import Data.Data (Data, Typeable)
import Data.Digest.Pure.MD5 (md5)
import Data.FileEmbed (embedFile)
import Data.Generics (everywhere, mkT, everywhereM, mkM)
import Data.List as List (map)
import Data.Map as Map (lookup, Map, insert, findWithDefault, map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (concatMap, dropWhile, pack, singleton, Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.URI (URI)
import Prelude hiding (concatMap, dropWhile, log, readFile, writeFile)
import qualified Prelude (writeFile)
import System.Directory (removeFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (Priority(DEBUG), rootLoggerName, setHandlers, setLevel, updateGlobalLogger, logM)
import System.Process.ByteString (readProcessWithExitCode)
import Test.HUnit (assertEqual, Counts(errors, failures), runTestTT, Test(TestCase, TestLabel))
import Text.LaTeX.Base.Render as LaTeX (render)

instance Ident ReportImage where
    ident' x = maybe (Just (show x)) Just (ident' (picOriginal x))

instance Ident ImageFile where
    ident' x = ident' (imageFile x)

instance Ident File where
    ident' x = Just (fileChksum x)

-- Copy of the AppraisalData, ReportID, ReportMap types from
-- appraisalscribe, with their safecopy instances.
data AppraisalData
    = AppraisalData
      { unAppraisalData :: ReportMap
      , trashCan :: ReportMap
      } deriving (Eq, Ord, Read, Show, Typeable, Data)

$(deriveSafeCopy 1 'base ''AppraisalData)

-- This looks up a Report by it's place in the list.  Bad dog. Will go away soon.
insecureQueryReportByID :: ReportID -> Query AppraisalData (Maybe Report)
insecureQueryReportByID rid = Map.lookup rid . unReportMap . unAppraisalData <$> ask

$(makeAcidic ''AppraisalData [
                  'insecureQueryReportByID
                 ])

loadReport :: Paths p => p -> ReportID -> IO (Maybe Report)
loadReport p rid =
    bracket (openLocalStateFrom (state p </> "appraisalData") (error $ "loadReport " ++ state p </> "appraisalData"))
            closeAcidState $ \ appraisalData ->
    query appraisalData (InsecureQueryReportByID rid)

-- main = loadReport >>= putStrLn . show

data Result =
    Result
    { code :: ExitCode
    , stdout :: String
    , stderr :: String
    , ltx :: String
    , log :: String }
    deriving (Eq, Show)

loadImageCache :: Paths p => p -> IO ImageCacheState
loadImageCache p =
    bracket (openLocalStateFrom (state p </> "imageCache") (error $ "loadImageCache " ++ state p </> "imageCache"))
            closeAcidState $ \ imageCache ->
    return imageCache

-- | Load the report from testdata/stateN/appraisalData/checkpoint,
-- apply the modify function to it, run it and compare the result to
-- the data in testdata/expectedN.
testReport :: Paths p => (Report -> ErrorWithIO Report) -> p -> Test
testReport modifyReport ver =
    TestLabel (top ver) $ TestCase $ withValueCache (top ver </> "_state/imageCache") $ testReport' modifyReport ver

testReport' :: Paths p => (Report -> ErrorWithIO Report) -> p -> ImageCacheState -> IO ()
testReport' modifyReport ver st =
    do debugLog <- fileHandler (top ver </> "log") DEBUG
       updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [debugLog])
       logM "test" DEBUG ("\nTest " ++ top ver ++ " Starting")
       Just report0 <- loadReport ver (ReportID $ fromJust (fromString "f9e4d387-1f31-4a76-8ec5-d5a01e195a08"))
       ltxActual <- runErrorT (testReport'' modifyReport ver st report0) >>= either (error . show) return
       writeFile "report.ltx" (encodeUtf8 ltxActual :: ByteString)

       -- Run XeLaTeX to generate the PDF file
       removeFile "report.pdf" `catchIOError` (\ e -> if isDoesNotExistError e then return () else throw e)
       (code1, _, _) <- readProcessWithExitCode "xelatex" ["report.ltx"] ""
       (code2, _, _) <- readProcessWithExitCode "xelatex" ["report.ltx"] ""
       (code, out, err) <- readProcessWithExitCode "xelatex" ["report.ltx"] ""
       logM "test" DEBUG ("Test " ++ top ver ++ " Finished")

       -- Load all the output files and see how they compare to the expected output.
       logActual <- readFile "report.log" >>= return . dropWhile (/= '\n') . decodeUtf8 :: IO Text
       writeFile "report.out" out
       writeFile "report.err" err
       expectedLtx <- readFile $ top ver </> "expected" </> "report.ltx"
       expectedLog <- readFile $ top ver </> "expected" </> "report.log"

       readProcessWithExitCode "diff" ["-ru", top ver </> "expected" </> "report.out", "report.out"] "" >>= \ (_, s, _) -> putStr (unpack (decodeUtf8 s))
       expect <- expected ver
       assertEqual (top ver) expect (Result code (checksum out) (unpack (decodeUtf8 err)) (checksum (encodeUtf8 ltxActual)) (checksum (encodeUtf8 logActual)))

testReport'' :: Paths p => (Report -> ErrorWithIO Report) -> p -> ImageCacheState -> Report -> ErrorWithIO Text
testReport'' modifyReport ver st report0 =
    runImageCacheIO (testReport''' modifyReport ver report0) (images ver) st

testReport''' :: Paths p => (Report -> ErrorWithIO Report) -> p -> Report -> ImageCacheIO ImageCacheTop Text
testReport''' modifyReport ver report0 =
    do validateImages report0
       report <- lift $ lift $ modifyReport report0
       validateImages report
       -- trace ("report=" ++ show report) (return ())
       -- Write out the haskell expression for the report
       liftIO $ Prelude.writeFile "report.hs" (show report)

       let images = gFind report :: [ReportImage]

       -- Get the resized images for the regular and enlarged printed images.
       picPrinter <- reportPrinterImages report
       picEnlarged <- reportEnlargedImages report
       -- Build the LaTeX output and convert it to Text
       let latex = reportToLaTeX ver latexFromMarkup picPrinter picEnlarged
                     -- (\ x -> Map.findWithDefault (error $ "picPrinter: " ++ show (ident x) ++ ", " ++ show (Map.map ident picPrinter)) x picPrinter)
                     -- (\ x -> Map.findWithDefault (error $ "picEnlarged: " ++ show (ident x) ++ ", " ++ show (Map.map ident picEnlarged)) x picEnlarged)
                     report
       return (LaTeX.render latex :: Text)


expected :: Paths p => p -> IO Result
expected ver =
    Result <$> return ExitSuccess
           <*> (checksum <$> readFile (top ver </> "expected" </> "report.out"))
           <*> return mempty
           <*> (checksum <$> readFile (top ver </> "expected" </> "report.ltx"))
           <*> (checksum <$> readFile (top ver </> "expected" </> "report.log"))

checksum =  show . md5 . fromStrict
