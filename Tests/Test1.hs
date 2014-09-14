module Test1 where

import Appraisal.Config (Top)
import Appraisal.File (File(fileChksum))
import Appraisal.ImageFile (ImageFile(imageFile))
import Appraisal.Report (Report)
import Appraisal.ReportImage (ReportImage(picOriginal, picMustEnlarge))
import Appraisal.Utils.ErrorWithIO (ErrorWithIO)
import Control.Monad.Error (ErrorT(runErrorT))
import Data.Generics (everywhereM, mkM)
import Prelude hiding (concatMap, dropWhile, log, readFile, writeFile)

modifyReport :: Top -> Report -> ErrorWithIO Report
modifyReport ver r =
    -- everywhere (mkT setEnlargementFlag) r
    everywhereM (mkM setEnlargementFlag) r
    where
      -- Set the picEnlargment bit of the image captioned "Hunting in Autumn" and
      -- the landscape shaped image in item 7.
      setEnlargementFlag :: ReportImage -> ErrorWithIO ReportImage
      setEnlargementFlag pic =
          case picOriginal pic of
            Nothing -> return pic
            Just (Left _) -> return pic
            Just (Right img) ->
                if elem (fileChksum (imageFile img)) [ "02e1e81e708d186b372bf556832432d2", "6b4942b2912862e0a74e8b745e7eb963" ]
                then return (pic {picMustEnlarge = True})
                else return pic

