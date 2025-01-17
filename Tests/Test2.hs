{-# LANGUAGE OverloadedStrings #-}
module Test2 where

import Appraisal.Abbrevs (AbbrevsM, runAbbrevs')
import Appraisal.Markdown
import Appraisal.Markup (Markup(..), rawMarkdown, markupText)
import Appraisal.ReportAbbrevs (latexFromMarkup)
import Appraisal.Utils.Pandoc (pandocFromMarkdown)
import Control.Monad.Identity
import Control.Monad.State
import Data.Monoid
import Data.Text
import Prelude
import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)
import Test.HUnit

test :: Test
test = TestCase (assertEqual "test2" expected actual)

actual :: LaTeX
actual = runAbbrevs' latexFromMarkup $ execLaTeXT $ latexFromPandoc $ pandoc

pandoc = pandocFromMarkdown $ markupText $ markdown

markdown :: Markup
markdown = rawMarkdown "# Section\n\n## SubSection\n\nAccording to Arnold Chang, the subject property is a good work of a type much sought after by collectors.  It dates from the 1940s when Zhang Daqian famously went to the Buddhist caves at Dunhuang in Gansu Province to study and produce paintings after the Buddhist subjects there. According to Mr. Chang, Buddhist deity  paintings produced in this period rarely appeared at auction prior to 2009, so this work would have been very fresh to the market in September of 2008. \n\nA search of auction records yielded three similar era and subject matter paintings. A \"Figure of a Bodhisattva\" copied from a Buddhist mural at the Mogao Caves at Dunhuang sold through Beijing Rongbao Auctions on May 15th, 2005 for US$172,705. Like the subject property, it depicts a standing deity and has a single line inscription stating that it is modeled after a Tang period mural. On April 25th, 2004 a painting depicting a standing Guanyin with a single line inscription indicating that is also from this period, signed and with two seals of the artist sold through Christie's Hong Kong for $US151,763 (Figure 3). Another painting from this period, \"Guanyin on a Lotus Seat\", 1944, sold through Sotheby's Hong Kong on May 1st, 2005 for $489,231 (Figure 4). It is a larger more elaborate painting with a long inscription by the artist at the top and is signed and has three seals of the artist. \n\nOf these three paintings, the subject property is most similar to the \"Figure of Bodhisattva\" (Figure 2). Both paintings feature a standing deity rendered in colors in an archaic style, and have a single line inscription identifying the subject and its location and that the artist copied directly from a Tang Dynasty mural. Therefore the realized sale of this painting is the basis of the Fair Market Conclusion of the subject property.\n\n1. Lot 159\nTitle: Figure of Bodhisattva \nMedium: Ink and color on gold paper\nInscribed, signed and one seal of the artist\nSize: 50.4 inches 128 cm \nAuction house: Beijing Rongbao Auctions \nDate of sale: 5/15/05 \nPrice: US$172,705, RMB1,430,000 (exchange rate 8.28).\n\n2. Lot 90\nTitle: Early Tang image of a Lady, Guanyin\nMedium: Ink and color on paper\nInscribed, signed and two seals of the artist\nAuction house: Christies, Hong Kong\nDate of Sale: 4/25/04\nSize: 44.88 inches x 23 .6 inches\nSales Price: $US151,763, HKD1,183,750 (exchange rate 7.80) \n\n3. Lot 258\nTitle: Guanyin on a Lotus Seat\nMedium: Ink and color on paper\nSize: 62.6 inches x 27.75 inches \nDate: 1944\nAuction House: Sotheby's Hong Kong\nDate of sale: 5/1/05\nSales price: $489,231, HKD3,816,000 (exchange rate 7.80)\n\n#Section\n##SubSection\nMore text\n"

expected :: LaTeX
expected = TeXSeq
            (TeXComm "section" [FixArg (TeXRaw "Section")])
            (TeXSeq
             (TeXRaw "\n")
             (TeXSeq
              (TeXRaw "\n")
              (TeXSeq
               (TeXComm "subsection" [FixArg (TeXRaw "SubSection")])
               (TeXSeq
                (TeXRaw "\n")
                (TeXSeq
                 (TeXRaw "\n")
                 (TeXSeq
                  (TeXRaw "According to Arnold Chang, the subject property is a good work of a type much sought after by collectors. It dates from the 1940s when Zhang Daqian famously went to the Buddhist caves at Dunhuang in Gansu Province to study and produce paintings after the Buddhist subjects there. According to Mr.\160Chang, Buddhist deity paintings produced in this period rarely appeared at auction prior to 2009, so this work would have been very fresh to the market in September of 2008.")
                  (TeXSeq
                   (TeXRaw "\n\n")
                   (TeXSeq
                    (TeXRaw "\n")
                    (TeXSeq
                     (TeXRaw "A search of auction records yielded three similar era and subject matter paintings. A \8220Figure of a Bodhisattva\8221 copied from a Buddhist mural at the Mogao Caves at Dunhuang sold through Beijing Rongbao Auctions on May 15th, 2005 for US\\$172,705. Like the subject property, it depicts a standing deity and has a single line inscription stating that it is modeled after a Tang period mural. On April 25th, 2004 a painting depicting a standing Guanyin with a single line inscription indicating that is also from this period, signed and with two seals of the artist sold through Christie\8217s Hong Kong for \\$US151,763 (Figure 3). Another painting from this period, \8220Guanyin on a Lotus Seat\8221, 1944, sold through Sotheby\8217s Hong Kong on May 1st, 2005 for \\$489,231 (Figure 4). It is a larger more elaborate painting with a long inscription by the artist at the top and is signed and has three seals of the artist.")
                     (TeXSeq
                      (TeXRaw "\n\n")
                      (TeXSeq
                       (TeXRaw "\n")
                       (TeXSeq
                        (TeXRaw "Of these three paintings, the subject property is most similar to the \8220Figure of Bodhisattva\8221 (Figure 2). Both paintings feature a standing deity rendered in colors in an archaic style, and have a single line inscription identifying the subject and its location and that the artist copied directly from a Tang Dynasty mural. Therefore the realized sale of this painting is the basis of the Fair Market Conclusion of the subject property.")
                        (TeXSeq
                         (TeXRaw "\n\n")
                         (TeXSeq
                          (TeXRaw "\n")
                          (TeXSeq
                           (TeXEnv
                            "enumerate"
                            []
                            (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "item" [OptArg (TeXRaw "$\\bullet$")]) (TeXSeq (TeXRaw "Lot 159 Title: Figure of Bodhisattva Medium: Ink and color on gold paper Inscribed, signed and one seal of the artist Size: 50.4 inches 128 cm Auction house: Beijing Rongbao Auctions Date of sale: 5/15/05 Price: US\\$172,705, RMB1,430,000 (exchange rate 8.28).") (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "item" [OptArg (TeXRaw "$\\bullet$")]) (TeXSeq (TeXRaw "Lot 90 Title: Early Tang image of a Lady, Guanyin Medium: Ink and color on paper Inscribed, signed and two seals of the artist Auction house: Christies, Hong Kong Date of Sale: 4/25/04 Size: 44.88 inches x 23 .6 inches Sales Price: \\$US151,763, HKD1,183,750 (exchange rate 7.80)") (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "item" [OptArg (TeXRaw "$\\bullet$")]) (TeXSeq (TeXRaw "Lot 258 Title: Guanyin on a Lotus Seat Medium: Ink and color on paper Size: 62.6 inches x 27.75 inches Date: 1944 Auction House: Sotheby\8217s Hong Kong Date of sale: 5/1/05 Sales price: \\$489,231, HKD3,816,000 (exchange rate 7.80)") (TeXRaw "\n")))))))))))
                           (TeXSeq
                            (TeXRaw "\n")
                            (TeXSeq (TeXComm "section" [FixArg (TeXRaw "Section")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "subsection" [FixArg (TeXRaw "SubSection")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXRaw "\n") (TeXSeq (TeXRaw "More text") (TeXRaw "\n\n"))))))))))))))))))))))))
