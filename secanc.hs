{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs, 
    NoMonomorphismRestriction,
    OverloadedStrings, PatternSynonyms, QuasiQuotes,
    ScopedTypeVariables, TemplateHaskell, TypeOperators,
    ViewPatterns #-}
module Main where

import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import Frames
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import System.Environment
import CodeAliases

tableTypes' rowGen { rowTypeName = "Secanc"
                    , separator = "\t" }
                    "IncWebR2011head.txt"

secancStream :: Producer Secanc IO ()
secancStream = readTableOpt secancParser "IncWebR2011.txt"

loadSecanc :: IO (Frame Secanc)
loadSecanc = inCoreAoS secancStream

sexFilter s = ((==s) . view sex)
icdFilter icd = ((==icd) . view icd7)
syrFilter y = ((>=y) . view year)
eyrFilter y = ((<=y) . view year)

sexicdyr s icd sy ey = secancStream >-> P.filter (sexFilter s) >-> 
    P.filter (icdFilter icd) >-> 
    P.filter (syrFilter sy) >-> P.filter (eyrFilter ey)

inccols = Map.fromList
    [("inc0",(inc0,"0–ω"))
    , ("inc1",(inc1,"0–4"))
    , ("inc2",(inc2,"5–9"))
    , ("inc3",(inc3,"10–14"))
    , ("inc4",(inc4,"15–19"))
    , ("inc5",(inc5,"20–24"))
    , ("inc6",(inc6,"25–29"))
    , ("inc7",(inc7,"30–34"))
    , ("inc8",(inc8,"35–39"))
    , ("inc9",(inc9,"40–44"))
    , ("inc10",(inc10,"45–49"))
    , ("inc11",(inc11,"50–54"))
    , ("inc12",(inc12,"55–59"))
    , ("inc13",(inc13,"60–64"))
    , ("inc14",(inc14,"65–69"))
    , ("inc15",(inc15,"70–74"))
    , ("inc16",(inc16,"75–79"))
    , ("inc17",(inc17,"80–84"))
    , ("inc18",(inc18,"85–ω"))
    , ("fob70",(foB70,"åldersstandardiserad FoB 70"))
    , ("bef2000",(bef2000,"åldersstandardiserad befolkningen 2000"))
    , ("europa",(europa,"åldersstandardiserad Europas befolkning"))
    , ("worldsegi",(worldSegi,"åldersstandardiserad världsbefolkningen, Segi"))
    , ("worldwho",(worldWHO,"åldersstandardiserad världsbefolkningen, WHO"))]

mkPlot [icdstr, systr, eystr, inccolname, fname] = do
    let icd = T.pack icdstr 
        (Just ial) = Map.lookup icd codealiases 
        (Just inccoldata) = Map.lookup inccolname inccols
        inccol = fst inccoldata 
        inccolal = snd inccoldata 
        sy = read systr
        ey = read eystr

    xsfem <- P.toListM (sexicdyr 2 icd sy ey) 
    xsmale <- P.toListM (sexicdyr 1 icd sy ey) 
    toFile FileOptions {_fo_size=(640,480), _fo_format=SVG} fname $ do
        layout_title .= "Cancerincidens " ++ ial ++ " Sverige"
        layout_x_axis . laxis_title .= "Tid"
        layout_y_axis . laxis_title .= "Fall/100 000 " ++ inccolal
        plot (line "kvinnor" [(map (view year &&& view inccol) xsfem)])
        plot (line "män" [(map (view year &&& view inccol) xsmale)])

main = do
    args <- getArgs
    mkPlot args 
