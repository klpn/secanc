{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs, 
    NoMonomorphismRestriction,
    OverloadedStrings, PatternSynonyms, QuasiQuotes,
    ScopedTypeVariables, TemplateHaskell, TypeOperators,
    ViewPatterns #-}
module Main where

import Data.List
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
    [("0",inc0)
    , ("1",inc1)
    , ("2",inc2)
    , ("3",inc3)
    , ("4",inc4)
    , ("5",inc5)
    , ("6",inc6)
    , ("7",inc7)
    , ("8",inc8)
    , ("9",inc9)
    , ("10",inc10)
    , ("11",inc11)
    , ("12",inc12)
    , ("13",inc13)
    , ("14",inc14)
    , ("15",inc15)
    , ("16",inc16)
    , ("17",inc17)
    , ("18",inc18)
    , ("fob70",foB70)
    , ("bef2000",bef2000)
    , ("europa",europa)
    , ("worldsegi",worldSegi)
    , ("worldwho",worldWHO)]

icolgrps = Map.fromList
    [("0-74", [1..15])
    , ("0-84", [1..17])
    , ("0-14", [1..3])
    , ("15-44", [4..9])
    , ("45-64", [10..13])
    , ("65-74", [14,15])
    , ("75-84", [16,17])]

isize = 5

inccolal "0" = "0–ω"
inccolal "18" = "85–ω"
inccolal "fob70" = "åldersstandardiserad FoB 70"
inccolal "bef2000" = "åldersstandardiserad befolkningen 2000"
inccolal "europa" = "åldersstandardiserad Europas befolkning"
inccolal "worldsegi" = "åldersstandardiserad världsbefolkningen, Segi"
inccolal "worldwho" = "åldersstandardiserad världsbefolkningen, WHO"

inccolal x = show istart ++ "–" ++ show iend
    where istart = (read x - 1) * isize
          iend = istart + isize - 1

colli li inccolname = map (view inccol) li
    where (Just inccol) = Map.lookup inccolname inccols

colgrpli li grp = map (colli li) icolnames
    where icolnames = map (show) icols 
          (Just icols) = Map.lookup grp icolgrps

cuminc li grp = map (*isize) . map sum . transpose $ colgrpli grp li

cumprob li grp = map ((1-) . exp . negate . (/10^5)) (cuminc grp li)

yrzip li yrli = zip (map (view year) yrli) li 

emdash str = map (\c -> if c=='-' then '–'; else c) str

mkCumPlot [icdstr, systr, eystr, grp, fname] = do
    let icd = T.pack icdstr 
        (Just icdal) = Map.lookup icd codealiases 
        sy = read systr
        ey = read eystr

    xsfem <- P.toListM (sexicdyr 2 icd sy ey) 
    xsmale <- P.toListM (sexicdyr 1 icd sy ey) 
    toFile FileOptions {_fo_size=(640,480), _fo_format=SVG} fname $ do
        layout_title .= "Kumulativ sannolikhet " ++ icdal ++ " " 
                ++ emdash grp ++ " Sverige"
        layout_x_axis . laxis_title .= "Tid"
        layout_y_axis . laxis_title .= "P(x)"
        plot (line "kvinnor" [(yrzip (cumprob xsfem grp) xsfem)])
        plot (line "män" [(yrzip (cumprob xsmale grp) xsmale)])

mkPlot [icdstr, systr, eystr, inccolname, fname] = do
    let icd = T.pack icdstr 
        (Just icdal) = Map.lookup icd codealiases 
        sy = read systr
        ey = read eystr

    xsfem <- P.toListM (sexicdyr 2 icd sy ey) 
    xsmale <- P.toListM (sexicdyr 1 icd sy ey) 
    toFile FileOptions {_fo_size=(640,480), _fo_format=SVG} fname $ do
        layout_title .= "Cancerincidens " ++ icdal ++ " Sverige"
        layout_x_axis . laxis_title .= "Tid"
        layout_y_axis . laxis_title .= "Fall/100 000 " ++ inccolal inccolname
        plot (line "kvinnor" [(yrzip (colli xsfem inccolname) xsfem)])
        plot (line "män" [(yrzip (colli xsmale inccolname) xsmale)])

parse ("-c":xs) = mkCumPlot xs
parse ("-i":xs) = mkPlot xs

main = getArgs >>= parse
