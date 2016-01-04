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

isize = 5

inccolal :: String -> String
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

grpal :: String -> String -> String
grpal scol ecol = show startage ++ "–" ++ show endage
    where startage = (read scol - 1) * isize
          endage = (read ecol - 1) * isize + isize - 1

colli li inccolname = map (view inccol) li
    where (Just inccol) = Map.lookup inccolname inccols

colgrpli li scol ecol = map (colli li) icolnames
    where icolnames = map (show) icols 
          icols = [start..end]
          start = read scol :: Int
          end = read ecol :: Int

cuminc li scol ecol = map (*isize) . map sum . transpose $ colgrpli li scol ecol

cumprob li scol ecol = map ((1-) . exp . negate . (/10^5)) (cuminc li scol ecol)

yrzip li yrli n = zip (map (subtract n) $ map (view year) yrli) li 

plists li yrli = yrzip li yrli 0
mapy = 5
malists li yrli = [(yrzip (ma mapy li) yrli $ round $ (mapy-1)/2)]

-- Moving average adapted from http://stackoverflow.com/a/1321775
ma :: Fractional a => Int -> [a] -> [a]
ma p = reverse . map ((/ (fromIntegral p)) . sum . take p) . (drop p) . reverse . tails

femaPlot colfem xsfem [] [] = do
   femPlot colfem xsfem 

femaPlot [] [] colmale xsmale = do
   malePlot colmale xsmale 

femaPlot colfem xsfem colmale xsmale = do
   femPlot colfem xsfem 
   malePlot colmale xsmale 

femPlot col xs = do
    plot (points "kvinnor" (plists col xs))
    plot (line "kvinnor jämnad" (malists col xs))

malePlot col xs = do
    plot (points "män" (plists col xs))
    plot (line "män jämnad" (malists col xs))

mkCumPlot :: [String] -> IO ()
mkCumPlot [icdstr, systr, eystr, scol, ecol, fname] = do
    let icd = T.pack icdstr 
        (Just icdal) = Map.lookup icd codealiases 
        sy = read systr
        ey = read eystr

    xsfem <- P.toListM (sexicdyr 2 icd sy ey) 
    xsmale <- P.toListM (sexicdyr 1 icd sy ey)
    let colfem = cumprob xsfem scol ecol
        colmale = cumprob xsmale scol ecol
    toFile FileOptions {_fo_size=(800,600), _fo_format=SVG} fname $ do
        layout_title .= "Kumulativ cancerrisk " ++ icdal ++ " " 
                ++ grpal scol ecol ++ " Sverige"
        layout_x_axis . laxis_title .= "Tid"
        layout_y_axis . laxis_title .= "P(x)"
        femaPlot colfem xsfem colmale xsmale

mkPlot :: [String] -> IO ()
mkPlot [icdstr, systr, eystr, inccolname, fname] = do
    let icd = T.pack icdstr 
        (Just icdal) = Map.lookup icd codealiases 
        sy = read systr
        ey = read eystr

    xsfem <- P.toListM (sexicdyr 2 icd sy ey) 
    xsmale <- P.toListM (sexicdyr 1 icd sy ey) 
    let colfem = colli xsfem inccolname
        colmale = colli xsmale inccolname
    toFile FileOptions {_fo_size=(800,600), _fo_format=SVG} fname $ do
        layout_title .= "Cancerincidens " ++ icdal ++ " Sverige"
        layout_x_axis . laxis_title .= "Tid"
        layout_y_axis . laxis_title .= "Fall/100 000 " ++ inccolal inccolname
        femaPlot colfem xsfem colmale xsmale

parse :: [String] -> IO ()
parse ("-c":xs) = mkCumPlot xs
parse ("-i":xs) = mkPlot xs

main :: IO ()
main = getArgs >>= parse
