module Tester where
import XMLHelp
import System.IO.Error
import System.FilePath.Posix
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import PGF
import Data.Maybe

---- Tests the parser

type ParsedS = (Sent,[Tree]) 
type Sent = (Id,String)

sentType = fromJust $ readType "Text"
pgfFile = "../gf/BigTest.pgf"

main :: String ->  IO ()
main filePath = do -- do args <- getArgs
        --  case args of
        --     filePath:args' -> do
                sents <- mainF filePath (return . map getSentence)
                res <- testParse getLang $ concat sents 
                let noOk = length $ filter ((/=[]). snd) res
                putStrLn $ "Writing output to file resFile"
                let outp = parsNo noOk ++ (unlines $ map formatRes res)
                writeFile "resFile2.txt" outp 
                putStrLn $ "Results written to: resFile"
                putStrLn $ "Parsed "++show noOk ++" out of totally "++show (length res)
        --        exitSuccess
        --     []      -> do
        --        putStrLn "Give a filename"
        --        exitFailure

parseS :: PGF -> Language -> Sent -> IO ParsedS
parseS pgf lang s = do
  putStrLn $ "try "++snd s
  let mParsed = parse pgf lang sentType (snd s)
  putMsg s mParsed
  return (s,mParsed)

testParse :: Language -> [Sent] -> IO [ParsedS]
testParse lang ss = do
  putStrLn "Parsing input..."
  mpgf <- try (readPGF pgfFile)
  putStrLn "Input parsed..."
  case mpgf of
    Left e    -> error "ingen pgf!!" 
    Right pgf -> mapM (parseS pgf lang) ss

putMsg :: Sent -> [Tree] -> IO ()
putMsg s [] = putStrLn $ show s ++ color red " was not parsed"
putMsg s ps = putStrLn $ show s ++ (color green " was parsed in "
                                      ++show (length ps)++" ways")

parsNo :: Int -> String
parsNo n = "Parsed "++show n++"sentences\n"

getLang :: Language
getLang = fromJust $ readLanguage "BigTestSwe"


formatRes :: ParsedS -> String
formatRes (s,[])  = show s ++ " was not Parsed"++"\n"
formatRes (s,trs) = show s ++ "("++show (length trs)++" trees): \n" ++ unlines (map (showExpr []) trs) ++ "\n"

-- Terminal color output
type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

normal = "\ESC[0m"

bold :: String -> String
bold = ("\ESC[1m" ++)


fgcol :: Int -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"

red = 1
green = 2
yellow = 3

