module Main where

import Parser (parseRegex, Regex)
import Regex
import Options.Applicative

-- some code for processing command line arguments

data Input = FileInput FilePath | StdInput String deriving Show

data Options
  = Options {
      regex :: String
    , input :: Input
    } deriving Show

inputParser :: Parser Input
inputParser = fileInput <|> stdInput

fileInput :: Parser Input
fileInput
  = FileInput <$> strOption ( long "file"
                            <> short 'f'
                            <> metavar "FILENAME"
                            <> help "Input file name")

stdInput :: Parser Input
stdInput
  = StdInput <$> strOption (  long "input"
                           <> short 'i'
                           <> metavar "TEXTINPUT"
                           <> help "Text to be parsed")

optionsParser :: Parser Options
optionsParser
  = Options <$> strOption (long "Regex"    <>
                           short 'R'       <>
                           metavar "REGEX" <>
                           help "regular expression to be used in the parsing algorithm")
                <*> inputParser


main :: IO ()
main = grep =<< execParser opts
       where
         opts = info (optionsParser <**> helper)
                     (  fullDesc
                     <> progDesc "RE parsing tool"
                     <> header "Virtual Machine-based RE parsing tool")

grep :: Options -> IO ()
grep (Options re inp)
  = either regexParseError
           (execute inp)
           (parseRegex re)

regexParseError :: String -> IO ()
regexParseError = putStrLn

-- here is the main algorithm execution.
-- idea: we break the input into lines and execute the algorithm
-- on each line. If a match is found in the current line it is
-- printed, otherwise nothing is done.

execute :: Input -> Regex -> IO ()
execute (FileInput name) re
  = mapM_ (exec re) . lines =<< readFile name
execute (StdInput inp) re
  = mapM_ (exec re) (lines inp)

exec :: Regex -> String -> IO ()
exec re s
  = maybe (return ())
          (const (putStrLn s))
          (substring s re)
