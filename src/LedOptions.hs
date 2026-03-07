module LedOptions
  ( optionsParser
  , Options(..)
  , opts
  ) where

import Options.Applicative

data Options = Options
  { optPrompt    :: Maybe Text
  , optSilent    :: Bool
  , optExecFiles :: [FilePath]
  , optFiles     :: [FilePath]
  } deriving stock (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption (short 'p' <> metavar "STRING" <> help "Set command prompt"))
  <*> switch (short 's' <> help "Suppress byte counts and ! in the output")
  <*> many (strOption (short 'e' <> metavar "FILE" <> help "Run script on startup (may be repeated)"))
  <*> many (strArgument (metavar "FILE..." <> help "Files to edit"))

opts :: ParserInfo Options
opts = info (optionsParser <**> helper) $
  fullDesc
  <> progDesc "The line editor"
  <> header "led"
