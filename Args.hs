module Args where

import System.Console.CmdArgs


data HSwf
  = Debug {
    files :: [FilePath],
    incremental :: Bool
  }
  | Decode {
    files :: [FilePath]
  }
  deriving (Show, Data, Typeable)

debugMode = mode $ Debug {
    files       = def   &= args & text "Files to read",
    incremental = False &=        text "Display debug output incrementally"
  }

decodeMode = mode $ Decode {
    files = def &= args & text "Files to decode"
  }

getArgs :: IO HSwf
getArgs = cmdArgs "HSwf v0.1, (C) 2010 Max Bolingbroke" [debugMode, decodeMode]