import System.Environment {- base -}

import Sound.SC3.UGen.DB {- hsc3-db -}
import Sound.SC3.UGen.DB.Record {- hsc3-db -}

main :: IO ()
main = do
  arg <- getArgs
  let usage = putStrLn "default-param ugen-name ..."
  case arg of
    [nm] -> case uLookup_ci nm of
              Just u -> putStrLn (u_default_param u)
              Nothing -> usage
    _ -> usage
