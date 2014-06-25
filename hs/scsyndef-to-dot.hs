import Control.Monad {- base -}
import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.Server.Synthdef.Read as R

import Sound.SC3.UGen.Dot {- hsc3-dot -}
import qualified Sound.SC3.UGen.Dot.Internal as D

scsyndef_to_dot :: Bool -> FilePath -> IO ()
scsyndef_to_dot opt sy_nm = do
  gr <- R.read_graphdef_file sy_nm
  let dot_nm = replaceExtension sy_nm "dot"
      svg_nm = replaceExtension sy_nm "svg"
      sy = R.graphdef_to_synthdef gr
      dr = D.dotGraph svg_options (synthdefGraph sy)
  writeFile dot_nm dr
  when opt (void (rawSystem "dot" ["-T","svg",dot_nm,"-o",svg_nm]))
  return ()

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> putStrLn "scsyndef-to-dot [-svg] file..."
    "-svg":nm -> mapM_ (scsyndef_to_dot True) nm
    nm -> mapM_ (scsyndef_to_dot False) nm
