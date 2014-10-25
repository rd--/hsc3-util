import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Sound.SC3.UGen.Graph.Reconstruct as G {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef as R {- hsc3 -}
import qualified Sound.SC3.Server.Graphdef.Read as R {- hsc3 -}

-- > let sy = "/home/rohan/sw/hsc3-graphs/scsyndef/analog_bubbles.scsyndef"
-- > scsyndef_to_fs sy "/dev/stdout"
scsyndef_to_fs :: FilePath -> FilePath -> IO ()
scsyndef_to_fs sy_nm fs_nm = do
  gr <- R.read_graphdef_file sy_nm
  let nm = dropExtension (takeFileName sy_nm)
      (_,gr') = R.graphdef_to_graph gr
      hs = G.reconstruct_graph_module nm gr'
      imp = "import qualified Sound.SC3.UGen.DB.PP as F"
      mn = ["main :: IO ()"
           ,"main = putStrLn (F.ugen_graph_forth_pp False " ++ nm ++ ")"]
      hs_nm = "/tmp/scsyndef-to-fs.hs"
  writeFile hs_nm (unlines (imp : hs ++ mn))
  _ <- system ("runhaskell " ++ hs_nm ++ " > " ++ fs_nm)
  return ()

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    [] -> scsyndef_to_fs "/dev/stdin" "/dev/stdout"
    [sy] -> scsyndef_to_fs sy "/dev/stdout"
    [sy,fs] -> scsyndef_to_fs sy fs
    _ -> putStrLn "scsyndef-to-fs scyndef [fs]"
