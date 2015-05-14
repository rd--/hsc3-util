import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

core :: [String]
core = ["hosc","hsc3"]

plain :: [String]
plain =
    ["hsc3-auditor"
    ,"hsc3-db","hsc3-dot"
    ,"hsc3-lang"
    ,"hsc3-plot"
    ,"hsc3-sf","hsc3-sf-hsndfile"
    ,"sc3-rdu"]

ext :: [String]
ext =
    ["after-pim"
    ,"hosc-json","hosc-utils"
    ,"hsc3-auditor","hsc3-cairo","hsc3-data","hsc3-rec","hsc3-rw","hsc3-unsafe","hsc3-utils"
    ,"hsdif"]

put_l,put_w :: [String] -> IO ()
put_l = putStrLn . unlines
put_w = putStrLn . unwords

pkg_set :: String -> [String]
pkg_set nm =
    case nm of
      "core" -> core
      "plain" -> plain
      "ext" -> ext
      "all" -> concat [core,plain,ext]
      _ -> error "hsc3-setup: unknown pkg_set"

s_echo :: String -> IO ()
s_echo nm = put_w (pkg_set nm)

s_run :: String -> [String] -> IO ExitCode
s_run cmd arg = do
  putStrLn (unwords (cmd : arg))
  rawSystem cmd arg

s_cwd :: String -> IO ()
s_cwd dir = put_w ["cd",dir] >> setCurrentDirectory dir

s_clone :: String -> FilePath -> String -> IO ()
s_clone nm src dst = do
  let f pkg = s_run "darcs" ["get",src </> pkg]
  s_cwd dst >> mapM_ f (pkg_set nm)

s_update :: String -> FilePath -> FilePath -> IO ()
s_update nm src dst = do
  let f pkg = s_cwd (dst </> pkg) >> s_run "darcs" ["pull",src </> pkg]
  mapM_ f (pkg_set nm)

s_at_each :: String -> FilePath -> String -> [String] -> IO ()
s_at_each nm dir cmd args = do
  let f pkg = s_cwd (dir </> pkg) >> s_run cmd args
  mapM_ f (pkg_set nm)

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["echo",nm] -> s_echo nm
    ["clone",nm,src,dst] -> s_clone nm src dst
    ["update",nm,src,dst] -> s_update nm src dst
    ["clean",nm,dir] -> s_at_each nm dir "cabal" ["clean"]
    _ -> putStrLn "hsc3-setup"
