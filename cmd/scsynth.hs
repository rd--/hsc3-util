import Control.Exception {- base -}
import Control.Monad {- base -}
import Control.Monad.IO.Class {- base -}
import qualified Data.Tree as T {- containers -}
import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}

import Sound.Osc {- hosc -}
import Sound.Osc.Text {- hosc3 -}
import Sound.Sc3 {- hsc3 -}

import qualified Sound.Sc3.Server.Graphdef as Graphdef {- hsc3 -}
import qualified Sound.Sc3.Server.Graphdef.Binary as  Graphdef.Binary {- hsc3 -}
import qualified Sound.Sc3.Server.Graphdef.Io as  Graphdef.Io {- hsc3 -}
import qualified Sound.Sc3.Server.Graphdef.Read as Graphdef.Read {- hsc3 -}
import qualified Sound.Sc3.Server.Graphdef.Text as Graphdef.Text {- hsc3 -}
import qualified Sound.Sc3.Server.Nrt.Stat as Nrt {- hsc3 -}
import qualified Sound.Sc3.Ugen.Graph.Reconstruct as Reconstruct {- hsc3 -}

import qualified Sound.File.Next as Sf {- hsc3-sf -}

-- * Util

kv_table_pp :: [(String,String)] -> [String]
kv_table_pp tbl =
    let lm = maximum (map length (map fst tbl))
        pp (k,v) = k ++ replicate (lm - length k) ' ' ++ " : " ++ v
    in map pp tbl

-- * Buffer

-- > buffer_free_range 0 100
buffer_free_range :: Int -> Int -> IO ()
buffer_free_range b0 bN = withSc3 (mapM_ (\n -> async (b_free n)) [b0 .. bN])

-- > buffer_query 0
buffer_query :: Int -> IO ()
buffer_query n = do
  (n',nf,nc,sr) <- withSc3 (b_query1_unpack n)
  let k = map snd b_info_fields
      v = [show n',show nf,show nc,show sr]
  putStrLn (unlines (kv_table_pp (zip k v)))

buffer_store :: Int -> FilePath -> IO ()
buffer_store n fn = do
  ((_,nf,nc,sr),d) <- withSc3 (b_fetch_hdr 512 n)
  let hdr = Sf.Sf_Header nf Sf.Float (round sr) nc
  Sf.au_write fn hdr d

buffer_store_seq :: Int -> Double -> Bool -> FilePath -> IO ()
buffer_store_seq n dt iso dir = do
  let run = do t <- time
               let t' = if iso then show t else show (ntpr_to_ntpi t)
                   fn = dir </> t' <.> "au"
               buffer_store n fn
               pauseThread dt
  forever run

-- * Clear

clear_all :: IO ()
clear_all = withSc3 (sendBundle (bundle immediately [g_freeAll [0],clearSched]))

-- * Dump Osc

-- > dump_osc 1
dump_osc :: Int -> IO ()
dump_osc md = withSc3 (sendMessage (dumpOsc (toEnum md)))

-- * Group

-- > group_query_tree 0
group_query_tree :: Int -> IO ()
group_query_tree n = do
  qt <- withSc3 (g_queryTree1_unpack n)
  let tr = queryTree_rt qt
  putStrLn (unlines ["::GROUP QUERY TREE::",T.drawTree (fmap query_node_pp tr)])

-- * Node

-- > node_query 1
node_query :: Int -> IO ()
node_query n = do
  r <- withSc3 (withNotifications (n_query1_unpack_plain n))
  case r of
    [] -> error "node_query"
    _ -> let tbl = zip (map (\(_,nm,_) -> nm) n_info_fields) (map show r)
         in putStrLn (unlines (kv_table_pp tbl))

-- * Wait for

wait_for :: IO ()
wait_for = do
  let w = pauseThread (0.25::Double)
      f = withSc3_ (sendMessage (c_get [0]) >> waitReply "/c_set")
      g e = print ("wait_for: retry",e::IOError) >> w >> h
      h = catch f g
  putStrLn "wait_for: begin" >> h >> putStrLn "wait_for: end"

-- * Scsyndef

{-
import qualified Sound.Sc3.Server.Graphdef as Graphdef {- hsc3 -}

-- > let sy = "/home/rohan/sw/hsc3-graphs/scsyndef/why-supercollider-rand.sc.scsyndef"
-- > scsyndef_stat sy "/dev/stdout"
scsyndef_stat :: FilePath -> FilePath -> IO ()
scsyndef_stat sy_nm st_nm = do
  str <- Graphdef.scsyndef_stat sy_nm
  writeFile st_nm str
-}

-- > scsyndef_ug_stat sy "/dev/stdout"
scsyndef_ug_stat :: FilePath -> FilePath -> IO ()
scsyndef_ug_stat sy_nm st_nm = do
  str <- Graphdef.Read.scsyndef_ug_stat sy_nm
  writeFile st_nm str

-- > Just sy <- UI.ui_choose_file "/home/rohan/sw/hsc3-graphs/db/"
-- > scsyndef_to_hs sy "/dev/stdout"
scsyndef_to_hs :: FilePath -> FilePath -> IO ()
scsyndef_to_hs sy_nm hs_nm = do
  gr <- Graphdef.Io.read_graphdef_file sy_nm
  let nm = dropExtension (takeFileName sy_nm) -- ascii_to_string (R.graphdef_name gr)
      (_,gr') = Graphdef.Read.graphdef_to_graph gr
      hs = Reconstruct.reconstruct_graph_module nm gr'
  writeFile hs_nm (unlines hs)

-- > UI.ui_choose_file "/home/rohan/sw/hsc3-graphs/db/" >>= maybe (return ()) scsyndef_play
scsyndef_play :: FilePath -> IO ()
scsyndef_play sy_nm = do
  gr <- Graphdef.Io.read_graphdef_file sy_nm
  audition gr

-- > UI.ui_choose_file "/home/rohan/sw/hsc3-graphs/db/" >>= maybe (return ()) (scsyndef_print True)
scsyndef_print :: Bool -> FilePath -> IO ()
scsyndef_print with_com sy_nm = do
  gr <- Graphdef.Io.read_graphdef_file sy_nm
  putStrLn (Graphdef.Text.print_graphdef with_com gr)

scsyndef_read :: FilePath -> FilePath -> IO ()
scsyndef_read txt_fn sy_fn = do
  txt <- readFile txt_fn
  Graphdef.Binary.graphdefWrite sy_fn (Graphdef.Text.read_graphdef txt)

-- > UI.ui_choose_file "/home/rohan/sw/hsc3-graphs/db/" >>= maybe (return ()) scsyndef_dump_ugens
scsyndef_dump_ugens :: FilePath -> IO ()
scsyndef_dump_ugens sy_nm = do
  gr <- Graphdef.Io.read_graphdef_file sy_nm
  Graphdef.graphdef_dump_ugens gr

-- * Status

message_print :: String -> IO ()
message_print addr =
    let pr = waitReply addr >>= \r -> liftIO (putStrLn (showMessage (Just 4) r))
    in withSc3 (async_ (notify True) >> forever pr)

status_monitor :: (DuplexOsc m,MonadIO m) => Double -> m ()
status_monitor dly = do
  str <- server_status_concise
  liftIO (hPutStr stdout ('\r' : str) >> hFlush stdout)
  pauseThread dly

-- * Main

help :: [String]
help =
  "hsc3-scynth command [arguments]" : map (' ' :)
    ["buffer query id:int"
    ,"buffer store id:int au-file:string"
    ,"buffer store-seq id:int dt:float iso|ntpi dir:string"
    ,"buffer free-range b0:int bN:int"
    ,"clear-all"
    ,"dump-osc mode:int (0=none,1=text,2=hex,3=all)"
    ,"group query-tree id:int"
    ,"node query id:int"
    ,"reset"
    ,"scsyndef dump-ugens scyndef-file"
    ,"scsyndef play scyndef-file"
    ,"scsyndef print with-comments:bool scyndef-file"
    ,"scsyndef read text-file scyndef-file"
    ,"scsyndef stat [scyndef-file] [stat-file]"
    ,"scsyndef to-hs [scyndef-file] [hs-file]"
    ,"status print|monitor [delay:float]"
    ,"message print address"
    ,"nrt audition file-name:string"
    ,"nrt stat file-name:string"
    ,"wait-for"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["buffer","free-range",b0,bN] -> buffer_free_range (read b0) (read bN)
    ["buffer","query",n] -> buffer_query (read n)
    ["buffer","store",n,fn] -> buffer_store (read n) fn
    ["buffer","store-seq",n,dt,ts,dir] -> buffer_store_seq (read n) (read dt) (ts == "iso") dir
    ["clear-all"] -> clear_all
    ["dump-osc",md] -> dump_osc (read md)
    ["group","query-tree",n] -> group_query_tree (read n)
    ["node","query",n] -> node_query (read n)
    ["reset"] -> withSc3 reset
    ["scsyndef","dump-ugens",sy] -> scsyndef_dump_ugens sy
    ["scsyndef","play",sy] -> scsyndef_play sy
    ["scsyndef","print",with_com,sy] -> scsyndef_print (read with_com) sy
    ["scsyndef","read",txt_fn,sy_fn] -> scsyndef_read txt_fn sy_fn
    ["scsyndef","stat"] -> scsyndef_ug_stat "/dev/stdin" "/dev/stdout"
    ["scsyndef","stat",sy] -> scsyndef_ug_stat sy "/dev/stdout"
    ["scsyndef","stat",sy,st] -> scsyndef_ug_stat sy st
    ["scsyndef","to-hs"] -> scsyndef_to_hs "/dev/stdin" "/dev/stdout"
    ["scsyndef","to-hs",sy] -> scsyndef_to_hs sy "/dev/stdout"
    ["scsyndef","to-hs",sy,hs] -> scsyndef_to_hs sy hs
    ["status","print"] -> withSc3 serverStatus >>= mapM_ putStrLn
    ["status","monitor",dly] -> withSc3 (forever (status_monitor (read dly)))
    ["message","print",addr] -> message_print addr
    ["nrt","audition",fn] -> readNrt fn >>= nrt_audition
    ["nrt","stat",fn] -> readNrt fn >>= print . Nrt.nrt_stat
    ["wait-for"] -> wait_for
    _ -> putStrLn (unlines help)
