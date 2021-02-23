import qualified Text.CSV.Lazy.String as CSV {- lazy-csv -}

import Sound.OSC {- hosc -}

import Sound.SC3 {- hsc3 -}
import Sound.SC3.Common.Base {- hsc3 -}

-- > tr <- trace_load "/tmp/trace.csv"
-- > trace_replay tr
trace_load :: FilePath -> IO [[Double]]
trace_load fn = do
  txt <- readFile fn
  let tbl = CSV.csvTable (CSV.parseDSV False ',' txt)
  return (map (map read) (CSV.fromCSVTable tbl))

trace_dt :: Num t => [[t]] -> [t]
trace_dt tr = let tm = map (!! 0) tr in d_dx tm

entry_send :: (MonadIO m,SendOSC m) => (Double,[Double]) -> m ()
entry_send (dt,e) = do
  case e of
    [_tm,k,w,x,y,z,o,rx,ry,p,px] -> do
      let msg = c_setn1 (13000 + (round k * 10),[w,x,y,z,o,rx,ry,p,px])
      pauseThread dt
      sendMessage msg
    _ -> error "entry_send"

trace_replay :: [[Double]] -> IO ()
trace_replay tr = do
  let dt = trace_dt tr
  withSC3 (mapM_ entry_send (zip dt tr))
