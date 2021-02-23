import Data.List {- base -}

import qualified Text.CSV.Lazy.String as CSV {- lazy-csv -}

import Sound.OSC {- hosc -}

import Sound.SC3 {- hsc3 -}
import Sound.SC3.Common.Base {- hsc3 -}

type REventTrace = (Double,Int,REvent Double)

-- | Load CSV trace file
rEventTrace_load :: FilePath -> IO [REventTrace]
rEventTrace_load fn = do
  txt <- readFile fn
  let tbl = CSV.csvTable (CSV.parseDSV False ',' txt)
  return (map (\(tm:ix:ev) -> (read tm,read ix,rEvent_from_list (map read ev))) (CSV.fromCSVTable tbl))

-- | Get delta-time sequence of trace
rEventTrace_dt :: [REventTrace] -> [Double]
rEventTrace_dt tr = let tm = map (\(t,_,_) -> t) tr in d_dx tm

-- | Pause for delta time, then send trace event
rEventTrace_send :: (MonadIO m,SendOSC m) => (Double,REventTrace) -> m ()
rEventTrace_send (dt,(_,k,(w,x,y,z,o,rx,ry,p,px,_))) = do
  let msg = c_setn1 (13000 + (k * 10),[w,x,y,z,o,rx,ry,p,px])
  pauseThread dt
  sendMessage msg

-- | Run rEventTrace_send over trace
rEventTrace_replay :: [REventTrace] -> IO ()
rEventTrace_replay tr = withSC3 (mapM_ rEventTrace_send (zip (rEventTrace_dt tr) tr))

-- | Select w=0 events
rEventTrace_is_end :: REventTrace -> Bool
rEventTrace_is_end (_,_,(w,_,_,_,_,_,_,_,_,_)) = not (w > 0)

-- | Re-assign k in trace using a strict lowest voice number not used algorithm.
rEventTrace_reassign :: [REventTrace] -> [REventTrace]
rEventTrace_reassign tr =
  -- rw tracks which k are being written using a [(k,k')] set
  let rw_get_k rw i = fmap snd (find ((== i) . fst) rw) -- lookup where k is re-writing to, nothing if k is onset
      rw_next_k rw = head (filter (\i -> i `notElem` (map snd rw)) [0..16]) -- find lowest k not being written to
      rw_delete_k rw i = filter (not . (== i) . fst) rw -- delete k at w=0 event
      f rw (tm,k,e) =
        let (w,_,_,_,_,_,_,_,_,_) = e
        in case rw_get_k rw k of
             Nothing -> let k' = rw_next_k rw in (((k,k') : rw),(tm,k',e))
             Just k' -> (if w > 0 then rw else rw_delete_k rw k,(tm,k',e))
  in snd (mapAccumL f [] tr)

{-
tr <- rEventTrace_load "/home/rohan/sw/hsc3-util/cmd/trace.csv"
tr' = rEventTrace_reassign tr
rEventTrace_replay tr'
-}
