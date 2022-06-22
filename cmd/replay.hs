import Data.List {- base -}

import qualified Text.CSV.Lazy.String as CSV {- lazy-csv -}

import Sound.Osc {- hosc -}

import Sound.Sc3 {- hsc3 -}
import Sound.Sc3.Common.Base {- hsc3 -}

-- | (time,voice,event)
type EventTrace = (Double,Int,Event Double)

-- | Load CSV trace file
ccEventTrace_load :: FilePath -> IO [EventTrace]
ccEventTrace_load fn = do
  txt <- readFile fn
  let tbl = CSV.csvTable (CSV.parseDSV False ',' txt)
  return (map (\(tm:ix:ev) -> (read tm,read ix,ccEvent_from_list (map read ev))) (CSV.fromCSVTable tbl))

-- | Get delta-time sequence of trace
ccEventTrace_dt :: [EventTrace] -> [Double]
ccEventTrace_dt tr = let tm = map (\(t,_,_) -> t) tr in d_dx tm

-- | Pause for delta time, then send trace event
ccEventTrace_send :: (MonadIO m,SendOsc m) => (Double,EventTrace) -> m ()
ccEventTrace_send (dt,(_,k,(w,x,y,z,o,rx,ry,p,px))) = do
  let msg = c_setn1 (13000 + (k * 10),[w,x,y,z,o,rx,ry,p,px])
  pauseThread dt
  sendMessage msg

-- | Run ccEventTrace_send over trace
ccEventTrace_replay :: [EventTrace] -> IO ()
ccEventTrace_replay tr = withSc3 (mapM_ ccEventTrace_send (zip (ccEventTrace_dt tr) tr))

-- | Select w=0 events
ccEventTrace_is_end :: EventTrace -> Bool
ccEventTrace_is_end (_,_,(w,_,_,_,_,_,_,_,_)) = not (w > 0)

-- | Re-assign k in trace using a strict lowest voice number not used algorithm.
ccEventTrace_reassign :: [EventTrace] -> [EventTrace]
ccEventTrace_reassign tr =
  {-
  rw :: [(Int,Int)] ; store which k are being written using a [(k,k')] set
  rw_get_k ; lookup where k is re-writing to, or nothing if k is an onset (ie. not in rw)
  rw_next_k ; find lowest k not being written to
  rw_delete_k ; delete k at w=0 event
  -}
  let rw_get_k rw i = fmap snd (find ((== i) . fst) rw)
      rw_next_k rw = head (filter (\i -> i `notElem` (map snd rw)) [0..16])
      rw_delete_k rw i = filter (not . (== i) . fst) rw
      f rw (tm,k,e) =
        let (w,_,_,_,_,_,_,_,_,_) = e
        in case rw_get_k rw k of
             Nothing -> let k' = rw_next_k rw in (((k,k') : rw),(tm,k',e))
             Just k' -> (if w > 0 then rw else rw_delete_k rw k,(tm,k',e))
  in snd (mapAccumL f [] tr)

{-
tr <- ccEventTrace_load "/home/rohan/sw/hsc3-util/cmd/trace.csv"
tr' = ccEventTrace_reassign tr
ccEventTrace_replay tr'
-}
