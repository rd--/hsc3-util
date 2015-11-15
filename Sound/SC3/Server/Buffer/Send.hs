module Sound.SC3.Server.Buffer.Send where

import Data.Hashable {- hashable -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

import qualified Sound.File.NeXT as F {- hsc3-sf -}
import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}

-- | Message to send data to scsynth via temporary audio file.
b_tmp_allocRead :: (Hashable n,Floating n,Real n) => Int -> [n] -> IO Message
b_tmp_allocRead nid d = do
  tmp <- getTemporaryDirectory
  let nc = 1
      sr = 1
      h = F.Header (length d) F.Float sr nc
      nm = tmp </> show (hash d) <.> "au"
  F.au_write nm h [d]
  return (b_allocRead nid nm 0 0)
