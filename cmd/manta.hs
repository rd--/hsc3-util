{- | Manta

Translate MantaOsc messages to Sc3 c_setn messages (c.f. Voicer)

<https://github.com/ssfrr/libmanta/tree/master/MantaOSC>

Implements voicer algorithm.

-}

import Control.Monad {- base -}
import Data.IORef {- base -}
import Data.Int {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Sound.Osc as Osc {- hosc -}
import qualified Sound.Osc.Fd as Osc.Fd {- hosc -}

import qualified Sound.Midi.VoiceList as VoiceList {- midi-osc -}

import qualified Sound.Sc3.Server.Command.Plain as Sc3 {- hsc3 -}

{- | Given key number (0,47) return (row,column).
Row is in (0,5) where zero is the lowermost row.
Column is in (0,7) where zero is the leftmost column.

>>> map mantaKeyToRc [0, 8, 17, 25, 34, 42]
[(0,0),(1,0),(2,1),(3,1),(4,2),(5,2)]

>>> map mantaKeyToRc [44, 37, 29, 22, 14, 7]
[(5,4),(4,5),(3,5),(2,6),(1,6),(0,7)]
-}
mantaKeyToRc :: Integral t => t -> (t, t)
mantaKeyToRc index =
  let row = index `div` 8
      column = index `mod` 8
  in (row, column)

{- | Given key number (0,47) return (x,y) in ((0,0),(1,1)).

>>> map (snd . mantaKeyToXy) [0, 8, 17, 25, 34, 42]
[0.0,0.2,0.4,0.6,0.8,1.0]

>>> map (round . (* 100) . fst . mantaKeyToXy) [0, 8, 17, 25, 34, 42]
[0,7,13,20,27,33]

>>> map (snd . mantaKeyToXy) [44, 37, 29, 22, 14, 7]
[1.0,0.8,0.6,0.4,0.2,0.0]

>>> map (round . (* 100) . fst . mantaKeyToXy) [44, 37, 29, 22, 14, 7]
[60,67,73,80,87,93]
-}
mantaKeyToXy :: (Integral i, Fractional r) => i -> (r, r)
mantaKeyToXy index =
  let (row, column) = mantaKeyToRc index
      offset = if even row then 0.0 else 0.5
  in ((fromIntegral column + offset) / 7.5
     ,fromIntegral row / 5.0)

-- | There are 48 keys (Index=0-47,Value=0-210)
type Key = (Int,Int)

-- | There are 2 "sliders" (Index=0-1,Value=0-4096), store value for each.
type Sliders = (Int,Int)

-- | Map from key index to relative midi note number.
type KeyMap = [(Int,Int)]

-- | (noteOffset, busZero, busIncrement)
type MessageOpt = (Double, Int, Int)

{- | c_setn message. -}
setMessage :: MessageOpt -> KeyMap -> Sliders -> Int -> Key -> Osc.Fd.Message
setMessage messageOpt keyMap (i, j) v (index, value) =
  let (noteOffset, busZero, busIncrement) = messageOpt
      w = 1
      (x,y) = mantaKeyToXy index
      z = fromIntegral value / 210.0
      p = (noteOffset + fromIntegral (List.lookup_err index keyMap)) / 100.0
  in Sc3.c_setn1
     (busZero + (v * busIncrement)
     ,[w, x, y, z, fromIntegral i / 4096, fromIntegral j / 4096, 0, p])

keyOf :: Int32 -> Int32 -> Key
keyOf index value = (fromIntegral index, fromIntegral value)

-- | (Message-Options, Key-Map, Sliders, Voice-List)
type MantaParam = (MessageOpt, KeyMap, Sliders, VoiceList.VoiceList)

translateMessage :: MantaParam -> Osc.Message -> Maybe (VoiceList.VoiceList, Osc.Message)
translateMessage (messageOpt, keyMap, sliders, voiceList) message =
  let (_, busZero, busIncrement) = messageOpt
  in case message of
       Osc.Message "/manta/velocity/pad" [Osc.Int32 index, Osc.Int32 0]
         -> case VoiceList.freeId voiceList (fromIntegral index) of
              Just (v,voiceList') ->
                Just (voiceList', Sc3.c_setn1 (busZero + (v * busIncrement), [0]))
              Nothing -> Nothing
       Osc.Message "/manta/velocity/pad" [Osc.Int32 index, Osc.Int32 value]
         -> case VoiceList.allocId voiceList (fromIntegral index) of
              Just (v, voiceList') ->
                Just (voiceList'
                     ,setMessage messageOpt keyMap sliders v (keyOf index value))
              Nothing -> Nothing
       Osc.Message "/manta/continuous/pad" [Osc.Int32 index, Osc.Int32 value]
         -> case VoiceList.readId voiceList (fromIntegral index) of
              Just v ->
                Just (voiceList
                     ,setMessage messageOpt keyMap sliders v (keyOf index value))
              Nothing -> Nothing
       _ -> Nothing

updateSliders :: Sliders -> Osc.Message -> Sliders
updateSliders (i,j) message =
  case message of
    Osc.Message "/manta/continuous/slider" [Osc.Int32 index, Osc.Int32 value] ->
      if value == 65535
      then (i,j)
      else case index of
             0 -> (fromIntegral value,j)
             1 -> (i,fromIntegral value)
             _ -> error "updateSliders?"
    _ -> (i,j)

recvMessage :: Osc.Fd.Udp -> IO Osc.Fd.Message
recvMessage udpFd = do
  packet <- Osc.Fd.udp_recv_packet udpFd
  case packet of
    Osc.Packet_Bundle _ -> error "recvMessage?"
    Osc.Packet_Message message -> return message

sendMessage :: Osc.Fd.Tcp -> Osc.Fd.Message -> IO ()
sendMessage tcpFd message = Osc.Fd.tcp_send_packet tcpFd (Osc.Packet_Message message)

type MantaState = (IORef Sliders, IORef VoiceList.VoiceList)

processPacket :: KeyMap -> MantaState -> Osc.Fd.Udp -> Osc.Fd.Tcp -> IO ()
processPacket keyMap (slidersRef, voiceListRef) mantaFd sc3Fd = do
  message <- recvMessage mantaFd
  voiceList <- readIORef voiceListRef
  sliders <- readIORef slidersRef
  writeIORef slidersRef (updateSliders sliders message)
  let messageOpt = (36, 13000, 10)
  case translateMessage (messageOpt, keyMap, sliders, voiceList) message of
    Just (voiceList', answer) -> do
      sendMessage sc3Fd answer
      writeIORef voiceListRef voiceList'
    Nothing -> return ()

translationServer :: Int -> Int -> IO b
translationServer mantaPort scsynthPort = do
  sc3Fd <- Osc.Fd.openTcp "127.0.0.1" scsynthPort
  keyMap <- loadKeyMap "/home/rohan/opt/src/ssfrr/libmanta/Wicki-Hayden.map" -- HarmonicTable
  voiceListRef <- newIORef (replicate 16 Nothing)
  slidersRef <- newIORef (2000, 2000)
  let fn mantaFd = forever (processPacket keyMap (slidersRef, voiceListRef) mantaFd sc3Fd)
  Osc.Fd.withTransport (Osc.Fd.udp_server mantaPort) fn

{- | Load key map

> loadKeyMap "/home/rohan/opt/src/ssfrr/libmanta/Wicki-Hayden.map"
> loadKeyMap "/home/rohan/opt/src/ssfrr/libmanta/HarmonicTable.map"
-}
loadKeyMap :: FilePath -> IO KeyMap
loadKeyMap fileName = do
  text <- readFile fileName
  let isEntry str = (length str > 0) && (List.head_err str /= '#')
      entries = map words (filter isEntry (lines text))
      parseEntry ent =
        case ent of
          [key,value] -> (read key,read value)
          _ -> error "loadKeyMap"
  return (map parseEntry entries)

main :: IO ()
main = do
  let mantaPort = 31416
      sc3Port = 57110
  translationServer mantaPort sc3Port

{-

MantaOsc Messages:

/manta/(continuous|velocity)/button index=(0,3) value=(0,210)
/manta/(continuous|velocity)/pad index=(0,47) value=(0,210)
/manta/continuous/slider index=(0,1) value=(0,4096)

-}
