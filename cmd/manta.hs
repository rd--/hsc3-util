{-

Translate MantaOsc messages to Sc3 Voicer

/manta/velocity/button index=(0,3) value=(0,210)
/manta/continuous/button index=(0,3) value=(0,210)
/manta/velocity/pad index=(0,47) value=(0,210)
/manta/continuous/pad index=(0,47) value=(0,210)
/manta/continuous/slider index=(0,1) value=(0,4000)

Implement voicer algorithm, or assign key index.

-}

import Control.Monad {- base -}
import Data.IORef {- base -}
import Data.List {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Sound.Osc as Osc {- hosc -}
import qualified Sound.Osc.Fd as Osc.Fd {- hosc -}

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

translatePacket :: [Int] -> Osc.Packet -> Maybe ([Int], Osc.Packet)
translatePacket list packet =
  case packet of
    Osc.Packet_Bundle _ -> error "translatePacket?"
    Osc.Packet_Message message ->
      case translateMessage list message of
        Just (list', message') -> Just (list', Osc.Packet_Message message')
        Nothing -> Nothing

{- | Store voices as list of numberOfVoices places.

>>> setEntry(replicate 8 (-1)) 5 7
[-1,-1,-1,-1,-1,7,-1,-1]

>>> setEntry(replicate 8 (-1)) 7 5
[-1,-1,-1,-1,-1,-1,-1,5]
-}
setEntry :: [Int] -> Int -> Int -> [Int]
setEntry list index key =
  let f index' value =
        if index == index'
        then key
        else value
  in zipWith f [0.. ] list

{- | Allocate Id

>>> allocId [4,-1,-1,-1] 7
Just (1,[4,7,-1,-1])
-}
allocId :: [Int] -> Int -> Maybe (Int, [Int])
allocId list key =
  case findIndex (== -1) list of
    Just index -> Just (index, setEntry list index key)
    Nothing -> Nothing

{- | Read Id

>>> readId [4,7,-1,-1] 7
Just (1,[4,7,-1,-1])
-}
readId :: [Int] -> Int -> Maybe (Int, [Int])
readId list key =
  case findIndex (== key) list of
    Just index -> Just (index, list)
    Nothing -> Nothing

{- | Free Id

>>> freeId [4,7,-1,-1] 7
[4,-1,-1,-1]
-}
freeId :: [Int] -> Int -> (Int, [Int])
freeId list key =
  case findIndex (== key) list of
    Just index -> (index, setEntry list index (-1))
    Nothing -> error "freeId"

{- | c_setn message. -}
setMessage :: Integral t => Int -> t -> t -> Osc.Fd.Message
setMessage v index value =
  let w = 1
      (x,y) = mantaKeyToXy index
      z = fromIntegral value / 210.0
      p = (36 + fromIntegral (List.lookup_err index wickiHayden)) / 100.0
  in Sc3.c_setn1 (13000 + (v * 10), [w, x, y, z, 0, 0, 0, p])

translateMessage :: [Int] -> Osc.Message -> Maybe ([Int], Osc.Message)
translateMessage list message =
  case message of
    Osc.Message "/manta/velocity/pad" [Osc.Int32 index, Osc.Int32 0]
      -> let (v,list') = freeId list (fromIntegral index)
         in Just (list', Sc3.c_setn1 (13000 + (v * 10), [0]))
    Osc.Message "/manta/velocity/pad" [Osc.Int32 index, Osc.Int32 value]
      -> case allocId list (fromIntegral index) of
           Just (v, list') -> Just (list', setMessage v index value)
           Nothing -> Nothing
    Osc.Message "/manta/continuous/pad" [Osc.Int32 index, Osc.Int32 value]
      -> case readId list (fromIntegral index) of
           Just (v, list') -> Just (list', setMessage v index value)
           Nothing -> Nothing
    _ -> Nothing

processPacket :: IORef [Int] -> Osc.Fd.Udp -> Osc.Fd.Tcp -> IO ()
processPacket listRef mantaFd sc3Fd = do
  packet <- Osc.Fd.udp_recv_packet mantaFd
  list <- readIORef listRef
  -- putStrLn (show (list, packet))
  case translatePacket list packet of
    Just (list', answer) -> Osc.Fd.tcp_send_packet sc3Fd answer >> writeIORef listRef list'
    Nothing -> return ()

translationServer :: Int -> Int -> IO b
translationServer mantaPort scsynthPort = do
  sc3Fd <- Osc.Fd.openTcp "127.0.0.1" scsynthPort
  listRef <- newIORef (replicate 16 (-1))
  let fn mantaFd = forever (processPacket listRef mantaFd sc3Fd)
  Osc.Fd.withTransport (Osc.Fd.udp_server mantaPort) fn

main :: IO ()
main = do
  let mantaPort = 31416
      sc3Port = 57110
  translationServer mantaPort sc3Port

wickiHayden :: Num t => [(t,t)]
wickiHayden =
  [(0,0)
  ,(1,2)
  ,(2,4)
  ,(3,6)
  ,(4,8)
  ,(5,10)
  ,(6,12)
  ,(7,14)
  ,(8,7)
  ,(9,9)
  ,(10,11)
  ,(11,13)
  ,(12,15)
  ,(13,17)
  ,(14,19)
  ,(15,21)
  ,(16,12)
  ,(17,14)
  ,(18,16)
  ,(19,18)
  ,(20,20)
  ,(21,22)
  ,(22,24)
  ,(23,26)
  ,(24,19)
  ,(25,21)
  ,(26,23)
  ,(27,25)
  ,(28,27)
  ,(29,29)
  ,(30,31)
  ,(31,33)
  ,(32,24)
  ,(33,26)
  ,(34,28)
  ,(35,30)
  ,(36,32)
  ,(37,34)
  ,(38,36)
  ,(39,38)
  ,(40,31)
  ,(41,33)
  ,(42,35)
  ,(43,37)
  ,(44,39)
  ,(45,41)
  ,(46,43)
  ,(47,45)]
