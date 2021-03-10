import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import System.IO {- base -}
import Text.Printf {- base -}

{-
length keys == 5
map length keys == reverse [14,13,12,10,1]
length (concat keys) == 50
map (filter (not . isPrint)) keys
-}
keys :: [[Char]]
keys =
  reverse
  ["`1234567890-=\b"
  ,"\tqwertyuiop[]\\"
  ,"asdfghjkl;'\n"
  ,"zxcvbnm,./"
  ," "
  ]

keys_offsets :: [Int]
keys_offsets = reverse [0,0,1,1,5]

keys_indices :: [[(Int,Int)]]
keys_indices =
  let f (r,k,o) = zip (repeat r) (map (+ o) [0 .. length k - 1])
  in map f (zip3 [0..] keys keys_offsets)

key_size :: (Double,Double)
key_size = (1/15,1/15)

-- > putStrLn $ unwords $ map char_to_sc3_str (concat keys)
char_to_sc3_str :: Char -> String
char_to_sc3_str c =
  case c of
    '\t' -> "$\\t"
    '\n' -> "$\\r"
    '\b' -> "$\\v" -- no backspace...
    '\\' -> "$\\\\"
    _ -> if isPrint c then '$' : c : [] else error "char_to_sc3_str?"

key_coord_sc3 :: (Char,(Double,Double)) -> String
key_coord_sc3 (c,(x,y)) = printf "%s, %.4f @ %.4f" (char_to_sc3_str c) x y

key_coord_seq_sc3 :: [(Char,(Double,Double))] -> String
key_coord_seq_sc3 = printf "Dictionary.newFrom([%s])" . intercalate "," . map key_coord_sc3

key_mnn_seq_sc3 :: [(Char,Double)] -> String
key_mnn_seq_sc3 =
  let f (c,mnn) = printf "%s, %.4f" (char_to_sc3_str c) mnn
  in printf "Dictionary.newFrom([%s])" . intercalate "," . map f

{- | (i,j) = row,column indices to (x,y) cartesian co-ordinates in (0,1)

h = zip (concat keys) (map key_index_to_coord (concat keys_indices))
putStrLn $ key_coord_seq_sc3 h
putStrLn $ key_mnn_seq_sc3 (zip (concat keys) [48..])
-}
key_index_to_coord :: (Int,Int) -> (Double,Double)
key_index_to_coord (i,j) =
  let i_y = 1 / 5
      j_x = 1 / 15
  in ((j_x / 2) + (fromIntegral j * j_x)
     ,(i_y / 2) + (fromIntegral i * i_y))

echo_step = do
  c <- hGetChar stdin
  hPutChar stdout c

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forever echo_step
