import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.List.Split as S {- split -}
import Text.ParserCombinators.Poly.State {- polyparse -}
import System.Environment

-- | Return indentation of line.
--
-- > indent_of "  a <- b" == "  "
indent_of :: String -> String
indent_of = takeWhile isSpace

-- | Variant of 'splitOn' requiring one match only and returning splitter.
--
-- > split_on_1 " <- " "  a <- f #(b) #(c)"
-- > split_on_1 " do " "  let a = do f #(b) #(c)"
split_on_1 :: Eq a => [a] -> [a] -> Maybe ([a],[a],[a])
split_on_1 p q =
    case S.splitOn p q of
      [r,s] -> Just (r,p,s)
      _ -> Nothing

-- | Split inline /do/ line into separate lines.
--
-- > let r = ["  let a = do ","             f #(b) #(c)"]
-- > in hp_remove_inline_do "  let a = do f #(b) #(c)" == r
hp_remove_inline_do :: String -> [String]
hp_remove_inline_do s =
    case split_on_1 " do " s of
      Just (p,q,r) -> let s0 = p ++ q
                          s1 = replicate (length s0) ' ' ++ r
                      in [s0,s1]
      _ -> [s]

-- | Name supply for introduced variables.
--
-- > hp_names !! 9 == "_hp_9"
hp_names :: [String]
hp_names = map (\n -> "_hp_" ++ show n) [0::Integer ..]

-- | Does /s/ have a /hash paren/ expression.
--
-- > has_hash_paren "  a <- f #(b) #(c)" == True
has_hash_paren :: String -> Bool
has_hash_paren = isInfixOf " #("

type ST = (Int,[Int])
type HP = Parser ST Char

hp_st :: ST
hp_st = (0,[])

safe_head :: [a] -> Maybe a
safe_head l =
    case l of
      [] -> Nothing
      e:_ -> Just e

-- | Only count parens in #().
hp_next :: HP (Char,Maybe Int)
hp_next = do
  let stPut st = stUpdate (const st)
  c <- next
  (n,h) <- stGet
  case c of
    '#' -> stPut (n,n : h) >> return (c,Just n)
    '(' -> stPut (if null h then (n,h) else (n + 1,h)) >> return (c,safe_head h)
    ')' -> let n' = n - 1
               (st',e) = case h of
                           [] -> ((n,[]),Nothing)
                           x:h' -> if x == n'
                                   then ((n',h'),Just x)
                                   else ((n',h),safe_head h)
           in stPut st' >> return (c,e)
    _ -> return (c,safe_head h)

type HP_Char = (Char,Maybe Int)
type HP_String = [HP_Char]

-- > runParser hp_hash_paren hp_st "r <- #(a)"
-- > runParser hp_hash_paren hp_st "#(a (b)) (c (d))"
-- > runParser hp_hash_paren hp_st "#(a (b) (c (d)))"
-- > runParser hp_hash_paren hp_st "#a"
-- > runParser hp_hash_paren hp_st "a"
-- > runParser hp_hash_paren hp_st "c <- f #(a) #(b c) d"
-- > runParser hp_hash_paren hp_st "c <- f #(a) #(b #(c)) d"
-- > runParser hp_hash_paren hp_st "c <- f #(a) #(b #(c #(d e) f) #(g)) #(h) i"
hp_hash_paren :: HP HP_String
hp_hash_paren = many1 hp_next

-- > hp_parse "c <- f #(a) #(b #(c #(d e) f) g) h"
hp_parse :: String -> HP_String
hp_parse s =
    case runParser hp_hash_paren hp_st s of
      (Right r,(0,[]),[]) -> r
      _ -> error "hp_parse"

-- | Left biased 'max' variant.
--
-- > maxBy last "cat" "mouse" == "cat"
-- > maxBy last "aa" "za" == "aa"
max_by :: Ord a => (t -> a) -> t -> t -> t
max_by f p q = if f q > f p then q else p

-- > replace_first 1 (-1) [-2,1,0,1] == [-2 .. 1]
replace_first :: Eq a => a -> a -> [a] -> [a]
replace_first p q =
    let rec r l = case l of
                  [] -> reverse r
                  e:l' -> if e == p then reverse (q : r) ++ l' else rec (e : r) l'
    in rec []

type Binding = (String,String)
type Name_Supply = [String]

-- > un_hash_paren "#(a)" == "a"
-- > un_hash_paren "b" == "b"
un_hash_paren :: String -> String
un_hash_paren s =
    let f = reverse . drop 1 . reverse
    in case s of
         '#' : '(' : s' -> f s'
         _ -> s

hp_next_binding :: Name_Supply -> HP_String -> Maybe (Name_Supply,Binding,HP_String)
hp_next_binding n s =
    if null s || all ((== Nothing) . snd) s
    then Nothing
    else let nm:n' = n
             s' = groupBy ((==) `on` snd) s
             e = foldl1 (max_by (fromMaybe (-1) . snd . head)) s'
             x = fromJust (snd (head e)) - 1
             x' = if x >= 0 then Just x else Nothing
             s'' = replace_first e (map (\c -> (c,x')) nm) s'
         in Just (n',(nm,un_hash_paren (map fst e)),concat s'')

-- | Process one line of /hash-paren/ re-writes.
--
-- > let r = ([("_hp_0","b"),("_hp_1","c (d e)")],"  a <- f _hp_0 _hp_1")
-- > in snd (hp_analyse hp_names "  a <- f #(b) #(c (d e))") == r
--
-- > let r = ([("_hp_0","a")],"  return (f _hp_0)")
-- > in snd (hp_analyse hp_names "  return (f #(a))") == r
--
-- > let r = ([("_hp_0","d e"),("_hp_1","c _hp_0 f"),("_hp_2","a"),("_hp_3","b _hp_1 g")]
-- >         ,"c <- f (_hp_2,_hp_3) h")
-- > in snd (hp_analyse hp_names "c <- f (#(a),#(b #(c #(d e) f) g)) h") == r
hp_analyse :: Name_Supply -> String -> (Name_Supply,([Binding],String))
hp_analyse nm =
    let rec n b s = case hp_next_binding n s of
                      Nothing -> (n,(reverse b,map fst s))
                      Just (n',b',s') -> rec n' (b':b) s'
    in rec nm [] . hp_parse

-- | Re-construct 'hp_analyse' output.
hp_build :: ([Binding],String) -> [String]
hp_build (b,e) =
    let f (i,j) = concat [i," <- ",j]
        ind = indent_of e
        b' = map ((ind ++) . f) b
    in b' ++ [e]

-- | Process one line of /hash paren/ file.
hp_line :: Name_Supply -> String -> (Name_Supply, [String])
hp_line n s =
    if has_hash_paren s
    then let (n',r) = hp_analyse n s
             r' = hp_build r
         in (n',r')
    else (n,[s])

-- | Run /hash-paren/ rewriter.
--
-- > hp_rewrite ["main = do"
-- >            ,"  let a = f #(b) #(c)"
-- >            ,"  d <- e"
-- >            ,"  p <- g #(q r) #(s #(t u))"
-- >            ,"  return (h #(v w))"]
hp_rewrite :: [String] -> [String]
hp_rewrite =
    let f n s = hp_line n s
    in concat .
       snd .
       mapAccumL f hp_names .
       concatMap hp_remove_inline_do

-- | Arguments as required by @ghc -F -pgmF hash-paren@.
main :: IO ()
main = do
  a <- getArgs
  case a of
    [_,i_fn,o_fn] -> do
           i <- readFile i_fn
           writeFile o_fn (unlines (hp_rewrite (lines i)))
    _ -> error "initial-file input-file output-file"
