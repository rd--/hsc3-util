import Control.Monad
import Data.Char {- base -}
import Data.List {- base -}
import qualified Data.List.Split as S {- split -}
import Text.ParserCombinators.Poly.State {- polyparse -}
import System.Environment {- base -}

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

type HP = Parser Int Char

hp_next :: HP Char
hp_next = do
  c <- next
  when (c == '(') (stUpdate succ)
  when (c == ')') (stUpdate pred)
  return c

hp_open :: HP Char
hp_open = do
  c <- hp_next
  n <- stGet
  if (c == '(' && n == 1) then return c else fail "hp_open"

hp_close :: HP Char
hp_close = do
  c <- hp_next
  n <- stGet
  if (c == ')' && n == 0) then return c else fail "hp_close"

-- > runParser hp_balance_paren 0 "(a (b) (c (d)))"
-- > runParser hp_balance_paren 0 "(a (b)) c (d)"
hp_balance_paren :: HP String
hp_balance_paren = hp_open *> manyFinally' hp_next hp_close

-- > runParser hp_hash_paren 0 "#(a (b) (c (d)))"
-- > runParser hp_hash_paren 0 "#a"
-- > runParser hp_hash_paren 0 "a"
hp_hash_paren :: HP String
hp_hash_paren = satisfy (== '#') *> hp_balance_paren

-- > runParser hp_parser 0 "c <- f #(a)"
hp_parser :: HP [Either Char String]
hp_parser = many1 (oneOf [fmap Right hp_hash_paren,fmap Left next])


type Binding = (String,String)
type Name_Supply = [String]

hp_gen_bindings :: Name_Supply -> [Either Char String] -> (Name_Supply,[Binding])
hp_gen_bindings nm =
    let rec n r l =
            case l of
              [] -> (n,reverse r)
              Left _:l' -> rec n r l'
              Right p:l' -> let n0:n' = n
                            in rec n' ((n0,p) : r) l'
    in rec nm []

hp_gen_rewrite :: [Binding] -> [Either Char String] -> String
hp_gen_rewrite bnd =
    let rec b s r =
            case r of
              [] -> reverse s
              Left c:r' -> rec b (c : s) r'
              Right _:r' -> let (n,_):b' = b
                            in rec b' (reverse n ++ s) r'
    in rec bnd []

-- | Process one line of /hash-paren/ re-writes.
--
-- > let r = ([("_hp_0","b"),("_hp_1","c (d e)")],"  a <- f _hp_0 _hp_1")
-- > in snd (hp_analyse hp_names "  a <- f #(b) #(c (d e))") == r
--
-- > let r = ([("_hp_0","a")],"  return (f _hp_0)")
-- > in snd (hp_analyse hp_names "  return (f #(a))") == r
hp_analyse :: Name_Supply -> String -> (Name_Supply,([Binding],String))
hp_analyse nm s =
    case runParser hp_parser 0 s of
      (Right r,0,[]) -> let (nm',b) = hp_gen_bindings nm r
                        in (nm',(b,hp_gen_rewrite b r))
      _ -> error "hp_analyse"

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
-- >            ,"  p <- g #(q r) #(s (t u))"
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
