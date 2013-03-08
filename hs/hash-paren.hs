import Data.List {- base -}
import Data.List.Split {- split -}
import Text.Regex {- regex-compat -}
import System.Environment {- base -}

-- | Does /s/ have a /hash paren/ expression.
--
-- > has_hash_paren "  a <- f #(b) #(c)" == True
has_hash_paren :: String -> Bool
has_hash_paren = isInfixOf " #("

-- | Variant of 'splitOn' requiring one match only and returning splitter.
--
-- > split_on_1 " <- " "  a <- f #(b) #(c)"
-- > split_on_1 " do " "  let a = do f #(b) #(c)"
split_on_1 :: Eq a => [a] -> [a] -> Maybe ([a],[a],[a])
split_on_1 p q =
    case splitOn p q of
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

-- | Regular expression to match a /hash paren/ expression.
--
-- > matchRegexAll hp_regex "#(a)" == Just ("","#(a)","",["a"])
-- > matchRegexAll hp_regex "#(a) #(b)" == Just ("","#(a)"," #(b)",["a"])
-- > matchRegexAll hp_regex "return (#(a))" == Just ("return (","#(a)",")",["a"])
hp_regex :: Regex
hp_regex = mkRegex "#\\(([^#()]*)\\)"

-- | Run 'hp_regex' matcher.
--
-- > hp_match "  a <- f #(b) #(c)" == Just ("  a <- f ","b"," #(c)")
hp_match :: String -> Maybe (String,String,String)
hp_match s =
    case matchRegexAll hp_regex s of
      Just (pre,_,post,[ele]) -> Just (pre,ele,post)
      _ -> Nothing

-- | Return indentation of line.
--
-- > indent_of "  a <- b" == "  "
indent_of :: String -> String
indent_of = takeWhile (== ' ')

type Binding = (String,String)
type Name_Supply = [String]

-- | Process one line of /hash-paren/ re-writes.
--
-- > let r = ([("_hp_0","b"),("_hp_1","c")],"  a <- f _hp_0 _hp_1")
-- > in snd (hp_analyse hp_names [] "  a <- f #(b) #(c)") == r
--
-- > let r = ([("_hp_0","a")],"  return (f _hp_0)")
-- > in snd (hp_analyse hp_names [] "  return (f #(a))") == r
hp_analyse :: Name_Supply -> [Binding] -> String -> (Name_Supply, ([Binding], String))
hp_analyse n r s =
    case hp_match s of
      Just (pre,ele,post) ->
          let nm:n' = n
              r' = (nm,ele) : r
          in hp_analyse n' r' (concat [pre,nm,post])
      Nothing -> (n,(reverse r,s))

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
    then let (n',r) = hp_analyse n [] s
             r' = hp_build r
         in (n',r')
    else (n,[s])

-- | Run /hash-paren/ rewriter.
--
-- > hp_rewrite ["main = do"
-- >            ,"  let a = f #(b) #(c)"
-- >            ,"  d <- e"
-- >            ,"  p <- g #(q r) #(s t)"]
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
