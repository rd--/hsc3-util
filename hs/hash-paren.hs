import Data.List {- base -}
import Text.Regex {- regex-compat -}
import System.Environment {- base -}

-- | Name supply for introduced variables.
--
-- > hp_names !! 9 == "_hp_9"
hp_names :: [String]
hp_names = map (\n -> "_hp_" ++ show n) [0::Integer ..]

-- | Regular expression to match a /hash paren/ expression.
--
-- > matchRegexAll hp_regex "#(a)" == Just ("","#(a)","",["a"])
-- > matchRegexAll hp_regex "#(a) #(b)"
-- > matchRegexAll hp_regex "return (#(a))"
hp_regex :: Regex
hp_regex = mkRegex "#\\(([^#()]*)\\)"

-- | Run 'hp_regex' matcher.
--
-- > hp_match "  a <- f #(b) #(c)" == Just ("  a <- f ","b"," #(c)")
hp_match :: String -> Maybe (String, String, String)
hp_match s =
    case matchRegexAll hp_regex s of
      Just (pre,_,post,[ele]) -> Just (pre,ele,post)
      _ -> Nothing

-- | Return indentation of line.
--
-- > indent_of "  a <- b" == "  "
indent_of :: String -> String
indent_of = takeWhile (== ' ')

-- | Process one line of /hash-paren/ re-writes.
--
-- > let r = ["  _hp_0 <- b","  _hp_1 <- c","  a <- f _hp_0 _hp_1"]
-- > in snd (hp_line hp_names [] "  a <- f #(b) #(c)") == r
--
-- > snd (hp_line hp_names [] "  return (f #(a))")
hp_line :: [String] -> [String] -> String -> ([String], [String])
hp_line n r s =
    case hp_match s of
      Just (pre,ele,post) ->
          let i = indent_of s
              nm:n' = n
              r' = concat [i,nm," <- ",ele] : r
          in hp_line n' r' (concat [pre,nm,post])
      Nothing -> (n,reverse r ++ [s])

-- | Run /hash-paren/ rewriter.
--
-- > hp_rewrite ["main = do"
-- >            ,"  a <- f #(b) #(c)"
-- >            ,"  d <- e"
-- >            ,"  p <- g #(q r) #(s t)"]
hp_rewrite :: [String] -> [String]
hp_rewrite =
    let f n s = hp_line n [] s
    in concat . snd . mapAccumL f hp_names

main :: IO ()
main = do
  a <- getArgs
  case a of
    [_,i_fn,o_fn] -> do
           i <- readFile i_fn
           writeFile o_fn (unlines (hp_rewrite (lines i)))
    _ -> error "initial-file input-file output-file"
