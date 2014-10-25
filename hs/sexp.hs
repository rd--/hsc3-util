import Sound.SC3.RW.SExp {- hsc3-rw -}
import System.Environment {- base -}

main :: IO ()
main = do
  a <- getArgs
  case a of
    [i_fn] -> hs_to_lisp_io i_fn "/dev/stdout"
    [i_fn,o_fn] -> hs_to_lisp_io i_fn o_fn
    _ -> error "input-file [output-file=/dev/stdout]"
