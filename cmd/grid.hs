import Text.Printf {- base -}

import qualified Music.Theory.List as List {- hmt -}

import Music.Theory.Tuning.Scala as Scala {- hmt -}
import Music.Theory.Tuning.Scala.Mode as Mode {- hmt -}

type RANGE n = (n,n)

type XY_RANGE n = (RANGE n,RANGE n)

-- | Row-order grid.  All rows have an equal number of columns.
type GRID t = [[t]]

-- > grid_indices (2,5)
grid_indices :: (Int,Int) -> [(Int,Int)]
grid_indices (nr,nc) = [(r,c) | r <- [0 .. nr - 1], c <- [0 .. nc - 1]]

grid_ix :: GRID t -> (Int,Int) -> t
grid_ix g (r,c) = (g !! r) !! c

axis_incr :: (Fractional n, Enum n) => RANGE n -> Int -> n
axis_incr (lhs,rhs) n = (rhs - lhs) / fromIntegral n

-- > map (axis_loc (0,100)) [1,2,5,10,16]
axis_loc :: (Fractional n, Enum n) => RANGE n -> Int -> [n]
axis_loc (lhs,rhs) n =
  let u = axis_incr (lhs,rhs) n
      i = u / 2.0
      l = take n [i, i + u ..]
  in map (+ lhs) l

{- | (x,y) grid. nr = number-of-rows, nc = number-of-columns, xos = x-offset-sequence

The x-offset sequence is given in x-incr units and is cycled (ie. it can have fewer than nr places).
I.e. xos=[0,0.5] is a hexagonal grid.  xos=[0] is a square grid.

> grid_coord ((0,100),(0,100)) [0,0.5] (4,10)
-}
grid_coord :: (Fractional n, Enum n) => XY_RANGE n -> [n] -> (Int,Int) -> GRID (n,n)
grid_coord (x_rng,y_rng) xos (nr,nc) =
  let x_incr = axis_incr x_rng nc
      x_loc = axis_loc x_rng nc
      y_loc = axis_loc y_rng nr
  in map (\(y,xo) -> map (\x -> (x + xo,y)) x_loc) (zip y_loc (map (* x_incr) (cycle xos)))

grid_coord_unit :: (Fractional n, Enum n) => [n] -> (Int,Int) -> GRID (n,n)
grid_coord_unit = grid_coord ((0,1),(0,1))

-- > grid_pitch ([1],[4]) (4,13) 60
grid_pitch :: (Num n,Enum n) => ([n],[n]) -> (Int,Int) -> n -> GRID n
grid_pitch (x_incr,y_incr) (nr,nc) p0 =
  let x_seq x = take nc (List.dx_d x (cycle x_incr))
      y_seq y = take nr (List.dx_d y (cycle y_incr))
  in map x_seq (y_seq p0)

-- | (x,y,p) CSV table
grid_csv :: (Int,Int) -> GRID (Double,Double) -> GRID Double -> [String]
grid_csv dm grid_c grid_p =
  let f ix = let (x,y) = grid_ix grid_c ix
                 p = grid_ix grid_p ix
                 (r,c) = ix
             in printf "%d,%d,%.4f,%.4f,%.4f" r c x y p
  in map f (grid_indices dm)

{-

c = grid_coord_unit [0] (1,5+1)
p = grid_pitch ([2,2,3,2,3],[]) (1,5+1) 48

c = grid_coord_unit [0] (1,10+1)
p = grid_pitch ([2,2,3,2,3],[]) (1,10+1) 48

c = grid_coord_unit [0] (2,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (2,10+1) 43

c = grid_coord_unit [0] (3,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (3,10+1) 39

c = grid_coord_unit [0,0.5] (4,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (4,10+1) 44

c = grid_coord_unit [0] (4,12+1)
p = grid_pitch ([1],[5]) (4,12+1) 36

putStrLn $ unlines $ grid_csv (length c,length (c !! 0)) c p

-}

scl_fmidi_k :: Scala.Scale -> Int -> [Double]
scl_fmidi_k scl k =
  let o = Scala.pitch_cents (Scala.scale_octave_err scl) * 0.01
      d = Scala.scale_degree scl
      c = Scala.scale_cents scl
  in take k (concatMap (\i -> take d (map ((+ i) . (* 0.01)) c)) [0,o ..])

scl_x_axis_proportional :: Scala.Scale -> Int -> Double -> [(Double,Double)]
scl_x_axis_proportional scl k m0 =
  let m = scl_fmidi_k scl k
      l = last m
      x = map (/ l) m
  in zip x (map (+ m0) m)

x_axis_csv :: [(Double,Double)] -> [String]
x_axis_csv e =
  let f c (x,p) = printf "0,%d,%.4f,0.5,%.4f" c x p
  in zipWith f [0::Int ..] e

mode_degree_seq_cycle :: Mode.MODE -> [Int]
mode_degree_seq_cycle m =
  let l = Mode.mode_length m
      d = Mode.mode_degree_seq m
      u = Mode.mode_univ m
      z = take l d
  in concat (map (\i -> map (+ i) z) [0,u ..])

mode_degree_select :: Mode.MODE -> [t] -> [t]
mode_degree_select m x =
  let k = length x
      dgr = takeWhile (< k) (mode_degree_seq_cycle m)
  in map (x !!) dgr

{-

scl <- Scala.scl_load "indian"
x = scl_x_axis_proportional scl 45 48
putStrLn $ unlines $ x_axis_csv x

mn <- Mode.load_modenam
m_seq = filter ((== 22) . Mode.mode_univ) (Mode.modenam_search_description mn "Raga")
wr (i,m) = writeFile (printf "R-%02d.csv" i) (unlines (x_axis_csv (mode_degree_select m x)))
mapM_ wr (zip [0..] m_seq)

-}
