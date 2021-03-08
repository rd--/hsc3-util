import Text.Printf {- base -}

import Data.CG.Minus.Plain {- hcg-minus -}

import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}
import qualified Music.Theory.Tuning.Scala.Mode as Mode {- hmt -}

type RANGE n = V2 n

type XY_RANGE n = V2 (RANGE n)

-- | Row-order grid.  All rows have an equal number of columns.
type GRID t = [[t]]

grid_map :: (t -> u) -> GRID t -> GRID u
grid_map f = map (map f)

-- | Row-order sequence of all grid indices.
--
-- > grid_indices (2,3) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid_indices :: V2 Int -> [V2 Int]
grid_indices (nr,nc) = [(r,c) | r <- [0 .. nr - 1], c <- [0 .. nc - 1]]

-- | Lookup element at grid.
grid_ix :: GRID t -> V2 Int -> t
grid_ix g (r,c) = (g !! r) !! c

-- | Increment for axis, for axis-aligned grid of /n/ places (ie. cell diameter)
axis_incr :: (Fractional n, Enum n) => RANGE n -> Int -> n
axis_incr (lhs,rhs) n = (rhs - lhs) / fromIntegral n

-- | Locations along axis, for axis-aligned grid.
--
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
grid_coord :: (Fractional n, Enum n) => XY_RANGE n -> [n] -> V2 Int -> GRID (V2 n)
grid_coord (x_rng,y_rng) xos (nr,nc) =
  let x_incr = axis_incr x_rng nc
      x_loc = axis_loc x_rng nc
      y_loc = axis_loc y_rng nr
  in map (\(y,xo) -> map (\x -> (x + xo,y)) x_loc) (zip y_loc (map (* x_incr) (cycle xos)))

grid_coord_unit :: (Fractional n, Enum n) => [n] -> V2 Int -> GRID (V2 n)
grid_coord_unit = grid_coord ((0,1),(0,1))

-- > grid_pitch ([1],[4]) (4,13) 60
grid_pitch :: (Num n,Enum n) => V2 [n] -> V2 Int -> n -> GRID n
grid_pitch (x_incr,y_incr) (nr,nc) p0 =
  let x_seq x = take nc (List.dx_d x (cycle x_incr))
      y_seq y = take nr (List.dx_d y (cycle y_incr))
  in map x_seq (y_seq p0)

-- | (i,j,x,y,p,w,h) CSV table
grid_csv :: V2 Int -> V2 Double -> GRID (V2 Double) -> GRID Double -> GRID [V2 Double] -> [String]
grid_csv dm (w,h) grid_c grid_p grid_r =
  let f ix = let (x,y) = grid_ix grid_c ix
                 p = grid_ix grid_p ix
                 [(x1,y1),(x2,y2),(x3,y3),(x4,y4)] = grid_ix grid_r ix
                 (i,j) = ix
             in printf "%d,%d,%.4f,%.4f,%.4f,%.4f,%.4f,4,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f" i j x y p w h x1 y1 x2 y2 x3 y3 x4 y4
  in map f (grid_indices dm)

grid_diameter :: V2 Int -> V2 Double
grid_diameter (nr,nc) = (0.5 / fromIntegral nc,0.5 / fromIntegral nr)

grid_rect :: V2 Double -> V2 Double -> [V2 Double]
grid_rect (w,h) (x,y) =
  let dx = w
      dy = h
  in [(x - dx,y - dy),(x + dx,y - dy),(x + dx,y + dy),(x - dx,y + dy)]

gen_grid_csv :: [Double] -> V2 Int -> V2 [Double] -> Double -> [String]
gen_grid_csv xos dm incr p0 =
  let c = grid_coord_unit xos dm
      p = grid_pitch incr dm p0
      r = grid_map (grid_rect (grid_diameter dm)) c
  in grid_csv dm (grid_diameter dm) c p r

{-

mk xos dm incr = putStrLn . unlines . gen_grid_csv xos dm incr

mk [0] (1,5+1) ([2,2,3,2,3],[]) 48
mk [0] (1,10+1) ([2,2,3,2,3],[]) 48
mk [0] (2,10+1) ([2,2,3,2,3],[5]) 43
mk [0] (3,10+1) ([2,2,3,2,3],[5]) 39
mk [0] (4,10+1) ([2,2,3,2,3],[5]) 44
mk [0,0.5] (4,10+1) ([2,2,3,2,3],[5]) 44
mk [0] (4,12+1) ([1],[5]) 36

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

x_axis_csv :: [(Double,Double,Double)] -> [String]
x_axis_csv e =
  let f c (x,p,w) = printf "0,%d,%.4f,0.5,%.4f,%.4f,1.0" c x p w
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
