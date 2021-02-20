import Text.Printf {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- > map (axis_loc (0,100)) [1,2,5,10,16]
axis_loc :: (Fractional n, Enum n) => (n,n) -> Int -> [n]
axis_loc (lhs,rhs) n =
  let dst = rhs - lhs
      u = dst / fromIntegral n
      i = u / 2.0
      l = take n [i, i + u ..]
  in map (+ lhs) l

type GRID t = [[t]]

-- | nr = number-of-rows, nc = number-of-columns
--
-- > grid_coord (0,100) (0,100) (4,10)
grid_coord :: (Fractional n, Enum n) => (n,n) -> (n,n) -> (Int,Int) -> GRID (n,n)
grid_coord x_rng y_rng (nr,nc) =
  let x_loc = axis_loc x_rng nc
      y_loc = axis_loc y_rng nr
  in map (\y -> map (\x -> (x,y)) x_loc) y_loc

grid_coord_unit :: (Fractional n, Enum n) => (Int,Int) -> GRID (n,n)
grid_coord_unit = grid_coord (0,1) (0,1)

-- > grid_pitch ([1],[4]) (4,13) 60
grid_pitch :: (Num n,Enum n) => ([n],[n]) -> (Int,Int) -> n -> GRID n
grid_pitch (x_incr,y_incr) (nr,nc) p0 =
  let x_seq x = take nc (T.dx_d x (cycle x_incr))
      y_seq y = take nr (T.dx_d y (cycle y_incr))
  in map x_seq (y_seq p0)

-- > 
grid_csv :: GRID (Double,Double) -> GRID Double -> [String]
grid_csv grid_c grid_p =
  let f (x,y) p = printf "%.4f,%.4f,%.4f" x y p
  in concat (zipWith (zipWith f) grid_c grid_p)

{-

c = grid_coord_unit (1,5+1)
p = grid_pitch ([2,2,3,2,3],[]) (1,5+1) 48

c = grid_coord_unit (1,10+1)
p = grid_pitch ([2,2,3,2,3],[]) (1,10+1) 48

c = grid_coord_unit (2,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (2,10+1) 43

c = grid_coord_unit (3,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (3,10+1) 39

c = grid_coord_unit (4,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (4,10+1) 44

c = grid_coord_unit (5,10+1)
p = grid_pitch ([2,2,3,2,3],[5]) (5,10+1) 42

c = grid_coord_unit (4,12+1)
p = grid_pitch ([1],[5]) (4,12+1) 36

putStrLn $ unlines $ grid_csv c p

-}
