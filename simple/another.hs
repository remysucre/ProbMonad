module Inf where

import Data.Ratio

type Prob = Rational
type Dist a = a -> Prob
type Face = Int

type InfFace b = b -> Dist Face

id :: InfFace Face
id x = \y -> if y == x then 1 else 0

small :: InfFace Bool
small True = \f -> if f <= 3 then 1%3 else 0
small False = \f -> if f > 3 then 1%3 else 0

-- | infer df @ [dist of f] lb @ [likelyhood of b from f]
infer :: Dist Face -> (Face -> Dist b) -> InfFace b
-- [F] -> [b] -> (F -> Prob) -> (F -> (b -> Prob)) -> (b -> (F-> Prob))
infer df lb = \b f -> lb f b * df f

d6 :: Dist Face
d6 n = if elem n [1..6] then 1%6 else 0

mark :: Face -> Dist Bool
mark f = \b -> if f <= 3 then (if b then 1 else 0)
                         else (if b then 0 else 1)

mark3 :: InfFace Bool
mark3 = infer d6 mark

-- 
-- small :: InfFace Bool
-- small True = Dist [ (Face 1, 1 % 3)
--                   , (Face 2, 1 % 3)
--                   , (Face 3, 1 % 3)
--                   , (Face 4, 0)
--                   , (Face 5, 0)
--                   , (Face 6, 0)
--                   ]
-- small False = Dist [ (Face 1, 0)
--                    , (Face 2, 0)
--                    , (Face 3, 0)
--                    , (Face 4, 1 % 3)
--                    , (Face 5, 1 % 3)
--                    , (Face 6, 1 % 3)
--                    ]
-- 
-- -- | infer df @ [a priori dist of face] lb @ [likelyhood of b given a]
-- infer :: Dist Face -> (Face -> Dist b) -> InfFace b
-- infer (Dist df) lb = map (\(f, p) -> lb f)
-- 
-- infer (Dist [(Face 1, 1%6),
--              (Face 2, 1%6), 
--              (Face 3, 1%6), 
--              (Face 4, 1%6),
--              (Face 5, 1%6),
--              (Face 6, 1%6)])
--       (\(Face f) -> if f <= 3 then Dist [(True, 1), (False, 0)] 
--                        else Dist [(True, 0), (False, 1)])
--       = small
-- 
