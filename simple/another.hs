module Inf where

import Data.Ratio

type Prob = Rational
type Dist a = a -> Prob
type Face = Int

type Inf a b = b -> Dist a

-- | infer da @ [prior dist of a] lb @ [likelyhood of b from a]
infer :: Dist Face -> (Face -> Dist b) -> Inf Face b
-- [F] -> [b] -> (F -> Prob) -> (F -> (b -> Prob)) -> (b -> (F-> Prob))
infer da lb = \b a -> lb a b * da a

-- | E1. infers a posterior distribution of faces after observing the face
fid :: Inf Face Face
fid x = \y -> if y == x then 1 else 0

-- | E2. an example of an Inf type, infers a distribution of faces from an 
-- observation of bool, where bool is whether the face <= 3
small :: Inf Face Bool
small True = \x -> if x <= 3 then 1%3 else 0
small False = \x -> if x > 3 then 1%3 else 0

-- posterior probability of throwing 3 from the observation that the throw is
-- less than 3 (the predicate face <= 3 is True)
p2Gt :: Prob
p2Gt = small True 3

-- | infers a posterior distribution of faces from the likely hood of marking
-- the left (<= 3) column
mark3 :: Inf Face Bool
mark3 = infer d6 mark

-- distribution of faces on a d6
d6 :: Dist Face
d6 n = if elem n [1..6] then 1%6 else 0

-- likelyhood of marking the left col from each face
mark :: Face -> Dist Bool
mark a = \b -> if a <= 3 then (if b then 1 else 0)
                         else (if b then 0 else 1)

