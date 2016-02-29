module Main where

import Lib

import Prelude hiding ((/))

{-
    syntatic category which maybe either a primitive like S, NP, N etc
    or function which constructing by forward or backward functor on other categories
    first argument of functor is result after application and second is argument
-}
data Category = 
      Category :/ Category  -- A / B - waiting argument on right side  
    | Category :\ Category  -- A \ B - waiting argument on left side
    | NP
    | S 
    | CONJ 
    deriving (Show, Read, Eq)

--(//) = ForwardFunctor
--(\\) = BackwardFunctor


isFunctor :: Category -> Bool
isFunctor (_ :/ _) = True
isFunctor (_ :\ _) = True
isFunctor _        = False

isPrimitive :: Category -> Bool
isPrimitive = not . isFunctor 

type Combinator =  Category         -- left lexeme
                -> Category         -- right lexeme
                -> Maybe Category   -- result if combinator applicable to arguments

-- X / Y • Y => X (>)
forwardApplication :: Combinator
forwardApplication  (x :/ y)  y' 
        | y == y' = Just x
forwardApplication _ _ = Nothing 

-- Y • X \ Y => X (<)
backwardApplication :: Combinator
backwardApplication  y  (x :\ y') 
        | y == y' = Just x
backwardApplication _ _ = Nothing 

-- X / Y • Y / Z => X / Z (> B)
forwardComposition :: Combinator
forwardComposition  (x :/ y)  (y' :/ z) 
        | y == y' = Just (x :/ z)
forwardComposition _ _ = Nothing 

-- there is also crossing forward and backward composition
-- X / Y • Y \ Z => X \ Z forward
-- X \ Y • Y / Z => X / Z backward

-- X \ Y • Y \ Z => X \ Z (> B)
backwardComposition :: Combinator
backwardComposition  (x :\ y)  (y' :\ z) 
        | y == y' = Just (x :\ z)
backwardComposition _ _ = Nothing 

-- (X / Y) / Z • Y / Z => X / Z (> S)
forwardSubstitution :: Combinator
forwardSubstitution  ((x :/ y) :/ z)  (y' :/ z')
        | y == y' && y == y' = Just (x :/ z)
forwardSubstitution _ _ = Nothing 

-- Y / Z • (X \ Y) / Z => X / Z (< Sx)
backwardXSubstitution :: Combinator
backwardXSubstitution  (y :/ z)  ((x :\ y') :/ z')
        | y == y' && y == y' = Just (x :/ z)
backwardXSubstitution _ _ = Nothing 

{-
    Type Raising
    In real parser we can't use this rule.
    So we should use it with any purpose for composition and our combinator will be pair of type raising and composition.
    We should use some euristics to prevent infinite type raising. Many implementations have different simple usages of type raising.
    There will be used NLTP algorithm.
-}

-- X => T / (T \ X) (>T)
forwardTypeRaising :: Category -> Category -> Category
forwardTypeRaising x t = t :/ (t :\ x)

-- X => T \ (T / X) (<T)
backwardTypeRaising :: Category -> Category -> Category
backwardTypeRaising x t = t :\ (t :/ x)



-- exception of combinators
-- X conj X => x (<&>)
coordination :: Category -> Category -> Category -> Maybe Category
coordination x CONJ x' | x == x' = Just x
coordination _ _ _ = Nothing


transitiveVerb = (S :\ NP) :/ NP

main :: IO ()
main = someFunc
