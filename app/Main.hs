module Main where

import Lib

--import Prelude hiding ((/))
import Data.Maybe (catMaybes) -- [Maybe a] -> [a]
import Control.Applicative ((<*>))
{-
    syntatic category which maybe either a primitive like S, NP, N etc
    or function which constructing by forward or backward functor on other categories
    first argument of functor is result after application and second is argument
-}
data Category = 
      Category :/ Category  -- A / B - waiting argument on right side  
    | Category :\ Category  -- A \ B - waiting argument on left side
    | NP
    | N
    | S 
    | CONJ 
    deriving (Show, Read, Eq)

--(//) = ForwardFunctor
--(\\) = BackwardFunctor

-- Cell data down right diagonal
data Chart = Cell [Category] Chart Chart Chart | End deriving (Show, Read, Eq)

getColumn   End                  = []
getColumn   (Cell val chart _ _) = val : (getColumn chart)
getRow      End                  = []
getRow      (Cell val _ chart _) = val : (getRow chart)
getDiagonal End                  = []
getDiagonal (Cell val _ _ chart) = val : (getDiagonal chart)

chartToList End                  = []
chartToList chart'@(Cell val chart _ _) = getRow chart' : (chartToList chart)

listToChartRow :: [[Category]] -> Chart
listToChartRow []     = End
listToChartRow (x:xs) = Cell x End (listToChartRow xs) End

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


-- very simple usage of type raising
-- X • (Y \ X) / Z => Y / (Y \ X) • (Y \ X) / Z (>T) => Y / Z (>B)
forwardTypeRaisingComposition :: Combinator
forwardTypeRaisingComposition x right@((y :\ x') :/ z)
        | x == x' = forwardComposition (forwardTypeRaising x y) right
forwardTypeRaisingComposition _ _ = Nothing

-- X • (Y / X) \ Z => Y \ (Y / X) • (Y / X) \ Z (<T) => Y \ Z (<B)
backwardTypeRaisingComposition :: Combinator
backwardTypeRaisingComposition x right@((y :/ x') :\ z)
        | x == x' = backwardComposition (backwardTypeRaising x y) right
backwardTypeRaisingComposition _ _ = Nothing

-- (X \ Y) / Z • R => (X \ Y) / Z • Z / (Z \ R) (>T) => (X \ Y) / (Z \ R) (>B)
leftForwardTypeRaisingComposition :: Combinator
leftForwardTypeRaisingComposition left@((x :\ y) :/ z) r = forwardComposition left (forwardTypeRaising r z)
leftForwardTypeRaisingComposition _ _ = Nothing

-- (X / Y) \ Z • R => (X / Y) \ Z • Z \ (Z / R) (<T) => (X / Y) \ (Z / R) (<B)
leftBackwardTypeRaisingComposition :: Combinator
leftBackwardTypeRaisingComposition left@((x :/ y) :\ z) r = backwardComposition left (backwardTypeRaising r z)
leftBackwardTypeRaisingComposition _ _ = Nothing

-- exception of combinators
-- X conj X => x (<&>)
coordination :: Category -> Category -> Category -> Maybe Category
coordination x CONJ x' | x == x' = Just x
coordination _ _ _ = Nothing


-- version of map which gets two items of list applying function to them and passes to new list
-- from list with length n you get n-1 length list: [a, b, c] => [f a b, f b c]
map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (a : tail@(b : _)) = f a b : map2 f tail
map2 _ _ = []

parse :: [Combinator] -> [[[Category]]] -> [Category]
parse [] _ = []
parse rules table@(lastline:_)  = head $ concat (parse_iter rules table (length lastline) 1)
{-        | length lastline <= 1 = concat lastline
        | otherwise = parse rules (map2 (derive rules) lastline : table)
        where-} 
        -- derive get two lists of possible categories and tries apply each rule with each combination of list production
        -- and then removes impossible rule applications 
parse _ _ = []
-- first step = 2, n = length
parse_iter :: [Combinator] -> [[[Category]]] -> Int -> Int -> [[[Category]]]
parse_iter rules table n step --n = 3,  step = 1
        | n == step = table 
        | otherwise = parse_iter rules (put_line:table) n (step + 1)
        where put_line     = map put_cell [n - step..1] -- j = 1..1
              get_cell' i j = table !! (i - 1) !! (j - 1)
              get_cell i j 
                    | i-1 >= length table = error $ "at i; " ++ show (i, j)  ++ "\n" ++ show table
                    | j-1 >= length (table !! (i-1)) = error $ "at j; " ++ show (i, j) ++ "\n" ++ show table
                    | otherwise = get_cell' i j
              put_cell j   = catMaybes [ rule left right | k <- [1..step], left <- get_cell (step + 1 - k) j, -- j = 1, k = 1..2
                                right <- get_cell k (j + k),  rule <- rules] -- kkk


parse' :: [Combinator] -> [[Category]] -> Chart
parse' rules lexemes = parse'' rules (listToChartRow lexemes)

parse'' :: [Combinator] -> Chart -> Chart
parse'' _ chart@(Cell _ _ End _) = chart
parse'' rules chart = parse'' rules (put_cell chart)
    where put_cell (Cell val _ End _) = End
          put_cell chart@(Cell _ _ right _) = Cell (cellData chart) chart (put_cell right) right
          cellData chart@(Cell _ _ right _) = cellData' (getColumn chart) (reverse $ getDiagonal right)
          cellData' lefts rights = catMaybes $ concat (zipWith (applyRules rules) lefts rights) -- unique
          applyRules rules ls rs = rules <*> ls <*> rs

derive rules leftCats rightCats = catMaybes (rules <*> leftCats <*> rightCats)

_derive = derive defaultCombinatorsSet

defaultCombinatorsSet = 
        [
            forwardApplication, backwardApplication,
            forwardComposition, backwardComposition,
            forwardSubstitution, backwardXSubstitution
            --forwardTypeRaisingComposition, backwardTypeRaisingComposition, leftForwardTypeRaisingComposition, leftBackwardTypeRaisingComposition
        ]

transitiveVerb = (S :\ NP) :/ NP

sample = parse defaultCombinatorsSet [[[NP], [transitiveVerb], [NP]]]
sample' = parse' defaultCombinatorsSet [[NP :/ N], [N], [transitiveVerb], [NP]]

main :: IO ()
main = someFunc
