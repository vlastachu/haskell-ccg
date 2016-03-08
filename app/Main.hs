module Main where

import Lib

--import Prelude hiding ((/))
import Data.Maybe (catMaybes) -- [Maybe a] -> [a]
import Control.Applicative ((<*>), (<$>))
import System.IO
--import Data.Functor ((<))
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
    | NON -- need for chart
    deriving (Show, Read, Eq)

prettyShow (l :/ r) = prettyShow l ++ " / "  ++ prettyShow r
prettyShow (l :\ r) = prettyShow l ++ " \\ " ++ prettyShow r
prettyShow other    = show other

isFunctor :: Category -> Bool
isFunctor (_ :/ _) = True
isFunctor (_ :\ _) = True
isFunctor _        = False

isPrimitive :: Category -> Bool
isPrimitive = not . isFunctor 

data CellData = CellData {category :: Category, catName :: String, path :: ParsePath} deriving (Show, Read, Eq)

instance Show ParsePath where
    show _ = ""
-- binary tree 
data ParsePath = ParsePath CellData CellData | PPEND deriving (Read, Eq)

-- Cell data down right diagonal
data Chart = Cell [CellData] Chart Chart Chart | End deriving (Show, Read, Eq)

getColumn   End                  = []
getColumn   (Cell val chart _ _) = val : (getColumn chart)
getRow      End                  = []
getRow      (Cell val _ chart _) = val : (getRow chart)
getDiagonal End                  = []
getDiagonal (Cell val _ _ chart) = val : (getDiagonal chart)

-- get down in char by n cells
verticalStep chart 0 = chart
verticalStep End _   = End
verticalStep (Cell _ chart _ _) n = verticalStep chart $ n - 1
-- right down by n
diagonalStep chart 0 = chart
diagonalStep End _   = End
diagonalStep (Cell _ _ chart _) n = diagonalStep chart $ n - 1


chartToList End                  = []
chartToList chart'@(Cell val chart _ _) = getRow chart' : (chartToList chart)

listToChartRow :: [(String, [Category])] -> Chart
listToChartRow []     = End
listToChartRow ((word, cats):xs) = Cell (map (\x' -> CellData x' word PPEND) cats) End (listToChartRow xs) End

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



parse :: [(Combinator, String)] -> [(String, [Category])] -> Chart
parse rules lexemes = parse_iter rules (listToChartRow lexemes)



parse_iter :: [(Combinator, String)] -> Chart -> Chart
-- parsing until pyramid have 1 block in top row (right link is End in first cell)
parse_iter _ chart@(Cell _ _ End _) = chart
parse_iter rules chart = parse_iter rules (put_cell chart)
    where -- put cell until there is two cell under new 
      put_cell (Cell val _ End _) = End
      -- put cell with: one last at down, one new at right and one last on diagonal
      put_cell chart@(Cell _ _ right _) = Cell (cellData chart) chart (put_cell right) right
      cellData chart@(Cell _ _ right _) = applyRulesToDerivations (getColumn chart) (reverse $ getDiagonal right)
      applyRulesToDerivations downList diagList 
            = catMaybes $ concat (zipWith applyRules downList diagList) -- unique
      applyRules lefts rights = 
        [(\x -> CellData x name (ParsePath ldata rdata)) <$> (rule left right) | 
            ldata@(CellData left  _ _) <- lefts, 
            rdata@(CellData right _ _) <- rights, 
            (rule, name) <- rules]

defaultCombinatorsSet = 
        [
            (forwardApplication, ">"), (backwardApplication, "<"),
            (forwardComposition, ">B"), (backwardComposition, "<B"),
            (forwardSubstitution, ">S"), (backwardXSubstitution, "<S"),
            (forwardTypeRaisingComposition, ">T"), (backwardTypeRaisingComposition, "<T"),
            (leftForwardTypeRaisingComposition, "<>T"), (leftBackwardTypeRaisingComposition, "<<T")
        ]

transitiveVerb = (S :\ NP) :/ NP

sample = parse defaultCombinatorsSet [("Mary", [NP]), ("loves", [transitiveVerb]), ("John", [NP])]
--sample = parse defaultCombinatorsSet [("There", [NP]), ("may", [(S:\NP):/(S:\NP)]), ("be", [transitiveVerb]), 
--    ("others", [N]), ("doing", [transitiveVerb]), ("what", [NP:/(S:/NP)]), ("she", [NP]), ("did", [transitiveVerb])
--    ]

html content = "<html><head>" ++
    "<style>* {padding:0; margin: 0} .root{padding:20px}.category{display:inline-block; padding: 0px 15px 15px 0px; text-align:center; vertical-align:top}</style>"
    ++ "<title>result</title></head><body>" ++ content ++ "</body>"

chartToHtml :: Chart -> String
chartToHtml (Cell datas _ _ _) = foldr (\d ds -> "<div class=\"derivation\">" ++ d ++ "</div>" ++ ds) [] showDerivations
    where 
    showDerivations = catMaybes $ map showDerivation datas
    showDerivation cellData@(CellData S _ _) = Just $ "<div class=\"root\">" ++ showParsePath cellData ++  "</div>"
    showDerivation _ = Nothing
    showParsePath (CellData cat ruleName (ParsePath left right)) = 
        "<div class=\"category\">" ++ showParsePath left ++ showParsePath right 
        ++ "<hr/>" ++ prettyShow cat ++ " (" ++ ruleName ++ ")</div>"
    showParsePath cellData@(CellData cat word PPEND) = 
        "<div class=\"word category\">" ++ word ++ "<hr/>" ++ prettyShow cat ++ "</div>"


chartFile = html . chartToHtml

main :: IO ()
main = do 
    file <- openFile "output.html" WriteMode
    hPutStrLn file $ chartFile sample
    hClose file
