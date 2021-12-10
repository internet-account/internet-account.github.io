-- permut :: [a] -> [a] -> [[a]]
-- permut _ [] = []
-- permut [] [x] = [[x]]
-- permut hs (x:xs) =
--     let rests = hs++xs
--     in  map (x:) (permut [] rests) ++ permut (x:hs) xs
import Control.Monad ( (>=>) )
import Data.List ( maximumBy, minimumBy, partition, groupBy, sortOn, sortBy )
import Data.Function ( on )
import Data.Maybe ( isJust, isNothing )

main = do
    print $ show numMoves ++ " moegliche Zuege"
    print $ show numIllegalMoves ++ " illegale Zuege"
    print $ "Ein Zug mit maximaler Punktzahl: " ++ show maximumScore

allPermutations :: [a] -> [[a]]
allPermutations = _allPermutations []

_allPermutations :: [a] -> [a] -> [[a]]
_allPermutations _ [] = [[]]
_allPermutations hs (x:xs) =
    let rests = hs++xs
    in  map (x:) (_allPermutations [] rests) ++ _allPermutations (x:hs) xs

data Assets = Assets 
                { ataola :: Int
                , quombao :: Int
                , tamoro :: Int
                }
                deriving (Show)

startAssets :: Assets
startAssets = Assets 4 4 5

data Card = Card
                { name :: String
                , function :: Assets -> Maybe Assets
                }

-- Rot = Ataola
-- Gelb = Quombao
-- Blau = Tamoro

anke :: Assets -> Maybe Assets
anke assets          | ataola assets  >= 3 = Just assets
                                                 { ataola = ataola assets - 3
                                                 , quombao = 2 * quombao assets
                                                 }
                     | otherwise           = Nothing

elfant :: Assets -> Maybe Assets
elfant assets        | quombao assets >= 2 = Just assets
                                                 { quombao = quombao assets - 2
                                                 , tamoro = tamoro assets + 5
                                                 }
                     | otherwise           = Nothing

joey :: Assets -> Maybe Assets
joey assets          | quombao assets >= 1 = Just assets
                                                 { quombao = quombao assets - 1
                                                 , tamoro = tamoro assets + ataola assets
                                                 }
                     | otherwise           = Nothing

pfitschiPfeil :: Assets -> Maybe Assets
pfitschiPfeil assets | tamoro assets  >= 3 = Just assets
                                                 { tamoro = tamoro assets - 3
                                                 , ataola = ataola assets + 1
                                                 , quombao = quombao assets + 2
                                                 }
                     | otherwise           = Nothing

sunJ :: Assets -> Maybe Assets
sunJ assets          | tamoro assets  >= 2 = Just assets
                                                 { tamoro = tamoro assets - 2
                                                 , ataola = quombao assets
                                                 , quombao = ataola assets
                                                 }
                     | otherwise           = Nothing

teledahner :: Assets -> Maybe Assets
teledahner assets    | ataola assets  >= 4 = Just assets
                                                 { ataola = ataola assets - 4 + tamoro assets
                                                 , tamoro = 0
                                                 }
                     | otherwise           = Nothing

allCards :: [Card]
allCards = [Card "A" anke, Card "E" elfant, Card "J" joey, Card "P" pfitschiPfeil, Card "S" sunJ, Card "T" teledahner]

playCards :: [Assets -> Maybe Assets] -> Assets -> Maybe Assets
playCards = foldl (>=>) pure

results :: [(String, Maybe Assets)]
results = map (\cards ->
                  ( concatMap name cards
                  , playCards (map function cards) startAssets
                  )
              )
              (allPermutations allCards)

cmpMaybe :: Maybe Assets -> Maybe Assets -> Ordering
cmpMaybe Nothing _ = LT
cmpMaybe _ Nothing = GT
cmpMaybe (Just a) (Just b) = (compare `on` ataola) a b

resultsSorted = sortBy (cmpMaybe `on` snd) results

eqMaybe :: Maybe Assets -> Maybe Assets -> Bool
eqMaybe Nothing Nothing = True
eqMaybe (Just a) (Just b) = ((==) `on` ataola) a b
eqMaybe _ _ = False

resultsGrouped = groupBy (eqMaybe `on` snd) resultsSorted

scoreMaybe :: Maybe Assets -> Maybe Int
scoreMaybe (Just x) = Just $ ataola x
scoreMaybe Nothing = Nothing

resultsMap = map ((,) <$> scoreMaybe . snd . head <*> id) resultsGrouped

numMoves = length results
numIllegalMoves = maybe 0 length $ lookup Nothing resultsMap
maximumScore = last resultsSorted
