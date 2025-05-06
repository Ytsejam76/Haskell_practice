{- Author: Elias Carotti

Naive implementation of
https://www.norvig.com/sudoku.html

to learn Haskell
-}

import Control.Monad (foldM, forM_, msum, when)
import Data.List (intercalate, minimumBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set

data Cols = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
  deriving (Show, Enum, Bounded, Ord, Eq)

data Rows = A | B | C | D | E | F | G | H | I
  deriving (Show, Enum, Bounded, Ord, Eq)

data Digits = Blank | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Show, Enum, Bounded, Ord, Eq)

type Coord = (Rows, Cols)

type KeyVals = Map.Map Coord [Digits]

digits :: [Digits]
digits = [minBound .. maxBound :: Digits]

rows :: [Rows]
rows = [minBound .. maxBound :: Rows]

cols :: [Cols]
cols = [minBound .. maxBound :: Cols]

squares :: [Coord]
squares = [(r', c') | r' <- rows, c' <- cols]

boxes :: [[Coord]]
boxes = [[(r', c') | r' <- rs', c' <- cs'] | rs' <- [[A, B, C], [D, E, F], [G, H, I]], cs' <- [[C1, C2, C3], [C4, C5, C6], [C7, C8, C9]]]

toDigit :: Int -> Digits
toDigit 0 = Blank
toDigit a = (toEnum a :: Digits)

units :: Map.Map Coord ([[Coord]])
units = Map.fromList $ map (\coo -> (coo, unit coo)) squares
  where
    unit :: Coord -> [[Coord]]
    unit (r, c) =
      let unitCol = [(r', c) | r' <- rows]
          unitRow = [(r, c') | c' <- cols]
          unitBox = head $ filter (elem (r, c)) boxes
       in Set.toList $ Set.fromList (unitCol : unitRow : unitBox : [])

readMatrix :: FilePath -> IO (Either String [[Digits]])
readMatrix filePath = do
  contents <- readFile filePath
  let firstNineLines = lines contents
  if length firstNineLines /= 9
    then return (Left $ "Error. File contains " ++ (show $ length firstNineLines) ++ " lines")
    else
      if all ((== 9) . length . words) firstNineLines
        then return (Right $ map (map (toDigit . read) . words) $ firstNineLines)
        else return (Left "Error: exactly 9 elements per line expected")

peers :: Map.Map Coord ([Coord])
peers =
  let sudoku = [(r, c) | r <- rows, c <- cols]
   in Map.fromList $ map (\coo -> (coo, peers' coo)) sudoku
  where
    peers' (r, c) =
      Set.toList $ Set.fromList $ filter (/= (r, c)) $ concat $ units Map.! (r, c)

gridValues :: [[Digits]] -> KeyVals
gridValues grid =
  Map.fromList $ [((r, c), [val]) | (r, rowVals) <- zip rows grid, (c, val) <- zip cols rowVals]

parseGrid :: [[Digits]] -> Maybe KeyVals
parseGrid grid = foldM assignValue initialValues (Map.toList $ gridValues grid)
  where
    initialValues = Map.fromList [((r, c), filter (/= Blank) digits) | r <- rows, c <- cols]

    assignValue :: KeyVals -> (Coord, [Digits]) -> Maybe KeyVals
    assignValue values (coo, [d])
      | d == Blank = Just values
      | otherwise = assign values coo d
    assignValue _ (_, ds) =
      error $ "Invalid input at coordinate: expected one digit, got " ++ show ds

assign :: KeyVals -> Coord -> Digits -> Maybe KeyVals
assign values coo digit = foldM (\vals d2 -> eliminate vals coo d2) values otherDigits
  where
    otherDigits = filter (/= digit) $ values Map.! coo

eliminate :: KeyVals -> Coord -> Digits -> Maybe KeyVals
eliminate values coo digit
  | digit `notElem` val_at_coo = Just values
  | otherwise = do
      let newValues = Map.adjust (filter (/= digit)) coo values
      case Map.lookup coo newValues of
        Just [] -> Nothing
        Just [d2] -> do
          newVals <- foldM (\vals p_coo -> eliminate vals p_coo d2) newValues $ peers Map.! coo
          eliminate' newVals coo digit
        _ -> eliminate' newValues coo digit
  where
    val_at_coo = values Map.! coo

    eliminate' :: KeyVals -> Coord -> Digits -> Maybe KeyVals
    eliminate' values' coo' digit' =
      foldM updateUnit values' (units Map.! coo')
      where
        updateUnit :: KeyVals -> [Coord] -> Maybe KeyVals
        updateUnit vals unit = do
          let dplaces = [s | s <- unit, Just ds <- [Map.lookup s vals], digit' `elem` ds]
          case dplaces of
            [] -> Nothing
            [s'] -> assign vals s' digit'
            _ -> Just vals

search :: Maybe KeyVals -> Maybe KeyVals
search Nothing = Nothing
search (Just values) = search' values

search' :: KeyVals -> Maybe KeyVals
search' values
  | all ((== 1) . length) (Map.elems values) = Just values
  | otherwise = do
      let unfilled = [(length ds, s) | (s, ds) <- Map.toList values, length ds > 1]
      (_, s) <- minimumMaybe unfilled
      msum [search (assign values s d) | d <- values Map.! s]
  where
    minimumMaybe :: (Ord b) => [(b, a)] -> Maybe (b, a)
    minimumMaybe [] = Nothing
    minimumMaybe xs = Just (minimumBy (comparing fst) xs)

solve :: [[Digits]] -> Either String KeyVals
solve matrix = do
  case parseGrid matrix of
    Nothing -> Left $ "Can't parse grid: " ++ (show $ gridValues matrix)
    Just values -> do
      let result = search (Just values)
      case result of
        Nothing -> Left "No solution found"
        Just solvedValues -> Right solvedValues

printGrid :: KeyVals -> IO ()
printGrid values = forM_ rows $ \r -> do
  putStrLn $ concat [center (map (fromEnum) (values Map.! (r, c))) ++ sep c | c <- cols]
  when (r `elem` [C, F]) $ putStrLn $ replicate (width * 9 + 2) '-'
  where
    width = 1 + maximum (map (length) (Map.elems values)) -- Adjust for element length
    center ds =
      let s = show ds
          pad = replicate ((width - length s) `div` 2) ' '
       in pad ++ s ++ pad ++ replicate (width - length (pad ++ s)) ' '
    sep c = if c `elem` [C3, C6] then " | " else ""

main :: IO ()
main = do
  let filePath = "test/sudoku.txt"
  grid <- readMatrix filePath
  case grid of
    Left err -> print $ "Error: " ++ err
    Right grid' -> do
      case (solve grid') of
        Left err -> print $ "Error: " ++ err
        Right solvedGrid -> printGrid solvedGrid
