module Main where

import Prelude hiding (lookup)
import Data.List hiding (lookup)
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import System.IO

type Digit  = Char
type Square = String
type Unit   = [Square]

type Grid = M.Map Square [Digit]

rows = "ABCDEFGHI"
cols = "123456789"
digits = "123456789"

cross :: String -> String -> [String]
cross rows cols = [ [r, c] | r <- rows, c <- cols ]

squares :: [Square]
squares = cross rows cols

unitList :: [Unit]
unitList = [ cross rows [c] | c <- cols ] ++ [ cross [r] cols | r <- rows ] ++
            [ cross rs cs | rs <- ["ABC","DEF","GHI"], cs <- ["123","456","789"]]

units :: M.Map Square [Unit]
units = M.fromList [ (s, [ u | u <- unitList, elem s u ]) | s <- squares ]

peers :: M.Map Square [Square]
peers = M.fromList [ (s, set [[ p | p <- e, p /= s ] | e <- lookup s units ]) | s <- squares ]
   where set = nub . concat

lookup :: (Ord a, Show a) => a -> M.Map a b -> b
lookup k v = case M.lookup k v of
                 Just x -> x
                 Nothing -> error $ "Error : key " ++ show k ++ " not in map !"

parseGrid :: String -> Maybe Grid
parseGrid g = do regularGrid g
                 foldM assign allPossibilities (zip squares g)
   where  allPossibilities :: Grid
          allPossibilities = M.fromList [ (s,digits) | s <- squares ]
          regularGrid :: String -> Maybe String
          regularGrid g  = if all (\c -> (elem c "0.-123456789")) g
                              then (Just g)
                              else Nothing

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = if (elem d digits) then do
                     let toDump = delete d (lookup s g)
                     res <- foldM eliminate g (zip (repeat s) toDump)
                     return res
                  else return g

eliminate ::  Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) = let cell = lookup s g in
                     if not (elem d cell) then return g
                        else do let newCell = delete d cell
                                    newV = M.insert s newCell g
                                newV2 <- case length newCell of
                                            0 -> Nothing
                                            1 -> do let peersOfS = [ s' | s' <- lookup s peers ]
                                                    res <- foldM eliminate newV (zip peersOfS (cycle newCell))
                                                    return res
                                            _ -> return newV
                                let dPlaces = [ s' | u <- lookup s units, s' <- u, elem d (lookup s' newV2) ]
                                case length dPlaces of
                                   0 -> Nothing
                                   1 -> assign newV2 (head dPlaces, d)
                                   _ -> return newV2

search :: Maybe Grid -> Maybe Grid
search Nothing  = Nothing
search (Just g) = if all (\xs -> length xs == 1) [ lookup s g | s <- squares ]
                     then (Just g)
                     else do let (_,s) = minimum [ (length (lookup s g),s) | s <- squares, length (lookup s g) > 1 ]
                                 g' = g
                             foldl' some Nothing [ search (assign g' (s,d)) | d <- lookup s g ]
   where some Nothing Nothing  = Nothing
         some Nothing (Just g) = (Just g)
         some (Just g) _ = (Just g)

printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString

gridToString :: Grid -> String
gridToString g = let l0 = map snd (M.toList g);
                     l1 = map (\s -> " " ++ s ++ " ") l0;
                     l2 = map concat . sublist 3 $ l1;
                     l3 = sublist 3 l2;
                     l4 = map (intercalate "|") l3;
                     l5 = intercalate [line] . sublist 3 $ l4
                 in unlines l5
    where sublist n [] = []
          sublist n xs = take n xs : sublist n (drop n xs)
          line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
          hyphens = take 9 (repeat '-')

main :: IO ()
main = withFile "sudoku.txt" ReadMode $ \h -> do
    grids <- hGetContents h
    let solved = mapMaybe (search . parseGrid) (lines grids)
    mapM_ printGrid solved 