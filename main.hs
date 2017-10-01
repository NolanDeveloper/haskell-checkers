{-
W - white
Q - white king
B - black
K - black king
  +-+-+-+-+-+-+-+-+
8 | |b| |b| |b| |b|
7 |b| |b| |b| |b| |
6 | |b| |b| |b| |b|
5 | | | | | | | | |
4 | | | | | | | | |
3 |w| |w| |w| |w| |
2 | |w| |w| |w| |w|
1 |w| |w| |w| |w| |
  +-+-+-+-+-+-+-+-+
   a b c d e f g h
-}

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Vector as V
import System.IO
import Debug.Trace

data Player = Black | White
    deriving (Eq, Show)

data Piece = Man | King
    deriving Eq

data Tile = Empty | Piece Player Piece
    deriving Eq

instance Show Tile where
    show Empty = " "
    show (Piece Black Man) = "b"
    show (Piece Black King) = "B"
    show (Piece White Man) = "w"
    show (Piece White King) = "W"

newtype Field = Field (V.Vector (V.Vector Tile))

type Position = (Int, Int)

data GameState = GameState Player Field [Position]

fieldGet :: Field -> Position -> Tile
fieldGet (Field tiles) (row, col) = tiles V.! row V.! col

fieldSet :: Field -> Position -> Tile -> Field
fieldSet (Field tiles) (row, col) tile =
    Field $ tiles V.// [(row, (tiles V.! row) V.// [(col, tile)])]

printField :: Field -> IO ()
printField field = do
    putStrLn "  +-+-+-+-+-+-+-+-+"
    forM_ [7, 6..0] $ \i -> do
        putStr $ show (i + 1)
        putStr " |"
        forM_ [0..7] $ \j -> do
            putStr $ show $ fieldGet field (i, j)
            putStr "|"
        putChar '\n'
    putStrLn "  +-+-+-+-+-+-+-+-+"
    putStrLn "   a b c d e f g h"

startingField :: Field
startingField =
    Field (V.fromList
        [ V.fromList [w, e, w, e, w, e, w, e]
        , V.fromList [e, w, e, w, e, w, e, w]
        , V.fromList [w, e, w, e, w, e, w, e]
        , V.fromList [e, e, e, e, e, e, e, e]
        , V.fromList [e, e, e, e, e, e, e, e]
        , V.fromList [e, b, e, b, e, b, e, b]
        , V.fromList [b, e, b, e, b, e, b, e]
        , V.fromList [e, b, e, b, e, b, e, b]
        ])
  where
    e = Empty
    b = Piece Black Man
    w = Piece White Man

isTileOf :: Tile -> Player -> Bool
isTileOf Empty _ = False
isTileOf (Piece player' _) player = player' == player

enemy :: Player -> Player
enemy White = Black
enemy Black = White

isOnField :: Position -> Bool
isOnField (r, c) = 0 <= r && r < 8 && 0 <= c && c < 8

kingRowOf :: Player -> Int
kingRowOf White = 0
kingRowOf Black = 7

makeKing :: Tile -> Tile
makeKing (Piece player _) = Piece player King
makeKing _ = error "Tile is not a piece"

movePiece :: Field -> Position -> Position -> Field
movePiece field src dst@(r, _) = fieldSet (fieldSet field src Empty) dst tile'
  where
    tile@(Piece player _) = fieldGet field src
    shouldBecomeKing = r == kingRowOf (enemy player)
    tile' = if shouldBecomeKing then makeKing tile else tile

frontDirection :: Player -> Int
frontDirection Black = (-1)
frontDirection White = 1

intermediatePositions :: Position -> Position -> [Position]
intermediatePositions (r, c) (r', c')
    | abs (r' - r) <= 2 || abs(c' - c) <= 2
        = []
intermediatePositions (r, c) (r', c') =
    let dr = (r' - r) `div` abs (r' - r)
        dc = (c' - c) `div` abs (c' - c)
        n = abs (r' - r) - 1
    in [(r + dr * i, c + dc * i) | i <- [1..n]]

intermediateTiles :: Field -> Position -> Position -> [Tile]
intermediateTiles field src dst =
    map (fieldGet field) (intermediatePositions src dst)

majorDiagonal :: Position -> [Position]
majorDiagonal (r, c) = [(r0 + i, c0 + i) | i <- [0..high]]
  where
    (r0, c0) = (max 0 (r - c), max 0 (c - r))
    high     = 7 - abs (r - c)

minorDiagonal :: Position -> [Position]
minorDiagonal (r, c) = [(r0 + i, c0 - i) | i <- [0..high]]
  where
    (r0, c0) = (max 0 (r - (7 - c)), 7 - max 0 ((7 - c) - r))
    high     = 7 - abs (r - (7 - c))

canCapture :: Tile -> Field -> Position -> Bool
canCapture (Piece player Man) field src =
    or $ do
        move <- [\(r, c) -> (r - 1, c - 1), \(r, c) -> (r - 1, c + 1),
                 \(r, c) -> (r + 1, c + 1), \(r, c) -> (r + 1, c - 1)]
        let aPos = move src
        let bPos = move aPos
        if not $ isOnField bPos
            then []
            else do
                let a = fieldGet field aPos
                let b = fieldGet field bPos
                [a `isTileOf` enemy player && b == Empty]
canCapture piece@(Piece player King) field src@(r, c) =
    or [isJust $ capture piece field src dst | dst <- turnCandidates]
  where
    turnCandidates = removeTooClose $ majorDiagonal src ++ minorDiagonal src
    removeTooClose = filter (\(r', c') -> 2 < abs (r' - r))

canStep :: Tile -> Field -> Position -> Bool
canStep (Piece player Man) field (row, col) =
    or [isOnField position && Empty == fieldGet field position
       | position <- [(row + dRow, col - 1), (row + dRow, col + 1)]]
  where
    dRow = frontDirection player
canStep (Piece player King) field src@(r, c) =
    any canStepTo stepCandidates
  where
    stepCandidates = filter (/= src) $ majorDiagonal src ++ minorDiagonal src
    canStepTo dst@(r', c') =
        let intermediate = intermediateTiles field src dst
        in abs (r' - r) == abs (c' - c) && all (== Empty) intermediate

captureTurns :: Field -> Player -> [Position]
captureTurns field player = do
    position <- [(row, col) | row <- [0..7], col <- [0..7]]
    let tile = fieldGet field position
    if not $ tile `isTileOf` player && canCapture tile field position
        then []
        else [position]

step :: Tile -> Field -> Position -> Position -> Maybe GameState
step (Piece player Man) field src@(r, c) dst@(r', c')
    | frontDirection player == r' - r && 1 == abs (c' - c) =
        Just $ GameState (enemy player) nextField turns
  where
    nextField = movePiece field src dst
    turns = captureTurns nextField (enemy player)
step (Piece player King) field src@(r, c) dst@(r', c')
    | abs (r' - r) == abs (c' - c)
    , all (== Empty) intermediate =
        Just $ GameState (enemy player) nextField turns
  where
    nextField = movePiece field src dst
    turns = captureTurns nextField (enemy player)
    intermediate = intermediateTiles field src dst
step _ _ _ _ = Nothing

capture :: Tile -> Field -> Position -> Position -> Maybe GameState
capture piece@(Piece player Man) field src@(r, c) dst@(r', c')
    | abs (r' - r) == abs (c' - c)
    , 2 == abs (r' - r)
    , fieldGet field captured `isTileOf` enemy player =
        let field'      = fieldSet field captured Empty
            nextField   = movePiece field' src dst
            canCapture' = canCapture piece nextField dst
            gameState
                | canCapture' = GameState player nextField [dst]
                | otherwise   = GameState (enemy player) nextField turns
              where
                turns = captureTurns nextField (enemy player)
        in Just gameState
  where
    captured = ((r + r') `div` 2, (c + c') `div` 2)
capture piece@(Piece player King) field src@(r, c) dst@(r', c')
    | abs (r' - r) == abs (c' - c)
    , all (isTileOfEnemyOrEmpty player) intermediate =
        let positions   = intermediatePositions src dst
            step f p    = fieldSet f p Empty
            field'      = foldl step field positions
            nextField   = movePiece field' src dst
            canCapture' = canCapture piece nextField dst
            gameState
                | canCapture' = GameState player nextField [dst]
                | otherwise   = GameState (enemy player) nextField turns
              where
                turns = captureTurns nextField (enemy player)
        in Just gameState
  where
    intermediate = intermediateTiles field src dst
    isTileOfEnemyOrEmpty player tile =
        tile == Empty || tile `isTileOf` enemy player
capture _ _ _ _ = Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@(Just _) y = x
orElse _ y          = y

stepOrCapture :: Tile -> Field -> Position -> Position -> Maybe GameState
stepOrCapture tile field src dst =
    step tile field src dst `orElse` capture tile field src dst

turn :: GameState -> Position -> Position -> Maybe GameState
turn (GameState player field positions) src dst
    | null positions || src `elem` positions
    , isOnField src
    , isOnField dst
    , srcTile <- fieldGet field src
    , srcTile `isTileOf` player
    , fieldGet field dst == Empty =
        case positions of
            [] -> stepOrCapture srcTile field src dst
            _  -> capture srcTile field src dst
turn _ _ _ = Nothing

getTurn :: GameState -> IO GameState
getTurn state = do
    putStr "> "
    hFlush stdout
    t <- readTurn <$> getLine
    case t of
        Nothing         -> invalidTurn
        Just (src, dst) ->
            case turn state src dst of
                Nothing     -> invalidTurn
                Just state' -> return state'
  where
    readTurn [a, b, c, d, e]
        | Just a' <- a `elemIndex` "abcdefgh"
        , b' < 8
        , '-' == c
        , Just d' <- d `elemIndex` "abcdefgh"
        , e' < 8 =
            Just ((b', a'), (e', d'))
      where
        b' = digitToInt b - 1
        e' = digitToInt e - 1
    readTurn _ = Nothing
    invalidTurn = do
        putStrLn "Invalid turn"
        getTurn state

countTiles :: Field -> (Int, Int)
countTiles field =
    foldl step (0, 0) tiles
  where
    tiles = [fieldGet field (row, col) | row <- [0..7], col <- [0..7]]
    step amounts Empty = amounts
    step (black, white) (Piece Black _) = (black + 1, white)
    step (black, white) (Piece White _) = (black, white + 1)

noTurns :: Player -> Field -> Bool
noTurns player field = all haveNoTurns allPositions
  where
    allPositions = [(row, col) | row <- [0..7], col <- [0..7]]
    haveNoTurns position =
        let piece = fieldGet field position
            canStep' = canStep piece field position
            canCapture' = canCapture piece field position
        in not $ piece `isTileOf` player && (canStep' || canCapture')

play :: GameState -> IO ()
play state@(GameState player field _) = do
    printField $ field
    let (black, white) = countTiles field
    case (black, white) of
        (0, _) -> putStrLn $ "White wins!"
        (_, 0) -> putStrLn $ "Black wins!"
        _      ->
            if noTurns player field
                then putStrLn $ show (enemy player) ++ " wins!"
                else do
                    putStrLn $ show player ++ "'s turn"
                    state' <- getTurn state
                    play state'

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    play (GameState White startingField [])
