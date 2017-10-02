import Data.Ord
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import System.IO

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

instance Show Field where
    show field =
        "  +-+-+-+-+-+-+-+-+\n"
        ++ concatFor [7, 6..0] (\i ->
            show (i + 1) ++ " |"
            ++ concatFor [0..7] (\j ->
                show (fieldGet field (i, j))
                ++ "|")
            ++ "\n")
        ++ "  +-+-+-+-+-+-+-+-+\n"
        ++ "   a b c d e f g h\n"
      where
        concatFor = flip concatMap

instance Show GameState where
    show (GameState player field obligatoryPositions) =
        show field
        ++ show player ++ "'s turn\n"
        ++ "obligatories: " ++ show obligatoryPositions ++ "\n"

startingField :: Field
startingField =
    Field (V.reverse $ V.fromList
        [ V.fromList [e, b, e, b, e, b, e, b]
        , V.fromList [b, e, b, e, b, e, b, e]
        , V.fromList [e, b, e, b, e, b, e, b]
        , V.fromList [e, e, e, e, e, e, e, e]
        , V.fromList [e, e, e, e, e, e, e, e]
        , V.fromList [w, e, w, e, w, e, w, e]
        , V.fromList [e, w, e, w, e, w, e, w]
        , V.fromList [w, e, w, e, w, e, w, e]
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
    | abs (r' - r) < 2 || abs(c' - c) < 2
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

captureTurns :: Field -> Player -> [Position]
captureTurns field player = do
    position <- [(row, col) | row <- [0..7], col <- [0..7]]
    let tile = fieldGet field position
    let canCapture = not $ null $ availableCaptures field position
    if not $ tile `isTileOf` player && canCapture
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
    , all isEmptyOrPlayers intermediate
    , fieldGet field dst == Empty =
        Just $ GameState (enemy player) nextField turns
  where
    isEmptyOrPlayers tile = tile `isTileOf` player || tile == Empty
    nextField = movePiece field src dst
    turns = captureTurns nextField (enemy player)
    intermediate = intermediateTiles field src dst
step _ _ _ _ = Nothing

capture :: Tile -> Field -> Position -> Position -> Maybe GameState
capture (Piece player Man) field src@(r, c) dst@(r', c')
    | abs (r' - r) == abs (c' - c)
    , 2 == abs (r' - r)
    , fieldGet field captured `isTileOf` enemy player
    , fieldGet field dst == Empty =
        let field'      = fieldSet field captured Empty
            nextField   = movePiece field' src dst
            canCapture' = not $ null $ availableCaptures nextField dst
            turns       = captureTurns nextField (enemy player)
            gameState
                | canCapture' = GameState player nextField [dst]
                | otherwise   = GameState (enemy player) nextField turns
        in Just gameState
  where
    captured = ((r + r') `div` 2, (c + c') `div` 2)
capture (Piece player King) field src@(r, c) dst@(r', c')
    | abs (r' - r) == abs (c' - c)
    , any (`isTileOf` enemy player) intermediate
    , all (isTileOfEnemyOrEmpty player) intermediate
    , fieldGet field dst == Empty =
        let positions   = intermediatePositions src dst
            eraseAt f p = fieldSet f p Empty
            field'      = foldl eraseAt field positions
            nextField   = movePiece field' src dst
            canCapture' = not $ null $ availableCaptures nextField dst
            turns       = captureTurns nextField (enemy player)
            gameState
                | canCapture' = GameState player nextField [dst]
                | otherwise   = GameState (enemy player) nextField turns
        in Just gameState
  where
    intermediate = intermediateTiles field src dst
    isTileOfEnemyOrEmpty player' tile =
        tile == Empty || tile `isTileOf` enemy player'
capture _ _ _ _ = Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@(Just _) _ = x
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
    foldl next (0, 0) tiles
  where
    tiles = [fieldGet field (row, col) | row <- [0..7], col <- [0..7]]
    next amounts Empty = amounts
    next (black, white) (Piece Black _) = (black + 1, white)
    next (black, white) (Piece White _) = (black, white + 1)

availableSteps :: Field -> Position -> [Position]
availableSteps field src@(row, col)
    | Piece player Man <- fieldGet field src =
        let dRow = frontDirection player
            canStepTo pos = isOnField pos && Empty == fieldGet field pos
        in filter canStepTo [(row + dRow, col - 1), (row + dRow, col + 1)]
availableSteps field src
    | Piece player King <- fieldGet field src =
        let emptyOrPlayers t = t `isTileOf` player || t == Empty
            diagonals = majorDiagonal src ++ minorDiagonal src
            stepCandidates = filter (/= src) diagonals
            canStepTo dst =
                let between = intermediateTiles field src dst
                in all emptyOrPlayers between && fieldGet field dst == Empty
        in filter canStepTo stepCandidates
availableSteps _ _ =
    error "availableSteps shouldn't be used on Empty tile"

middle :: Position -> Position -> Position
middle (r, c) (r', c') = ((r + r') `div` 2, (c + c') `div` 2)

testField :: Field
testField =
    Field $ V.reverse $ V.fromList
        [ V.fromList [ e, b, e, b, e, b, e, ww ],
          V.fromList [ b, e, b, e, b, e, e, e ],
          V.fromList [ e, b, e, b, e, e, e, b ],
          V.fromList [ w, e, e, e, b, e, e, e ],
          V.fromList [ e, e, e, e, e, e, e, e ],
          V.fromList [ w, e, w, e, w, e, e, e ],
          V.fromList [ e, e, e, w, e, w, e, w ],
          V.fromList [ w, e, w, e, w, e, w, e ]
        ]
  where
    e = Empty
    b = Piece Black Man
    w = Piece White Man
    ww = Piece White King

availableCaptures :: Field -> Position -> [Position]
availableCaptures field src@(r, c)
    | piece@(Piece _ Man) <- fieldGet field src =
        filter (\dst -> isJust $ capture piece field src dst) turnCandidates
  where
    turnCandidates =
        filter isOnField [(r - 2, c - 2), (r - 2, c + 2),
                          (r + 2, c + 2), (r + 2, c - 2)]
availableCaptures field src@(r, _)
    | piece@(Piece _ King) <- fieldGet field src =
        filter (\dst -> isJust $ capture piece field src dst) turnCandidates
  where
    turnCandidates = removeTooClose $ majorDiagonal src ++ minorDiagonal src
    removeTooClose = filter (\(r', _) -> 2 <= abs (r' - r))
availableCaptures _ _ =
    error "availableCaptures shouldn't be used on Empty tile"

availableTurns :: GameState -> [(Position, Position)]
availableTurns (GameState player field []) = do
    row <- [0..7]
    col <- [0..7]
    let position = (row, col)
    let piece = fieldGet field position
    let steps = availableSteps field position
    let captures = availableCaptures field position
    if piece `isTileOf` player
        then map ((,) position) $ steps ++ captures
        else []
availableTurns (GameState _ field obligatoryPositions) = do
    position <- obligatoryPositions
    map ((,) position) $ availableCaptures field position

maxDepth :: Int
maxDepth = 4

heuristic :: GameState -> Int
heuristic (GameState player field _) =
    foldl next 0 [(row, col) | row <- [0..7], col <- [0..7]]
  where
    next acc pos@(_, c)
        | (Piece player' Man) <- fieldGet field pos
        , player == player'
        , c `elem` [0, 7]
            = let steps = length $ availableSteps field pos
                  captures = length $ availableCaptures field pos
              in acc + steps + 2 * captures + 2
    next acc pos
        | (Piece player' Man) <- fieldGet field pos
        , player == player'
            = let steps = length $ availableSteps field pos
                  captures = length $ availableCaptures field pos
              in acc + steps + 2 * captures + 1
    next acc pos
        | (Piece player' King) <- fieldGet field pos
        , player == player'
            = let steps = length $ availableSteps field pos
                  captures = length $ availableCaptures field pos
              in acc + steps + 2 * captures + 5
    next acc _ = acc

assess :: Int -> Player -> Bool -> Int -> Int -> GameState -> Int
assess depth player maximizing alpha beta state =
    case (depth, winner state) of
        (0, _) -> heuristic state
        (_, Just player') ->
            if player' == player then maxBound else minBound
        _                   ->
            if maximizing
                then go minBound alpha childStates
                else go minBound alpha childStates
  where
    turn' (src, dst) = fromJust $ turn state src dst
    childStates = map turn' $ availableTurns state
    minOrMax = if maximizing then max else min
    go v _ [] = v
    go v alpha' _ | beta <= alpha' = v
    go v alpha' (state' : states) =
        let v' = assess (depth - 1) player (not maximizing) alpha' beta state'
        in go (minOrMax v v') (minOrMax alpha' v') states

aiNextStep :: GameState -> (Position, Position)
aiNextStep state@(GameState player _ []) =
    maximumBy (comparing assess') $ availableTurns state
  where
    alpha = minBound :: Int
    beta = maxBound :: Int
    assess' (src, dst) =
        let nextState = fromJust $ turn state src dst
        in assess maxDepth player False alpha beta nextState
aiNextStep state@(GameState player field obligatoryPositions) =
    maximumBy (comparing assess') turns
  where
    alpha = minBound :: Int
    beta = maxBound :: Int
    assess' (src, dst) =
        let nextState = fromJust $ turn state src dst
        in assess maxDepth player False alpha beta nextState
    availableCaptures' pos = map ((,) pos) $ availableCaptures field pos
    turns = concatMap availableCaptures' obligatoryPositions

winner :: GameState -> Maybe Player
winner state@(GameState player field _) =
    case countTiles field of
        (0, _)       -> Just White
        (_, 0)       -> Just Black
        _ | noTurns  -> Just $ enemy player
        _            -> Nothing
  where
    noTurns = null $ availableTurns state

type Actor = GameState -> IO GameState

humanActor :: GameState -> IO GameState
humanActor state = getTurn state

computerActor :: GameState -> IO GameState
computerActor state = do
    let (src, dst) = aiNextStep state
    return $ fromJust $ turn state src dst

play :: Actor -> Actor -> GameState -> IO ()
play black white state@(GameState player field _) = do
    print field
    case winner state of
        Just player' -> putStrLn $ show player' ++ " wins!"
        Nothing      -> do
            putStrLn $ show player ++ "'s turn"
            state' <- playerAction state
            play black white state'
  where
    playerAction = if Black == player then black else white

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    play humanActor computerActor (GameState White startingField [])
