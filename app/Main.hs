{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List (find, sortBy)
import Data.List.Extra (delete, firstJust)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Raylib.Core (beginDrawing, clearBackground, closeWindow, endDrawing, getMousePosition, initWindow, isMouseButtonPressed, setTargetFPS)
import Raylib.Core.Shapes (checkCollisionPointRec, drawLineEx, drawRectangleRec, drawRectangleRounded, drawRing)
import Raylib.Core.Text (drawText)
import Raylib.Types (MouseButton (MouseButtonLeft), Rectangle (Rectangle, rectangle'x, rectangle'y), Vector2 (Vector2))
import Raylib.Types qualified (Color)
import Raylib.Util (whileWindowOpen_)
import Raylib.Util.Colors (black, blue, green, rayWhite, red, white, yellow)
import System.Random (initStdGen)
import System.Random.Shuffle (shuffle')

data Color = Red | Yellow | Green | Blue deriving (Enum, Eq, Ord)

getRlColor :: Main.Color -> Raylib.Types.Color
getRlColor Red = red
getRlColor Yellow = yellow
getRlColor Green = green
getRlColor Blue = blue

data Number = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Enum, Eq)

instance Show Number where
  show Zero = "0"
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"

data Card
  = Normal Color Number
  | Skip Color
  | Reverse Color
  | Draw Color
  | Wild (Maybe Color)
  | WildDraw (Maybe Color)
  deriving (Eq)

data Update
  = None
  | Play Card
  | Pickup

data Failure
  = InvalidMove

cardWidth :: Float
cardWidth = 80

cardHeight :: Float
cardHeight = 120

cardBorder :: Float
cardBorder = 4

cardSpacing :: Float
cardSpacing = 50

playerCardY :: Float
playerCardY = fromIntegral height - 50 - cardHeight

pileCard :: Rectangle
pileCard = Rectangle (fromIntegral width / 2 - cardWidth / 2) (fromIntegral height / 2 - cardHeight / 2) cardWidth cardHeight

drawCardBackground :: Float -> Float -> Raylib.Types.Color -> IO ()
drawCardBackground x y c =
  do
    drawRectangleRounded (Rectangle x y cardWidth cardHeight) 0.2 0 white
    drawRectangleRounded (Rectangle (x + cardBorder) (y + cardBorder) (cardWidth - 2 * cardBorder) (cardHeight - 2 * cardBorder)) 0.2 0 c

drawWildcardBackground :: Float -> Float -> IO ()
drawWildcardBackground x y =
  do
    drawRectangleRec (Rectangle x y (cardWidth / 2) (cardHeight / 2)) red
    drawRectangleRec (Rectangle (x + cardWidth / 2) y (cardWidth / 2) (cardHeight / 2)) yellow
    drawRectangleRec (Rectangle x (y + cardHeight / 2) (cardWidth / 2) (cardHeight / 2)) green
    drawRectangleRec (Rectangle (x + cardWidth / 2) (y + cardHeight / 2) (cardWidth / 2) (cardHeight / 2)) blue

instance Draw Card where
  draw (Normal c n, x, y) =
    do
      drawCardBackground x y (getRlColor c)
      drawText (show n) (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Skip c, x, y) =
    do
      drawCardBackground x y (getRlColor c)
      drawRing (Vector2 (x + 5 * cardBorder) (y + 5 * cardBorder)) 10 13 0 360 0 black
      drawLineEx (Vector2 (x + 5 * cardBorder - 12) (y + 5 * cardBorder)) (Vector2 (x + 5 * cardBorder + 12) (y + 5 * cardBorder)) 3 black
  draw (Reverse c, x, y) =
    do
      drawCardBackground x y (getRlColor c)
      drawText "<=>" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Draw c, x, y) =
    do
      drawCardBackground x y (getRlColor c)
      drawText "+2" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Wild Nothing, x, y) =
    do
      drawWildcardBackground x y
      drawText "wild" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Wild (Just c), x, y) =
    do
      drawCardBackground x y (getRlColor c)
      drawText "wild" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (WildDraw Nothing, x, y) =
    do
      drawWildcardBackground x y
      drawText "+4" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (WildDraw (Just c), x, y) =
    do
      drawCardBackground x y (getRlColor c)
      drawText "+4" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black

drawBlankCard :: Float -> Float -> IO ()
drawBlankCard x y = drawCardBackground x y black

type Deck = [Card]

data Model = Model Card Deck Deck Deck

class Draw a where
  draw :: (a, Float, Float) -> IO ()

allCards :: Deck
allCards =
  ((\c -> Normal c Zero : concat (replicate 2 ([Skip c, Reverse c, Draw c] ++ (Normal c <$> enumFrom One)))) =<< enumFrom Red)
    ++ replicate 4 (Wild Nothing)
    ++ replicate 4 (WildDraw Nothing)

deal :: Deck -> (Deck, Deck)
deal = splitAt 7

isStarterCard :: Card -> Bool
isStarterCard (Normal _ _) = True
isStarterCard _ = False

genInitialState :: IO Model
genInitialState =
  do
    gen <- initStdGen
    let pile = shuffle' allCards (length allCards) gen
    let top = fromMaybe (head pile) (find isStarterCard pile)
    let pile2 = delete top pile
    let (player, pile3) = deal pile2
    let (computer, pile4) = deal pile3

    return (Model top player computer pile4)

deckCardUpdate :: Vector2 -> [(Card, Float, Float)] -> Update
deckCardUpdate mouse cards = fromMaybe None (firstJust f cards)
  where
    f (Wild _, x, y) =
      let redRect = Rectangle x y (cardWidth / 2) (cardHeight / 2)
          yellowRect = Rectangle (x + cardWidth / 2) y (cardWidth / 2) (cardHeight / 2)
          greenRect = Rectangle x (y + cardHeight / 2) (cardWidth / 2) (cardHeight / 2)
          blueRect = Rectangle (x + cardWidth / 2) (y + cardHeight / 2) (cardWidth / 2) (cardHeight / 2)
          rects = [(redRect, Red), (yellowRect, Yellow), (greenRect, Green), (blueRect, Blue)]
       in firstJust (\(rect, color) -> toMaybe (checkCollisionPointRec mouse rect) (Play (Wild (Just color)))) rects
    f (WildDraw _, x, y) =
      let redRect = Rectangle x y (cardWidth / 2) (cardHeight / 2)
          yellowRect = Rectangle (x + cardWidth / 2) y (cardWidth / 2) (cardHeight / 2)
          greenRect = Rectangle x (y + cardHeight / 2) (cardWidth / 2) (cardHeight / 2)
          blueRect = Rectangle (x + cardWidth / 2) (y + cardHeight / 2) (cardWidth / 2) (cardHeight / 2)
          rects = [(redRect, Red), (yellowRect, Yellow), (greenRect, Green), (blueRect, Blue)]
       in firstJust (\(rect, color) -> toMaybe (checkCollisionPointRec mouse rect) (Play (WildDraw (Just color)))) rects
    f (card, x, y) =
      let rect = Rectangle x y cardSpacing cardHeight
       in toMaybe (checkCollisionPointRec mouse rect) (Play card)

pileCardUpdate :: Vector2 -> Update
pileCardUpdate mouse = toUpdate (checkCollisionPointRec mouse pileCard) Pickup

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a

toUpdate :: Bool -> Update -> Update
toUpdate False _ = None
toUpdate True a = a

getColor :: Card -> Maybe Color
getColor (Normal c _) = Just c
getColor (Skip c) = Just c
getColor (Reverse c) = Just c
getColor (Draw c) = Just c
getColor (Wild c) = c
getColor (WildDraw c) = c

getNumber :: Card -> Maybe Number
getNumber (Normal _ n) = Just n
getNumber _ = Nothing

countColors :: Deck -> Map.Map Color Int
countColors [] = Map.empty
countColors (card : cards) = case getColor card of
  Just color -> snd $ Map.insertLookupWithKey (\_ a b -> a + b) color 1 (countColors cards)
  Nothing -> countColors cards

allMoves :: Card -> Deck -> Deck
allMoves top = filter (isValidMove top)

-- given a list of available moves and the priority of colors, pick the best move or return Nothing

colorMove :: Deck -> Color -> Maybe Card
colorMove [] _ = Nothing
colorMove ((Wild _) : xs) c = colorMove xs c
colorMove ((WildDraw _) : xs) c = colorMove xs c
colorMove (x : xs) c
  | all (== c) (getColor x) = Just x
  | otherwise = colorMove xs c

priorityMove :: Deck -> [Color] -> Maybe Card
priorityMove moves = firstJust (colorMove moves)

wildMove :: Deck -> [Color] -> Maybe Card
wildMove [] _ = Nothing
wildMove ((Wild _) : _) colors = Just (Wild (Just (head colors)))
wildMove ((WildDraw _) : _) colors = Just (WildDraw (Just (head colors)))
wildMove (_ : xs) colors = wildMove xs colors

{-

1. Try to play a number, skip, reverse, or draw card from the color we have the most cards of.
2. If no such move exists, try each color in order of how many cards we have of that color.
3. If there's still no valid move, play a +4 and change the color to the one we have the most of.
4. If we don't have a +4, play a wildcard and change the color to the one we have the most of.

-}

-- computerPlayCard :: Model -> Card -> Model
-- computerPlayCard (Model top player computer pile) card =
--   let (Model newTop newComputer newPlayer newPile) = playCard (Model top computer player pile) card
--    in Model newTop newPlayer newComputer newPile

computerPlayCard :: Model -> Card -> Model
computerPlayCard (Model _ player computer pile) card@(Normal _ _) =
  Model card player (delete card computer) pile
computerPlayCard (Model _ player computer pile) card@(Skip _) =
  let state = Model card player (delete card computer) pile
   in computerPlay state
computerPlayCard (Model _ player computer pile) card@(Reverse _) =
  let state = Model card player (delete card computer) pile
   in computerPlay state
computerPlayCard (Model _ player computer pile) card@(Draw _) =
  let (drawn, newPile) = splitAt 2 pile
   in Model card (drawn ++ player) (delete card computer) newPile
computerPlayCard (Model _ player computer pile) (Wild (Just c)) =
  Model (Wild (Just c)) player (delete (Wild Nothing) computer) pile
computerPlayCard (Model _ player computer pile) (WildDraw (Just c)) =
  let (drawn, newPile) = splitAt 4 pile
      state = Model (WildDraw (Just c)) (drawn ++ player) (delete (WildDraw Nothing) computer) newPile
   in computerPlay state
computerPlayCard _ (Wild Nothing) =
  error "computer played wild with no color"
computerPlayCard _ (WildDraw Nothing) =
  error "computer played wilddraw with no color"

computerDrawCard :: Model -> Model
computerDrawCard (Model top player computer (pickup : pile)) = Model top player (pickup : computer) pile
computerDrawCard (Model top player computer []) = Model top player computer []

computerGetMove :: Card -> Deck -> Maybe Card
computerGetMove top deck = listToMaybe $ catMaybes [priorityMove moves idealColor, wildMove moves idealColor]
  where
    moves = allMoves top deck
    idealColor = map fst $ sortBy (\(_, a) (_, b) -> compare b a) (Map.toList (countColors deck))

computerPlay :: Model -> Model
computerPlay state@(Model top _ computer _) = case computerGetMove top computer of
  Just card -> computerPlayCard state card
  Nothing -> computerDrawCard state

-- Precondition: Card must be playable
playCard :: Model -> Card -> Model
playCard (Model _ player computer pile) card@(Normal _ _) =
  let state = Model card (delete card player) computer pile
   in computerPlay state
playCard (Model _ player computer pile) card@(Skip _) =
  Model card (delete card player) computer pile
playCard (Model _ player computer pile) card@(Reverse _) =
  Model card (delete card player) computer pile
playCard (Model _ player computer pile) card@(Draw _) =
  let (drawn, newPile) = splitAt 2 pile
      state = Model card (delete card player) (drawn ++ computer) newPile
   in computerPlay state
playCard (Model _ player computer pile) (Wild (Just c)) =
  let state = Model (Wild (Just c)) (delete (Wild Nothing) player) computer pile
   in computerPlay state
playCard (Model _ player computer pile) (WildDraw (Just c)) =
  let (drawn, newPile) = splitAt 4 pile
   in Model (WildDraw (Just c)) (delete (WildDraw Nothing) player) (drawn ++ computer) newPile
playCard _ (Wild Nothing) =
  error "player played wild with no color"
playCard _ (WildDraw Nothing) =
  error "player played wilddraw with no color"

update :: Model -> Update -> Either Failure Model
update state None = Right state
update state@(Model top _ _ _) (Play card)
  | isValidMove top card = Right (playCard state card)
  | otherwise = Left InvalidMove
update (Model top player computer (pickup : pile)) Pickup = Right (computerPlay (Model top (pickup : player) computer pile))
update (Model top player computer []) Pickup = Right (Model top player computer [])

isValidMove :: Card -> Card -> Bool
isValidMove _ (Wild _) = True
isValidMove _ (WildDraw _) = True
isValidMove (Skip _) (Skip _) = True
isValidMove (Reverse _) (Reverse _) = True
isValidMove (Draw _) (Draw _) = True
isValidMove a b = matchingColor || matchingNumber
  where
    matchingColor = case (getColor a, getColor b) of
      (Just c1, Just c2) -> c1 == c2
      _ -> False
    matchingNumber = case (getNumber a, getNumber b) of
      (Just c1, Just c2) -> c1 == c2
      _ -> False

runUpdates :: Model -> [Update] -> Either Failure Model
runUpdates state [] = Right state
runUpdates state (x : xs) = case update state x of
  Left failure -> Left failure
  Right next -> runUpdates next xs

width :: Int
width = 800

height :: Int
height = 600

mainLoop :: Model -> IO Model
mainLoop state@(Model _ [] _ _) = do
  beginDrawing
  clearBackground rayWhite
  drawText "You Win!" 50 50 24 black
  endDrawing
  pure state
mainLoop state@(Model _ _ [] _) = do
  beginDrawing
  clearBackground rayWhite
  drawText "You Lose!" 50 50 24 black
  endDrawing
  pure state
mainLoop state@(Model _ _ _ []) = do
  beginDrawing
  clearBackground rayWhite
  drawText "It's a tie!" 50 50 24 black
  endDrawing
  pure state
mainLoop state@(Model top player computer _) = do
  let playerCardLocations = (\(card, x) -> (card, x, playerCardY)) <$> zip player [cardSpacing, (cardSpacing * 2) ..]
  beginDrawing

  clearBackground rayWhite

  drawBlankCard (rectangle'x pileCard) (rectangle'y pileCard)
  draw (top, fromIntegral width / 2 + cardWidth / 2 + 10, fromIntegral height / 2 - cardHeight / 2)
  mapM_ (\(_, offset) -> drawBlankCard (offset + 40) 50) (zip computer [0, cardSpacing ..])
  mapM_ draw playerCardLocations

  endDrawing

  click <- isMouseButtonPressed MouseButtonLeft
  mousePosition <- getMousePosition

  let updates =
        [ toMaybe click (deckCardUpdate mousePosition playerCardLocations),
          toMaybe click (pileCardUpdate mousePosition)
        ]

  case runUpdates state (catMaybes updates) of
    Left _ -> return state
    Right newState -> return newState

main :: IO ()
main = do
  initWindow width height "Uno"
  setTargetFPS 60

  genInitialState >>= whileWindowOpen_ mainLoop

  closeWindow
