{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.State.Lazy (MonadState (get, put), State, evalState)
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
import System.Random (RandomGen, initStdGen)
import System.Random.Shuffle (shuffle')

data Color = Red | Yellow | Green | Blue deriving (Enum, Eq, Ord)

-- Convert a Color to a Raylib color
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

type Deck = [Card]

data Model = Model
  { top :: Card,
    player :: Deck,
    cpu :: Deck,
    pile :: Deck
  }

class Draw a where
  draw :: (a, Float, Float) -> IO ()

cardWidth :: Float
cardWidth = 80

cardHeight :: Float
cardHeight = 120

cardBorder :: Float
cardBorder = 4

cardSpacing :: Float
cardSpacing = 50

-- Width of the window
width :: Int
width = 800

-- Height of the window
height :: Int
height = 600

playerCardY :: Float
playerCardY = fromIntegral height - 50 - cardHeight

pileCard :: Rectangle
pileCard = Rectangle (fromIntegral width / 2 - cardWidth / 2) (fromIntegral height / 2 - cardHeight / 2) cardWidth cardHeight

-- Draws the background of an uno card
drawCardBackground :: Raylib.Types.Color -> Float -> Float -> IO ()
drawCardBackground c x y =
  do
    drawRectangleRounded (Rectangle x y cardWidth cardHeight) 0.2 0 white
    drawRectangleRounded (Rectangle (x + cardBorder) (y + cardBorder) (cardWidth - 2 * cardBorder) (cardHeight - 2 * cardBorder)) 0.2 0 c

-- Draws the background of an uno wildcard
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
      drawCardBackground (getRlColor c) x y
      drawText (show n) (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Skip c, x, y) =
    do
      drawCardBackground (getRlColor c) x y
      drawRing (Vector2 (x + 5 * cardBorder) (y + 5 * cardBorder)) 10 13 0 360 0 black
      drawLineEx (Vector2 (x + 5 * cardBorder - 12) (y + 5 * cardBorder)) (Vector2 (x + 5 * cardBorder + 12) (y + 5 * cardBorder)) 3 black
  draw (Reverse c, x, y) =
    do
      drawCardBackground (getRlColor c) x y
      drawText "<=>" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Draw c, x, y) =
    do
      drawCardBackground (getRlColor c) x y
      drawText "+2" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Wild Nothing, x, y) =
    do
      drawWildcardBackground x y
      drawText "wild" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (Wild (Just c), x, y) =
    do
      drawCardBackground (getRlColor c) x y
      drawText "wild" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (WildDraw Nothing, x, y) =
    do
      drawWildcardBackground x y
      drawText "+4" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black
  draw (WildDraw (Just c), x, y) =
    do
      drawCardBackground (getRlColor c) x y
      drawText "+4" (round (x + 4 * cardBorder)) (round (y + 4 * cardBorder)) 36 black

-- Draws the back of a card
drawBlankCard :: Float -> Float -> IO ()
drawBlankCard = drawCardBackground black

listReplicate :: Int -> [a] -> [a]
listReplicate = concatMap . replicate

-- Creates all of the preset cards in a deck
allCards :: Deck
allCards =
  ([Red ..] >>= colorCards)
    ++ replicate 4 (Wild Nothing)
    ++ replicate 4 (WildDraw Nothing)
  where
    colorCards c =
      zero : listReplicate 2 special ++ normal
      where
        zero = Normal c Zero
        special = [Skip c, Reverse c, Draw c]
        normal = Normal c <$> [One ..]

-- Deals 7 cards
deal :: State Deck Deck
deal =
  get >>= \pile ->
    let (dealt, newPile) = splitAt 7 pile
     in put newPile >> pure dealt

-- Checks if a card is a valid starter card for the pile
isStarterCard :: Card -> Bool
isStarterCard (Normal _ _) = True
isStarterCard _ = False

getTopCard :: State Deck Card
getTopCard =
  get >>= \pile ->
    let top = fromMaybe (head pile) (find isStarterCard pile)
     in put (delete top pile) >> pure top

-- Creates the initial state by shuffling the cards and drawing for the players
mkState :: (RandomGen gen) => gen -> Model
mkState gen =
  evalState setup deck
  where
    deck = shuffle' allCards (length allCards) gen
    setup = do
      top <- getTopCard
      player <- deal
      cpu <- deal
      pile <- get
      return Model {top, player, cpu, pile}

genInitialState :: IO Model
genInitialState = mkState <$> initStdGen

-- Checks if a card in the player's deck was clicked and issues an update
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

-- Checks if the pile was clicked and issues an update
pileCardUpdate :: Vector2 -> Update
pileCardUpdate mouse = toUpdate (checkCollisionPointRec mouse pileCard) Pickup

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a = Just a

toUpdate :: Bool -> Update -> Update
toUpdate False _ = None
toUpdate True a = a

-- Gets the color of a card
getColor :: Card -> Maybe Color
getColor (Normal c _) = Just c
getColor (Skip c) = Just c
getColor (Reverse c) = Just c
getColor (Draw c) = Just c
getColor (Wild c) = c
getColor (WildDraw c) = c

-- Gets the number of a card
getNumber :: Card -> Maybe Number
getNumber (Normal _ n) = Just n
getNumber _ = Nothing

-- Counts the total of each color of card in the deck
countColors :: Deck -> Map.Map Color Int
countColors [] = Map.empty
countColors (card : cards) = case getColor card of
  Just color -> snd $ Map.insertLookupWithKey (\_ a b -> a + b) color 1 (countColors cards)
  Nothing -> countColors cards

-- Finds all valid moves in a deck given the top card
allMoves :: Card -> Deck -> Deck
allMoves = filter . isValidMove

-- Finds the best move with a card of the given color
colorMove :: Deck -> Color -> Maybe Card
colorMove [] _ = Nothing
colorMove ((Wild _) : xs) c = colorMove xs c
colorMove ((WildDraw _) : xs) c = colorMove xs c
colorMove (x : xs) c
  | c `elem` getColor x = Just x
  | otherwise = colorMove xs c

-- Finds the best move given a descending list of colors ordered by count
priorityMove :: Deck -> [Color] -> Maybe Card
priorityMove moves = firstJust (colorMove moves)

-- Finds a wildcard to be played and sets the color to the ideal color
-- given a descending list of colors ordered by count
wildMove :: Deck -> [Color] -> Maybe Card
wildMove [] _ = Nothing
wildMove ((Wild _) : _) (color : _) = Just (Wild (Just color))
wildMove ((Wild _) : _) [] = Just (Wild (Just Red))
wildMove ((WildDraw _) : _) (color : _) = Just (WildDraw (Just color))
wildMove ((WildDraw _) : _) [] = Just (WildDraw (Just Red))
wildMove (_ : xs) colors = wildMove xs colors

{-

1. Try to play a number, skip, reverse, or draw card from the color we have the most cards of.
2. If no such move exists, try each color in order of how many cards we have of that color.
3. If there's still no valid move, play a +4 and change the color to the one we have the most of.
4. If we don't have a +4, play a wildcard and change the color to the one we have the most of.
5. Pick up a card if no move is possible
-}

-- Draws a card for the computer and updates the state
computerDrawCard :: Model -> Model
computerDrawCard s@Model {pile = (pickup : newPile)} =
  s
    { cpu = pickup : s.cpu,
      pile = newPile
    }
computerDrawCard s@Model {pile = []} = s

-- Gets the computer's next move if possible
computerGetMove :: Card -> Deck -> Maybe Card
computerGetMove top deck = firstJust (\f -> f moves idealColor) [priorityMove, wildMove]
  where
    moves = allMoves top deck
    idealColor = fst <$> sortBy (\(_, a) (_, b) -> compare b a) (Map.toList (countColors deck))

-- Either plays the best move or picks up if no valid move exists
computerPlay :: Model -> Model
computerPlay s = case computerGetMove s.top s.cpu of
  Just card -> computerPlayCard s card
  Nothing -> computerDrawCard s

-- Plays a card and plays another turn if needed
-- Precondition: Card must be playable
computerPlayCard :: Model -> Card -> Model
computerPlayCard s card@(Normal _ _) =
  s
    { top = card,
      cpu = delete card s.cpu
    }
computerPlayCard s card@(Skip _) =
  computerPlay
    s
      { top = card,
        cpu = delete card s.cpu
      }
computerPlayCard s card@(Reverse _) =
  computerPlay
    s
      { top = card,
        cpu = delete card s.cpu
      }
computerPlayCard s card@(Draw _) =
  let (drawn, newPile) = splitAt 2 s.pile
   in Model
        { top = card,
          player = drawn ++ s.player,
          cpu = delete card s.cpu,
          pile = newPile
        }
computerPlayCard s card@(Wild (Just _)) =
  s
    { top = card,
      cpu = delete (Wild Nothing) s.cpu
    }
computerPlayCard s card@(WildDraw (Just _)) =
  let (drawn, newPile) = splitAt 4 s.pile
   in computerPlay
        Model
          { top = card,
            player = drawn ++ s.player,
            cpu = delete (WildDraw Nothing) s.cpu,
            pile = newPile
          }
computerPlayCard _ (Wild Nothing) =
  error "computer played wild with no color"
computerPlayCard _ (WildDraw Nothing) =
  error "computer played wilddraw with no color"

-- Plays a card and runs the computer's move if needed
-- Precondition: Card must be playable
playCard :: Model -> Card -> Model
playCard s card@(Normal _ _) =
  computerPlay
    s
      { top = card,
        player = delete card s.player
      }
playCard s card@(Skip _) =
  s
    { top = card,
      player = delete card s.player
    }
playCard s card@(Reverse _) =
  s
    { top = card,
      player = delete card s.player
    }
playCard s card@(Draw _) =
  let (drawn, newPile) = splitAt 2 s.pile
   in computerPlay
        Model
          { top = card,
            player = delete card s.player,
            cpu = drawn ++ s.cpu,
            pile = newPile
          }
playCard s card@(Wild (Just _)) =
  computerPlay
    s
      { top = card,
        player = delete (Wild Nothing) s.player
      }
playCard s card@(WildDraw (Just _)) =
  let (drawn, newPile) = splitAt 4 s.pile
   in Model
        { top = card,
          player = delete (WildDraw Nothing) s.player,
          cpu = drawn ++ s.cpu,
          pile = newPile
        }
playCard _ (Wild Nothing) =
  error "player played wild with no color"
playCard _ (WildDraw Nothing) =
  error "player played wilddraw with no color"

-- Runs an update and updates the model
update :: Model -> Update -> Either Failure Model
update s None = Right s
update s (Play card)
  | isValidMove s.top card = Right (playCard s card)
  | otherwise = Left InvalidMove
update s@Model {pile = (pickup : newPile)} Pickup =
  Right
    ( computerPlay
        s
          { player = pickup : s.player,
            pile = newPile
          }
    )
update s@Model {pile = []} Pickup =
  Right s

-- Returns true if the second card can be played on the first
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

-- Run every update until a failure is encountered or all finish
runUpdates :: Model -> [Update] -> Either Failure Model
runUpdates s [] = Right s
runUpdates s (x : xs) = case update s x of
  Left failure -> Left failure
  Right next -> runUpdates next xs

-- Draws all graphics, checks for mouse updates, and updates the state
mainLoop :: Model -> IO Model
mainLoop s@Model {player = []} = do
  beginDrawing
  clearBackground rayWhite
  drawText "You Win!" 50 50 24 black
  endDrawing
  return s
mainLoop s@Model {cpu = []} = do
  beginDrawing
  clearBackground rayWhite
  drawText "You Lose!" 50 50 24 black
  endDrawing
  return s
mainLoop s@Model {pile = []} = do
  beginDrawing
  clearBackground rayWhite
  drawText "It's a tie!" 50 50 24 black
  endDrawing
  return s
mainLoop s = do
  let playerCardLocations = (\(card, x) -> (card, x, playerCardY)) <$> zip s.player [cardSpacing, (cardSpacing * 2) ..]

  beginDrawing

  clearBackground rayWhite

  drawBlankCard pileCard.rectangle'x pileCard.rectangle'y
  draw (s.top, fromIntegral width / 2 + cardWidth / 2 + 10, fromIntegral height / 2 - cardHeight / 2)
  mapM_ (\(_, offset) -> drawBlankCard (offset + 40) 50) (zip s.cpu [0, cardSpacing ..])
  mapM_ draw playerCardLocations

  endDrawing

  click <- isMouseButtonPressed MouseButtonLeft
  mousePosition <- getMousePosition

  let updates =
        [ toMaybe click (deckCardUpdate mousePosition playerCardLocations),
          toMaybe click (pileCardUpdate mousePosition)
        ]

  case runUpdates s (catMaybes updates) of
    Left _ -> return s
    Right newState -> return newState

-- Opens the window and runs the main loop
main :: IO ()
main = do
  window <- initWindow width height "Uno"
  setTargetFPS 60
  s <- genInitialState
  whileWindowOpen_ mainLoop s
  closeWindow window
