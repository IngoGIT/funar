{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)
{-
data DealHandsCommand = DealHands (Map Player Hand)

data PlayCardCommand = PlayCard Player Card

executeDealHandsCommand (DealHands map) = ...

executeDealHandsCommand (DealHands map)
-}

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsValidCard Player Card (Bool -> Game a)
  | TrickTakenBy (Maybe (Player, Trick) -> Game a)
  | GetNextPlayer Player (Player -> Game a)
  | IsGameFinished (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isValidCardM :: Player -> Card -> Game Bool
isValidCardM player card = IsValidCard player card Return

trickTakenByM :: Game (Maybe (Player, Trick))
trickTakenByM = TrickTakenBy Return

getNextPlayerM :: Player -> Game Player
getNextPlayerM player = GetNextPlayer player Return

isGameFinishedM :: Game (Maybe Player)
isGameFinishedM = IsGameFinished Return

-- getCommandM :: Game (Game GameCommand)
-- getCommandM = GetCommand Return
 
instance Functor Game where

instance Applicative Game where
  pure :: a -> Game a
  pure = Return

instance Monad Game where
  (>>=) :: Game a -> (a -> Game b) -> Game b
  (>>=) (RecordEvent event callback) next =
    RecordEvent event (\() -> callback() >>= next)
  (>>=) (IsValidCard player card  callback) next =
    IsValidCard player card
        (\valid -> callback valid >>= next)
  (>>=) (TrickTakenBy callback) next =
    TrickTakenBy (\ takerTrick -> callback takerTrick >>= next)
  (>>=) (GetNextPlayer player callback) next =
    GetNextPlayer player (\ nextPlayer -> callback nextPlayer >>= next)
  (>>=) (IsGameFinished callback) next =
    IsGameFinished (\ player -> callback player >>= next)
  (>>=) (GetCommand callback) next =
    GetCommand (\ command -> callback command >>= next)
  (>>=) (Return result) next = next result



-- Event, wenn ein Spieler eine Karte spielt *ein* Command, Ergebnis Gewinner, falls Spiel zu Ende
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hand) =
    -- HandDealt-Events verzeichnen
    do let pairs = Map.toList hand
       let events = map (uncurry HandDealt) pairs
       mapM_ recordEventM events
       return Nothing --Spiel noch nicht zu ende
tableProcessCommandM (PlayCard player card) =
    do valid <- isValidCardM player card
       if valid
       then do recordEventM (LegalCardPlayed player card)
               takerTrick <- trickTakenByM
               case takerTrick of
                Just (taker, trick) -> do
                    recordEventM (TrickTaken taker trick)
                    maybePlayer <- isGameFinishedM
                    case maybePlayer of
                        Just winner -> do
                            recordEventM (GameEnded winner)
                            return (Just winner)
                        Nothing -> do
                            recordEventM (PlayerTurnChanged taker)
                            return Nothing
                Nothing -> do
                    nextPlayer <- getNextPlayerM player
                    recordEventM (PlayerTurnChanged nextPlayer)
                    return Nothing
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing


tableLoopM :: GameCommand -> Game Player
tableLoopM command =
    do maybeWinner <- tableProcessCommandM command
       case maybeWinner of
        Just winner -> return winner
        Nothing ->
          GetCommand tableLoopM
