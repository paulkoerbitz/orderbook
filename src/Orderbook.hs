{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Orderbook (
         Currency(..),
         Amount(..),
         Security(..),
         Quantity(..),
         Limit(..),
         OrderId(..),
         Order(..),
         OrderStatus(..),
         OrderData(..),
         OrderBook(..),
         empty,
         update,
         spread
       ) where

import qualified Data.Map as M
import           Control.Monad (when)
import           Data.Maybe (isJust, fromJust, isNothing)

data Currency = EUR deriving (Show, Ord, Eq)
data Amount   = Amount { amtSmallestUnit :: Int, amtCurrency :: Currency } deriving (Show, Ord, Eq)

liftNum :: (Int -> Int -> Int) -> (Amount -> Amount -> Amount)
liftNum f = \(Amount amtA curA) (Amount amtB curB) ->
  if curA /= curB then error "Amount: Currencies don't match" else (Amount (f amtA amtB) curA)

newtype Security = Security Int deriving (Show, Ord, Eq)
newtype Quantity = Quantity Int deriving (Show, Ord, Eq, Num)
newtype Limit    = Limit Amount deriving (Show, Ord, Eq)
newtype OrderId  = OrderId Int  deriving (Show, Ord, Eq)

incOId :: OrderId -> OrderId
incOId (OrderId i) = OrderId (i+1)

data OrderData = OrderData Security Quantity Limit

data Direction = DirBuy | DirSell

data Order = Buy OrderData
           | Sell OrderData
           | Cancel OrderId

type SingleObSide = M.Map Amount [(OrderId, Security, Quantity)]

data OrderBook = OrderBook OrderId        -- ^ The orderId issued to the next incomming order
                           (Maybe Amount) -- ^ The bid price, Nothing if there are no orders
                           (Maybe Amount) -- ^ The ask price, Nothing if there are no orders
                           SingleObSide   -- ^ The buy side of the order book
                           SingleObSide   -- ^ The sell side of the order book
               deriving (Eq, Show)


empty :: OrderBook
empty = OrderBook (OrderId 0) Nothing Nothing M.empty M.empty

data OrderStatus = Filled OrderId Security [(Amount, Quantity)]
                 | PartiallyFilled OrderId Security [(Amount, Quantity)]
                 | Pending OrderId
                 deriving (Eq, Show)

appendSOS :: OrderData -> OrderId -> SingleObSide -> SingleObSide
appendSOS (OrderData sec quant (Limit amt)) oId = M.alter updateOrders amt
  where
    newEntry     = (oId, sec, quant)
    updateOrders = Just . maybe [newEntry] (newEntry:)

-- This function was first written for buy orders and that is where a lot of names still come from
fillOrder :: Direction -> OrderData -> SingleObSide -> (Quantity, [(Amount, Quantity)], [OrderStatus], Maybe Amount, SingleObSide)
fillOrder dir (OrderData _ quant (Limit lmt)) = loop quant [] []
  where
    stopCond = case dir of { DirBuy -> (<); DirSell -> (>) }
    findDelete = case dir of { DirBuy -> M.deleteFindMin; DirSell -> M.deleteFindMax }

    loop qntToFill filled othersFilled sell =
      if qntToFill == 0 || M.null sell
      then done qntToFill filled othersFilled sell
      else let ((ask, sellOffers), sell') = findDelete sell
           in if stopCond lmt ask
              then done qntToFill filled othersFilled sell
              else let (qntToFill', newSellOffers, filledNow, othersFilledNow) = fill ask [] [] qntToFill sellOffers
                       newSellSide = if null newSellOffers then sell' else M.insert ask newSellOffers sell'
                   in loop qntToFill' (filledNow ++ filled) (othersFilledNow ++ othersFilled) newSellSide

    fill _   filled othersFilled 0         offers                        = (0, offers, reverse filled, reverse othersFilled)
    fill _   filled othersFilled qntToFill []                            = (qntToFill, [], reverse filled, reverse othersFilled)
    fill amt filled othersFilled qntToFill ((oid, sec, qntOffer):offers) =
      let qntFilledNow    = min qntToFill qntOffer
          qntOfferLeft    = qntOffer - qntFilledNow
          filledNow       = (amt, qntFilledNow)
          othersFilledNow = if qntOfferLeft == 0
                            then Filled oid sec [(amt, qntFilledNow)]
                            else PartiallyFilled oid sec [(amt, qntFilledNow)]
          offers'         = if qntOfferLeft == 0 then offers else (oid,sec,qntOfferLeft):offers
      in fill amt (filledNow:filled) (othersFilledNow:othersFilled) (qntToFill - qntFilledNow) offers'

    done qntToFill filled othersFilled sell =
      (quant - qntToFill, filled, othersFilled, if M.null sell then Nothing else Just . fst . M.findMin $ sell, sell)

processBuyOrSell :: Direction -> OrderData -> OrderBook -> Either String ([OrderStatus], OrderBook)
processBuyOrSell dir oData@(OrderData sec qntToFill (Limit lim)) (OrderBook nId bid ask buy sell) = do
  when (isJust ask && amtCurrency lim /= amtCurrency (fromJust ask)) $ error $ "Currency must be " ++ show (amtCurrency $ fromJust ask)
  if isJust otherSpot && lim `cmp` fromJust otherSpot
    then do let (qntFilled, filled, othersFilled, newOtherSpot, newOtherSide) = fillOrder dir oData otherSide
            if qntFilled == qntToFill
              then return $ ((Filled nId sec filled):othersFilled,
                             setOb thisSpot thisSide newOtherSpot newOtherSide)
              else let newThisSide = appendSOS (OrderData sec (qntToFill - qntFilled) (Limit lim)) nId thisSide
                       newThisSpot = if isNothing thisSpot || lim `cmp` fromJust thisSpot then Just lim else thisSpot
                   in return $ ((PartiallyFilled nId sec filled):othersFilled,
                                setOb newThisSpot newThisSide newOtherSpot newOtherSide)
    else let newThisSide = appendSOS oData nId thisSide
             newThisSpot = if isNothing thisSpot || lim `cmp` fromJust thisSpot then Just lim else thisSpot
         in return $ ([Pending nId], setOb newThisSpot newThisSide otherSpot otherSide)
  where
    (cmp, thisSpot, thisSide, otherSpot, otherSide) = case dir of
      DirBuy ->  ((>=), bid, buy, ask, sell)
      DirSell -> ((<=), ask, sell, bid, buy)
    setOb thisSpot' thisSide' otherSpot' otherSide' = case dir of
      DirBuy  -> OrderBook (incOId nId) thisSpot' otherSpot' thisSide' otherSide'
      DirSell -> OrderBook (incOId nId) otherSpot' thisSpot' otherSide' thisSide'

-- FIXME: Cancels
update :: Order -> OrderBook -> Either String ([OrderStatus], OrderBook)
update (Buy oData) ob = processBuyOrSell DirBuy oData ob
update (Sell oData) ob = processBuyOrSell DirSell oData ob
-- update (Cancel oId) _ = undefined

spread :: OrderBook -> Maybe Amount
spread (OrderBook _ obAsk obBid _ _) = do ask <- obAsk
                                          bid <- obBid
                                          return $ liftNum (-) ask bid
