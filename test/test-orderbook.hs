import Test.HUnit (assertBool)
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import qualified Orderbook as OB
import qualified Data.Map as M

main :: IO ()
main = defaultMain tests

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq expected actual = assertBool msg (expected == actual)
  where
    msg = "Expected: " ++ show expected ++ ", but got: " ++ show actual

data OType = Pending Int
           | Filled  Int Int [(Int,Int)]
           | Partial Int Int [(Int,Int)]

order :: OType -> OB.OrderStatus
order (Pending oid)                        = OB.Pending (OB.OrderId oid)
order (Filled  oid sec quantities)          = OB.Filled (OB.OrderId oid) (OB.Security sec) $ map (\(amt,qnt) -> (OB.Amount amt OB.EUR, OB.Quantity qnt)) quantities
order (Partial oid sec quantities) = OB.PartiallyFilled (OB.OrderId oid) (OB.Security sec) $ map (\(amt,qnt) -> (OB.Amount amt OB.EUR, OB.Quantity qnt)) quantities

tests :: [TF.Test]
tests = [ testGroup "Small known examples for Orderbook manipulation"
          [ testCase "Buy and sell a single item" $ assertEq
            (Right ([OB.Pending (OB.OrderId 0),OB.Filled (OB.OrderId 1) (OB.Security 0) [(OB.Amount {OB.amtSmallestUnit = 100, OB.amtCurrency = OB.EUR},OB.Quantity 123)],OB.Filled (OB.OrderId 0) (OB.Security 0) [(OB.Amount {OB.amtSmallestUnit = 100, OB.amtCurrency = OB.EUR},OB.Quantity 123)]],
                    OB.OrderBook (OB.OrderId 2) Nothing Nothing M.empty M.empty))
            (do (ords1, ob1) <- OB.update (OB.Buy (OB.OrderData (OB.Security 0) (OB.Quantity 123) (OB.Limit (OB.Amount 100 OB.EUR)))) OB.empty
                (ords2, ob2) <- OB.update (OB.Sell (OB.OrderData (OB.Security 0) (OB.Quantity 123) (OB.Limit (OB.Amount 100 OB.EUR)))) ob1
                return (ords1 ++ ords2, ob2))
          , testCase "Sell and buy a single item" $ assertEq
            (Right ([OB.Pending (OB.OrderId 0),OB.Filled (OB.OrderId 1) (OB.Security 0) [(OB.Amount {OB.amtSmallestUnit = 100, OB.amtCurrency = OB.EUR},OB.Quantity 123)],OB.Filled (OB.OrderId 0) (OB.Security 0) [(OB.Amount {OB.amtSmallestUnit = 100, OB.amtCurrency = OB.EUR},OB.Quantity 123)]],
                    OB.OrderBook (OB.OrderId 2) Nothing Nothing M.empty M.empty))
            (do (ords1, ob1) <- OB.update (OB.Sell (OB.OrderData (OB.Security 0) (OB.Quantity 123) (OB.Limit (OB.Amount 100 OB.EUR)))) OB.empty
                (ords2, ob2) <- OB.update (OB.Buy (OB.OrderData (OB.Security 0) (OB.Quantity 123) (OB.Limit (OB.Amount 100 OB.EUR)))) ob1
                return (ords1 ++ ords2, ob2))
          , testCase "Two buy and one sell order which is not completely filled" $ assertEq
            (Right (map order [Pending 0,
                               Pending 1,
                               Partial 2 7 [(100,80), (105, 120)],
                               Filled 0 7 [(100,  80)],
                               Filled 1 7 [(105, 120)]],
                    (OB.OrderBook (OB.OrderId 3) Nothing (Just (OB.Amount 90 OB.EUR)) M.empty
                     (M.fromList [(OB.Amount 90 OB.EUR, [(OB.OrderId 2, OB.Security 7, OB.Quantity 50)])]))))
            (do (ords1, ob1) <- OB.update (OB.Buy (OB.OrderData (OB.Security 7) (OB.Quantity 80) (OB.Limit (OB.Amount 100 OB.EUR)))) OB.empty
                (ords2, ob2) <- OB.update (OB.Buy (OB.OrderData (OB.Security 7) (OB.Quantity 120) (OB.Limit (OB.Amount 105 OB.EUR)))) ob1
                (ords3, ob3) <- OB.update (OB.Sell (OB.OrderData (OB.Security 7) (OB.Quantity 250) (OB.Limit (OB.Amount 90 OB.EUR)))) ob2
                return (ords1 ++ ords2 ++ ords3, ob3))
          , testCase "A buy, a sell, and a buy, middle sell order is not completely filled" $ assertEq
            (Right (map order [ Pending 0
                              , Partial 1 7 [(100, 80)]
                              , Filled  0 7 [(100, 80)]
                              , Filled  2 7 [(90, 120)]
                              , Partial 1 7 [(90, 120)]
                              ],
                    (OB.OrderBook (OB.OrderId 3) Nothing (Just (OB.Amount 90 OB.EUR)) M.empty
                     (M.fromList [(OB.Amount 90 OB.EUR, [(OB.OrderId 1, OB.Security 7, OB.Quantity 50)])]))))
            (do (ords1, ob1) <- OB.update (OB.Buy  (OB.OrderData (OB.Security 7) (OB.Quantity 80)  (OB.Limit (OB.Amount 100 OB.EUR)))) OB.empty
                (ords2, ob2) <- OB.update (OB.Sell (OB.OrderData (OB.Security 7) (OB.Quantity 250) (OB.Limit (OB.Amount 90 OB.EUR)))) ob1
                (ords3, ob3) <- OB.update (OB.Buy  (OB.OrderData (OB.Security 7) (OB.Quantity 120) (OB.Limit (OB.Amount 105 OB.EUR)))) ob2
                return (ords1 ++ ords2 ++ ords3, ob3))
          , testCase "A sell, a buy, and a sell, middle order is completely filled" $ assertEq
            (Right (map order [ Pending 0
                              , Partial 1 7 [(100,  80)]
                              , Filled  0 7 [(100,  80)]
                              , Filled  2 7 [(105, 120)]
                              , Filled  1 7 [(105, 120)]
                              ],
                    (OB.OrderBook (OB.OrderId 3) Nothing Nothing M.empty M.empty)))
            (do (ords1, ob1) <- OB.update (OB.Sell (OB.OrderData (OB.Security 7) (OB.Quantity 80)  (OB.Limit (OB.Amount 100 OB.EUR)))) OB.empty
                (ords2, ob2) <- OB.update (OB.Buy  (OB.OrderData (OB.Security 7) (OB.Quantity 200) (OB.Limit (OB.Amount 105 OB.EUR)))) ob1
                (ords3, ob3) <- OB.update (OB.Sell (OB.OrderData (OB.Security 7) (OB.Quantity 120) (OB.Limit (OB.Amount 90 OB.EUR)))) ob2
                return (ords1 ++ ords2 ++ ords3, ob3))
          ]
        ]
