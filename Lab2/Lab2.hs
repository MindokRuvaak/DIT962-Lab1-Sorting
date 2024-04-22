import Control.Applicative
import Data.Maybe (fromJust, isJust)
import PriorityQueue.PrioSkew
import System.Environment
import System.IO

-- | Bids.
data Bid
  = Buy Person Price -- Person offers to buy share
  | Sell Person Price -- Person offers to sell share
  | NewBuy Person Price Price -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid
  deriving (Show)

type Person = String

type Price = Integer

-- | Buy and sell bids
data BuyOrder = BuyOrder Person Price deriving (Show)

instance Eq BuyOrder where
  (BuyOrder name1 price1) == (BuyOrder name2 price2) = name1 == name2 && price1 == price2

instance Ord BuyOrder where
  compare (BuyOrder _ price1) (BuyOrder _ price2) = compare price1 price2

buyingPrice :: BuyOrder -> Price
buyingPrice (BuyOrder _ p) = p

data SellOrder = SellOrder Person Price deriving (Show)

instance Eq SellOrder where
  (SellOrder name1 price1) == (SellOrder name2 price2) = name1 == name2 && price1 == price2

instance Ord SellOrder where
  compare (SellOrder _ price1) (SellOrder _ price2) = compare price2 price1

sellingPrice :: SellOrder -> Price
sellingPrice (SellOrder _ p) = p

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').
parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K", Just [price]) -> Right (Buy name price)
      ("S", Just [price]) -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
    readInteger :: String -> Maybe Integer
    readInteger s = case filter (null . snd) $ reads s of
      [(x, _)] -> Just x
      _ -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.
parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
    check (Left bid) = do
      hPutStrLn stderr $ "Malformed bid: " ++ bid
      return []
    check (Right bid) = return [bid]

-- | The main function of the program.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> process stdin
    [f] -> process =<< openFile f ReadMode
    _ ->
      hPutStr stderr $
        unlines
          [ "Usage: ./Lab2 [<file>]",
            "If no file is given, then input is read from standard input."
          ]
  where
    process h = trade =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.
trade :: [Bid] -> IO ()
trade = performTrades (OrderBook (emptySkewHeap, emptySkewHeap)) 

newtype OrderBook = OrderBook (SkewHeap BuyOrder, SkewHeap SellOrder)

instance Show OrderBook where
  show (OrderBook (b, s)) = "Order book:\n" ++ "Sellers: " ++ show s ++ "\n" ++ "Buyers: " ++ show b ++ "\n"

-- | the algoithm for trading given the list of bids
performTrades :: OrderBook -> [Bid] -> IO ()
performTrades oB [] = print oB
performTrades oB (bid : bids) = do
  if orderCheck tempOB
    then do
      nOB <- doTrade tempOB
      performTrades nOB bids
    else performTrades tempOB bids
  where
    tempOB = addBid oB bid


-- please try to improve, this feels bad but I can't figure out anything better ;_;
addBid :: OrderBook -> Bid -> OrderBook
addBid (OrderBook (buyOrders, sellOrders)) bid = case bid of
  Buy person price ->
    let newBuyOrder = BuyOrder person price
     in OrderBook (insert newBuyOrder buyOrders, sellOrders)
  Sell person price ->
    let newSellOrder = SellOrder person price
     in OrderBook (buyOrders, insert newSellOrder sellOrders)
  NewBuy person oldPrice newPrice ->
    let (oldBuy, newBuy) = (BuyOrder person oldPrice, BuyOrder person newPrice)
     in OrderBook (reNewOrder buyOrders oldBuy newBuy, sellOrders)
  NewSell person oldPrice newPrice ->
    let (oldSell, newSell) = (SellOrder person oldPrice, SellOrder person newPrice)
     in OrderBook (buyOrders, reNewOrder sellOrders oldSell newSell)

-- | delete old bid from heap and insert new
reNewOrder :: Ord a => SkewHeap a -> a -> a -> SkewHeap a
reNewOrder sh oBid nBid = insert nBid (delete oBid sh)

-- | checks if trade the highest priority bids match and a trade should be performed,
-- returns true if trade should be performed, otherwise false
orderCheck :: OrderBook -> Bool
orderCheck (OrderBook (buys, sells)) =
  bothHaveVaues && buyingPrice (fromJust bRoot) >= sellingPrice (fromJust sRoot)
  where
    bRoot = rootOf buys
    sRoot = rootOf sells
    bothHaveVaues = isJust bRoot && isJust sRoot

-- | performs the trade and prints details
-- 
-- precondition: the root values of the two priority-heaps contain the orders to be traded
doTrade :: OrderBook -> IO OrderBook
doTrade oB@(OrderBook (buys, sells)) = do
  print $  tradeMessage buyingOrder sellingOrder
  return (OrderBook (delete buyingOrder buys, delete sellingOrder sells))
  where
    buyingOrder@(BuyOrder buyer boughtprice) = fromJust $ rootOf buys
    sellingOrder@(SellOrder seller soldprice) = fromJust $ rootOf sells

-- | returns the string message of who bought from who at what price
tradeMessage :: BuyOrder -> SellOrder -> String
tradeMessage (BuyOrder buyer boughtprice) (SellOrder seller soldgprice)
  = buyer ++ " buys from " ++ seller ++ " for " ++ show boughtprice
