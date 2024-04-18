import Control.Applicative
import PriorityQueue.PrioSkew
import System.Environment
import System.IO
import Data.Maybe (isJust, fromJust)

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
trade bids = undefined -- TODO: implement


type OrderBook = (SkewHeap BuyOrder, SkewHeap SellOrder)

addBids :: OrderBook -> [Bid] -> OrderBook
addBids = foldl addAndUpdate
  where
    addAndUpdate :: OrderBook -> Bid -> OrderBook
    addAndUpdate ob b = orderCheck $ addBid ob b


-- please try to improve, this feels bad but I can't figure out anything better ;_;
addBid :: OrderBook -> Bid -> OrderBook
addBid (buyOrders, sellOrders) bid = case bid of
  Buy person price -> let newBuyOrder = BuyOrder person price in
    (insert buyOrders newBuyOrder, sellOrders)
  Sell person price -> let newSellOrder = SellOrder person price in
    (buyOrders, insert sellOrders newSellOrder )
  NewBuy person oldPrice newPrice ->
    let (oldBuy, newBuy) = (BuyOrder person oldPrice, BuyOrder person newPrice) in
    (reNewOrder buyOrders oldBuy newBuy, sellOrders)
  NewSell person oldPrice newPrice ->
    let (oldSell, newSell) = (SellOrder person oldPrice, SellOrder person newPrice) in
    (buyOrders, reNewOrder sellOrders oldSell newSell)


reNewOrder :: Ord a => SkewHeap a -> a -> a -> SkewHeap a
reNewOrder sh oBid = insert (delete oBid sh)

orderCheck :: OrderBook -> OrderBook
orderCheck oB@(buys, sells) 
  | bothHaveVaues && buyingPrice (fromJust bRoot) >= sellingPrice (fromJust sRoot) = doTrade oB
  | otherwise = oB -- do nothng to the order book
  where
    bRoot = rootOf buys
    sRoot = rootOf sells
    bothHaveVaues = isJust bRoot && isJust sRoot

doTrade :: OrderBook -> OrderBook
doTrade = undefined 
