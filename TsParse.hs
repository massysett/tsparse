-- | Parses U.S. federal Thrift Savings Plan (TSP) statements.
--
-- This module works with PDF TSP statements downloaded from the TSP
-- web site. It works with the statement format used as of July 2013.
-- The format recently changed to allow for Roth contributions.
--
-- You need to have the pdftotext program installed and available on
-- your PATH.  This program is part of the poppler project.  On Debian
-- GNU/Linux systems, it is part of the poppler-utils package.
module TsParse where

import qualified Data.Decimal as D
import qualified Data.Time as T
import Data.Decimal (Decimal)
import Prelude hiding (words)
import Control.Applicative
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type Dollars = Decimal
type Shares = Decimal

-- | A single word in a text column.
word :: Parser String
word = P.many1 (P.noneOf "\n ")

-- | Multiple words in a text column. Separated by a single space.
words :: Parser String
words = fmap concat $ P.sepBy1 word sep
  where
    sep = P.try (P.char ' ' *> P.notFollowedBy (P.char ' '))

-- | Parses a single decimal value. Recognizes negative signs. Strips
-- out dollar signs and commas.
decimal :: Parser D.Decimal
decimal = do
  ws <- fmap (filter (not . (`elem` "$, "))) words
  (isNeg, num) <- case ws of
    "" -> fail "empty string, cannot parse decimal"
    x:xs -> return $ if x == '-' then (True, xs) else (False, x:xs)
  dec <- case reads num of
    (r, ""):[] -> return r
    _ -> fail $ "could not parse decimal: " ++ num
  return $ if isNeg then negate dec else dec
  

-- | Parses a single date.
date :: Parser T.Day
date = do
  w <- word
  let ws = splitOn "/" w
  case ws of
    m:d:y:[] ->
      T.fromGregorian <$> safeRead y
      <*> safeRead m <*> safeRead d
    _ -> fail $ "could not parse date: " ++ w

safeRead :: Read x => String -> Parser x
safeRead s = case reads s of
  (x, ""):[] -> return x
  _ -> fail $ "could not read string: " ++ s

columnBreak :: Parser ()
columnBreak
  = () <$ P.char ' ' <* P.char ' ' <* P.many (P.char ' ')


data TxnBySource = TxnBySource
  { tbsPayrollOffice :: String
  , tbsPostingDate :: T.Day
  , tbsTransactionType :: String
  , tbsTraditional :: Dollars
  , tbsRoth :: Dollars
  , tbsAutomatic :: Dollars
  , tbsMatching :: Dollars
  , tbsTotal :: Dollars
  } deriving (Eq, Show)

txnBySource :: Parser TxnBySource
txnBySource
  = TxnBySource
  <$ P.many (P.char ' ')
  <*> word                              -- payroll office
  <* columnBreak
  <*> date                              -- date
  <* columnBreak
  <*> words                             -- transaction type
  <* columnBreak
  <*> decimal                             -- traditional
  <* columnBreak
  <*> decimal                             -- roth (words - can be negative)
  <* columnBreak
  <*> decimal                             -- automatic (can be negative)
  <* columnBreak
  <*> decimal                             -- matching (can be negative)
  <* columnBreak
  <*> decimal                             -- total (can be negative)
  <* P.char '\n'

-- | Summary data in the TxnsBySource. Can be either a Gain or Loss or
-- a Beginning Balance or an EndingBalance.
data TxnsBySourceSummary = TxnsBySourceSummary
  { tbssTraditional :: Dollars
  , tbssRoth :: Dollars
  , tbssAuto :: Dollars
  , tbssMatching :: Dollars
  , tbssTotal :: Dollars
  } deriving (Eq, Show)

-- | Transactions by source beginning balance. In the TSP statement
-- these have dollar signs; these values remove this leading dollar
-- sign.
type TxnsBySourceBeginningBal = TxnsBySourceSummary

-- | Transactions by source gain or loss.
type TxnsBySourceGainLoss = TxnsBySourceSummary

-- | Transactions by source ending balance.
type TxnsBySourceEndingBal = TxnsBySourceSummary


txnsBySourceSummary :: String -> Parser TxnsBySourceSummary
txnsBySourceSummary s
  = TxnsBySourceSummary
  <$ P.many (P.char ' ')
  <* P.string s              -- Description
  <* columnBreak
  <*> decimal                -- Traditional
  <* columnBreak
  <*> decimal                -- Roth
  <* columnBreak
  <*> decimal                -- Automatic
  <* columnBreak
  <*> decimal                -- Matching
  <* columnBreak
  <*> decimal                -- Total
  <* P.char '\n'

--
-- Transaction detail by fund
--

fundName :: Parser String
fundName = do
  _ <- P.many (P.char ' ')
  ws <- words
  if "Fund" `isSuffixOf` ws
    then P.char '\n' *> return ws
    else fail "not a fund name"

data TxnByFund = TxnByFund
  { tbfPostingDate :: T.Day
  , tbfTransactionType :: String
  , tbfTraditional :: Dollars
  , tbfRoth :: Dollars
  , tbfTotal :: Dollars
  , tbfSharePrice :: Dollars
  , tbfNumShares :: Shares
  } deriving (Eq, Show)

txnByFund :: Parser TxnByFund
txnByFund
  = TxnByFund
  <$ P.many (P.char ' ')
  <*> date                    -- Posting date
  <* columnBreak
  <*> words                   -- Transaction type
  <* columnBreak
  <*> decimal                   -- Traditional
  <* columnBreak
  <*> decimal                   -- Roth
  <* columnBreak
  <*> decimal                   -- Total
  <* columnBreak
  <*> decimal                   -- Share price
  <* columnBreak
  <*> decimal                   -- Number of shares
  <* P.char '\n'

data ByFundBeginningBal = ByFundBeginningBal
  { bfbSharePrice :: Dollars
  , bfbNumShares :: Shares
  , bfbDollarBalance :: Dollars
  } deriving (Eq, Show)

byFundBeginningBal :: Parser ByFundBeginningBal
byFundBeginningBal
  = ByFundBeginningBal
  <$ P.many (P.char ' ')
  <* P.string "Beginning Balance"
  <* columnBreak
  <*> decimal                   -- Share Price
  <* columnBreak
  <*> decimal                   -- Number of Shares
  <* columnBreak
  <*> decimal                   -- Dollar Balance
  <* P.char '\n'

data ByFundGainLoss = ByFundGainLoss
  { bfgDollarBalance :: Dollars }
  deriving (Eq, Show)

byFundGainLoss :: Parser ByFundGainLoss
byFundGainLoss
  = ByFundGainLoss
  <$ P.many (P.char ' ')
  <* P.string "Gain or Loss This Quarter"
  <* columnBreak
  <*> decimal
  <* P.char '\n'

data ByFundEndingBal = ByFundEndingBal
  { bfeSharePrice :: Dollars
  , bfeNumShares :: Shares
  , bfeDollarBalance :: Dollars
  } deriving (Eq, Show)

byFundEndingBal :: Parser ByFundEndingBal
byFundEndingBal
  = ByFundEndingBal
  <$ P.many (P.char ' ')
  <* P.string "Ending Balance"
  <* columnBreak
  <*> decimal                   -- Share Price
  <* columnBreak
  <*> decimal                   -- Number of shares
  <* columnBreak
  <*> decimal                   -- Dollar balance
  <* P.char '\n'  

data TransactionDetailBySource = TransactionDetailBySource
  { tdbsBeginningBal :: TxnsBySourceBeginningBal
  , tdbsTxns :: [TxnBySource]
  , tdbsGainLoss :: TxnsBySourceGainLoss
  , tdbsEndingBal :: TxnsBySourceEndingBal
  } deriving (Eq, Show)

skipLine :: Parser ()
skipLine = P.many (P.noneOf "\n") >> P.char '\n' >> return ()

-- | Runs the given parser. If it fails, skip the current line. Keeps
-- running the given parser until it succeeds.
skipLinesThrough :: Parser a -> Parser a
skipLinesThrough p = do
  r <- optional (P.try p)
  case r of
    Nothing -> skipLine *> skipLinesThrough p
    Just g -> return g

-- | Runs the end parser. If it succeeds, returns all values parsed so
-- far and the value of the end parser. If it fails, runs the main
-- parser. If the main parser succeeds, recurses. If the main parser
-- fails, skips a line and recurses.
parseLinesThrough
  :: Parser body
  -> Parser end
  -> Parser ([body], end)
parseLinesThrough b e = do
  maybeE <- optional (P.try e)
  case maybeE of
    Nothing -> do
      maybeB <- optional (P.try b)
      case maybeB of
        Nothing -> skipLine >> parseLinesThrough b e
        Just bdy -> do
          (bs, end) <- parseLinesThrough b e
          return (bdy:bs, end)
    Just end -> return ([], end)
          

txnDetailBySourceSection :: Parser TransactionDetailBySource
txnDetailBySourceSection = do
  begBal <- skipLinesThrough (txnsBySourceSummary "Beginning Balance")
  (txns, gainLoss) <- parseLinesThrough txnBySource
                      (txnsBySourceSummary "Gain or Loss This Quarter")
  endBal <- skipLinesThrough
            (txnsBySourceSummary "Ending Balance")
  return $ TransactionDetailBySource begBal txns gainLoss endBal
