module TsParse where

import Prelude hiding (words)
import Control.Applicative
import Data.List (isSuffixOf)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

-- | A single word in a text column.
word :: Parser String
word = P.many1 (P.noneOf "\n ")

-- | Multiple words in a text column. Separated by a single space.
words :: Parser String
words = fmap concat $ P.sepBy1 word sep
  where
    sep = P.try (P.char ' ' *> P.notFollowedBy (P.char ' '))

columnBreak :: Parser ()
columnBreak
  = () <$ P.char ' ' <* P.char ' ' <* P.many (P.char ' ')

data ValueChange = ValueChange
  { vcBeginningValueDate :: String
  , vcBeginningValueAmount :: String
  , vcContributionsAdditions :: String
  , vcWithdrawalsDeductions :: String
  , vcGainLoss :: String
  , vcEndingValueDate :: String
  , vcEndingValueAmount :: String
  , vcVestedBalance :: String
  } deriving (Eq, Show)

data AccountBalance = AccountBalance
  { acFundName :: String
  , acFundPercent :: String
  } deriving (Eq, Show)



data TxnBySource = TxnBySource
  { tbsPayrollOffice :: String
  , tbsPostingDate :: String
  , tbsTransactionType :: String
  , tbsTraditional :: String
  , tbsRoth :: String
  , tbsAutomatic :: String
  , tbsMatching :: String
  , tbsTotal :: String
  } deriving (Eq, Show)

txnBySource :: Parser TxnBySource
txnBySource
  = TxnBySource
  <$ P.many (P.char ' ')
  <*> word                              -- payroll office
  <* columnBreak
  <*> word                              -- date
  <* columnBreak
  <*> words                             -- transaction type
  <* columnBreak
  <*> words                             -- traditional
  <* columnBreak
  <*> words                             -- roth (words - can be negative)
  <* columnBreak
  <*> words                             -- automatic (can be negative)
  <* columnBreak
  <*> words                             -- matching (can be negative)
  <* columnBreak
  <*> words                             -- total (can be negative)
  <* P.char '\n'

-- | Summary data in the TxnsBySource. Can be either a Gain or Loss or
-- a Beginning Balance or an EndingBalance.
data TxnsBySourceSummary = TxnsBySourceSummary
  { tbsbbTraditional :: String
  , tbsbbRoth :: String
  , tbsbbAuto :: String
  , tbsbbMatching :: String
  , tbsbbTotal :: String
  } deriving (Eq, Show)

-- | Transactions by source beginning balance. In the TSP statement
-- these have dollar signs; these values remove this leading dollar
-- sign.
newtype TxnsBySourceBeginningBal = TxnsBySourceBeginningBal
  { unTransactionsBySourceBeginningBal :: TxnsBySourceSummary }
  deriving (Eq, Show)

-- | Transactions by source gain or loss.
newtype TxnsBySourceGainLoss = TxnsBySourceGainLoss
  { unTransactionsBySourceGainLoss :: TxnsBySourceSummary }
  deriving (Eq, Show)

-- | Transactions by source ending balance.
newtype TxnsBySourceEndingBal = TxnsBySourceEndingBal
  { unTransactionsBySourceEndingBal :: TxnsBySourceSummary }
  deriving (Eq, Show)


txnsBySourceSummary :: Parser TxnsBySourceSummary
txnsBySourceSummary
  = TxnsBySourceSummary
  <$ P.many (P.char ' ')
  <* words                              -- Description
  <* columnBreak
  <*> words                -- Traditional
  <* columnBreak
  <*> words                -- Roth
  <* columnBreak
  <*> words                -- Automatic
  <* columnBreak
  <*> words                -- Matching
  <* columnBreak
  <*> words                -- Total
  <* P.char '\n'

txnsBySourceBeginningBal :: Parser TxnsBySourceBeginningBal
txnsBySourceBeginningBal
  = TxnsBySourceBeginningBal
  <$> txnsBySourceSummary

txnsBySourceGainLoss :: Parser TxnsBySourceGainLoss
txnsBySourceGainLoss
  = TxnsBySourceGainLoss
  <$> txnsBySourceSummary

txnsBySourceEndingBal :: Parser TxnsBySourceEndingBal
txnsBySourceEndingBal
  = TxnsBySourceEndingBal
  <$> txnsBySourceSummary

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
  { tbfPostingDate :: String
  , tbfTransactionType :: String
  , tbfTraditional :: String
  , tbfRoth :: String
  , tbfTotal :: String
  , tbfSharePrice :: String
  , tbfNumShares :: String
  } deriving (Eq, Show)

txnByFund :: Parser TxnByFund
txnByFund
  = TxnByFund
  <$ P.many (P.char ' ')
  <*> word                    -- Posting date
  <* columnBreak
  <*> words                   -- Transaction type
  <* columnBreak
  <*> words                   -- Traditional
  <* columnBreak
  <*> words                   -- Roth
  <* columnBreak
  <*> words                   -- Total
  <* columnBreak
  <*> words                   -- Share price
  <* columnBreak
  <*> words                   -- Number of shares
  <* P.char '\n'

data ByFundBeginningBal = ByFundBeginningBal
  { bfbSharePrice :: String
  , bfbNumShares :: String
  , bfbDollarBalance :: String
  } deriving (Eq, Show)

byFundBeginningBal :: Parser ByFundBeginningBal
byFundBeginningBal
  = ByFundBeginningBal
  <$ P.many (P.char ' ')
  <* P.string "Beginning Balance"
  <* columnBreak
  <*> words                   -- Share Price
  <* columnBreak
  <*> words                   -- Number of Shares
  <* columnBreak
  <*> words                   -- Dollar Balance
  <* P.char '\n'

data ByFundGainLoss = ByFundGainLoss
  { bfgDollarBalance :: String }
  deriving (Eq, Show)

byFundGainLoss :: Parser ByFundGainLoss
byFundGainLoss
  = ByFundGainLoss
  <$ P.many (P.char ' ')
  <* P.string "Gain or Loss This Quarter"
  <* columnBreak
  <*> words
  <* P.char '\n'

data ByFundEndingBal = ByFundEndingBal
  { bfeSharePrice :: String
  , bfeNumShares :: String
  , bfeDollarBalance :: String
  } deriving (Eq, Show)

byFundEndingBal :: Parser ByFundEndingBal
byFundEndingBal
  = ByFundEndingBal
  <$ P.many (P.char ' ')
  <* P.string "Ending Balance"
  <* columnBreak
  <*> words                   -- Share Price
  <* columnBreak
  <*> words                   -- Number of shares
  <* columnBreak
  <*> words                   -- Dollar balance
  <* P.char '\n'  
