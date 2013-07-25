{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
             OverloadedStrings, CPP, TemplateHaskell #-}
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
import Data.List.Split (splitOn)
import qualified Text.Parsec as P
import Text.Read (readMaybe)
import Text.Parsec ((<?>))
import Text.Parsec.String (Parser)
import System.Process (readProcess)
import qualified Text.PrettyPrint as Y
import Data.Monoid ((<>))

#ifdef test
import qualified Test.QuickCheck as Q
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck (Gen, Arbitrary(..))
import Data.List (intersperse)
import Data.List.Split (chunksOf)
#endif

type Dollars = Decimal
type Shares = Decimal

#ifdef test

genMantissa :: Gen Integer
genMantissa = fmap fromIntegral
  $ Q.oneof [ Q.arbitrarySizedIntegral :: Gen Int
            , Q.arbitrarySizedBoundedIntegral
            ]

genDollars :: Gen Dollars
genDollars = D.Decimal <$> pure 2 <*> genMantissa

genShares :: Gen Shares
genShares = D.Decimal <$> pure 4 <*> genMantissa

#endif

-- | A single word in a text column.
word :: Parser String
word = P.many1 (P.noneOf "\n ")

#ifdef test

genChar :: Gen Char
genChar = Q.choose ('\0', '\127')

genNonSpacePrintable :: Gen Char
genNonSpacePrintable = Q.choose ('!', '~')

data Rendered a = Rendered
  { ast :: a
  , rendering :: String
  } deriving (Eq, Show)

class Renderable a where
  render :: Gen (Rendered a)

genWord :: Gen (Rendered String)
genWord = do
  cs <- Q.listOf1 genNonSpacePrintable
  return $ Rendered cs cs

#endif

-- | Multiple words in a text column. Separated by a single space.
words :: Parser [String]
words = P.sepBy1 word sep
  where
    sep = P.try (P.char ' ' *> P.notFollowedBy (P.char ' '))

#ifdef test

genWords :: Gen (Rendered [String])
genWords = do
  ws <- Q.listOf1 genWord
  let withSpaces = concat . intersperse " " . map ast $ ws
  return $ Rendered (map ast ws) withSpaces

newtype WordsRen = WordsRen { unWordsRen :: Rendered [String] }
  deriving (Eq, Show)

instance Arbitrary WordsRen where
  arbitrary = WordsRen <$> genWords

prop_words :: WordsRen -> Bool
prop_words = testRendered words . unWordsRen

#endif

-- | Parses a single decimal value. Recognizes negative signs. Strips
-- out dollar signs and commas.
--
-- Use 'readMaybe' rather than 'reads'. The Read instance of Decimal
-- returns an ambiguous parse; 'readMaybe' will use the parse that
-- consumes the entire string if there is one.
decimal :: Parser D.Decimal
decimal = do
  ws <- fmap (map (filter (not . (`elem` "$, ")))) words
  (isNeg, num) <- case ws of
    [] -> fail "empty string, cannot parse decimal"
    x:[] -> return (False, x)
    x:y:[] -> if x == "-"
              then return (True, y)
              else fail "could not parse decimal: too many words"
    _ -> fail "could not parse decimal, too many words"
  dec <- case readMaybe num of
    Nothing -> fail $ "could not parse decimal: " ++ num
    Just r -> return r
  return $ if isNeg then negate dec else dec

#ifdef test

prop_Decimal :: DecimalRen -> Bool
prop_Decimal = testRendered decimal . unDecimalRen

newtype DecimalRen = DecimalRen { unDecimalRen :: Rendered D.Decimal }
  deriving (Eq, Show)

instance Arbitrary DecimalRen where
  arbitrary = do
    dec <- Q.oneof [genDollars, genShares]
    ren <- showDecimalWithSign dec
    return . DecimalRen . Rendered dec $ ren

-- | Shows a decimal, with commas. Does not show the negative sign.

showDecimalNoSign :: D.Decimal -> String
showDecimalNoSign dec =
  let shown = show . abs $ dec
      (whole, frac) = case splitOn "." shown of
        x:xs:[] -> (x, xs)
        _ -> error "unexpected split result"
      commaed = reverse
              . concat
              . intersperse ","
              . chunksOf 3
              . reverse
              $ whole
  in commaed ++ "." ++ frac

-- | Show decimal, with sign. Randomly adds a dollar sign.
showDecimalWithSign :: D.Decimal -> Gen String
showDecimalWithSign d = fmap f arbitrary
  where
    f dolSign =
      let noSign = showDecimalNoSign d
          withDols = if dolSign then '$' : noSign else noSign
      in if d < 0 then "- " ++ withDols else withDols

testRendered :: Eq a => Parser a -> Rendered a -> Bool
testRendered p (Rendered tgt rend) =
  case P.parse p "" rend of
    Left _ -> False
    Right g -> g == tgt

#endif

-- | Parses a single date.
date :: Parser T.Day
date = do
  w <- word
  let ws = splitOn "/" w
  case ws of
    m:d:y:[] -> do
      maybeDy <- T.fromGregorianValid <$> safeRead y
                 <*> safeRead m <*> safeRead d
      case maybeDy of
        Nothing -> fail $ "invalid date: " ++ w
        Just dy -> return dy
    _ -> fail $ "could not parse date: " ++ w

safeRead :: Read x => String -> Parser x
safeRead s = case reads s of
  (x, ""):[] -> return x
  _ -> fail $ "could not read string: " ++ s

columnBreak :: Parser ()
columnBreak
  = () <$ P.char ' ' <* P.char ' ' <* P.many (P.char ' ')


class Pretty a where
  pretty :: a -> Y.Doc

instance Pretty D.Decimal where
  pretty = Y.text . show

instance Pretty String where
  pretty = Y.text

instance Pretty [String] where
  pretty = Y.hsep . map Y.text

instance Pretty T.Day where
  pretty = Y.text . show

data TxnBySource = TxnBySource
  { tbsPayrollOffice :: String
  , tbsPostingDate :: T.Day
  , tbsTransactionType :: [String]
  , tbsTraditional :: Dollars
  , tbsRoth :: Dollars
  , tbsAutomatic :: Dollars
  , tbsMatching :: Dollars
  , tbsTotal :: Dollars
  } deriving (Eq, Show)

label :: Pretty p => String -> p -> Y.Doc
label l p = Y.text l <> Y.text ": " <> pretty p

instance Pretty TxnBySource where
  pretty x = Y.vcat
    [ label "Payroll office" (tbsPayrollOffice x)
    , label "Posting date" (tbsPostingDate x)
    , label "Transaction type" (tbsTransactionType x)
    , label "Traditional" (tbsTraditional x)
    , label "Roth" (tbsRoth x)
    , label "Automatic" (tbsAutomatic x)
    , label "Matching" (tbsMatching x)
    , label "Total" (tbsTotal x)
    ]

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

instance Pretty TxnsBySourceSummary where
  pretty x = Y.vcat
    [ label "Traditional" (tbssTraditional x)
    , label "Roth" (tbssRoth x)
    , label "Automatic" (tbssAuto x)
    , label "Matching" (tbssMatching x)
    , label "Total" (tbssTotal x)
    ]

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

fundName :: Parser [String]
fundName = do
  _ <- P.many (P.char ' ')
  ws <- words
  if last ws == "Fund"
    then P.char '\n' *> return ws
    else fail "not a fund name"

data TxnByFund = TxnByFund
  { tbfPostingDate :: T.Day
  , tbfTransactionType :: [String]
  , tbfTraditional :: Dollars
  , tbfRoth :: Dollars
  , tbfTotal :: Dollars
  , tbfSharePrice :: Dollars
  , tbfNumShares :: Shares
  } deriving (Eq, Show)

instance Pretty TxnByFund where
  pretty x = Y.vcat
    [ label "Posting date" (tbfPostingDate x)
    , label "Transaction type" (tbfTransactionType x)
    , label "Traditional" (tbfTraditional x)
    , label "Roth" (tbfRoth x)
    , label "Total" (tbfTotal x)
    , label "Share price" (tbfSharePrice x)
    , label "Number of shares" (tbfNumShares x)
    ]

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

instance Pretty ByFundBeginningBal where
  pretty x = Y.vcat
    [ label "Share price" (bfbSharePrice x)
    , label "Number of shares" (bfbNumShares x)
    , label "Dollar balance" (bfbDollarBalance x)
    ]

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

instance Pretty ByFundGainLoss where
  pretty x = label "Dollar balance" (bfgDollarBalance x)

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

instance Pretty ByFundEndingBal where
  pretty x = Y.vcat
    [ label "Share price" (bfeSharePrice x)
    , label "Number of shares" (bfeNumShares x)
    , label "Dollar balance" (bfeDollarBalance x)
    ]

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

instance Pretty TransactionDetailBySource where
  pretty x = Y.vcat . Y.punctuate (Y.text "\n") $
    [ Y.hang "Beginning balance:" 2
             (pretty . tdbsBeginningBal $ x)

    , Y.hang "Transactions:" 2
             (Y.vcat . Y.punctuate (Y.text "\n")
                     . map pretty . tdbsTxns $ x)

    , Y.hang "Gain or loss:" 2
             (pretty . tdbsGainLoss $ x)

    , Y.hang "Ending balance:" 2
             (pretty . tdbsEndingBal $ x)
    ]

skipLine :: Parser ()
skipLine
  = P.many (P.noneOf "\n")
  >> P.char '\n'
  >> return ()
  <?> "skip line"

-- | Runs the given parser. If it fails without consuming any input,
-- skip the current line. Keeps running the given parser until it
-- succeeds.
skipLinesThrough :: Parser a -> Parser a
skipLinesThrough p = do
  r <- optional p
  case r of
    Nothing -> skipLine *> skipLinesThrough p
    Just g -> return g

-- | Runs the end parser. If it succeeds, returns all values parsed so
-- far and the value of the end parser. If it fails without consuming
-- any input, runs the main parser. If the main parser succeeds,
-- recurses. If the main parser fails without consuming any input,
-- skips a line and recurses.
parseLinesThrough
  :: Parser body
  -> Parser end
  -> Parser ([body], end)
parseLinesThrough b e = do
  maybeE <- optional e
  case maybeE of
    Nothing -> do
      maybeB <- optional b
      case maybeB of
        Nothing -> skipLine >> parseLinesThrough b e
        Just bdy -> do
          (bs, end) <- parseLinesThrough b e
          return (bdy:bs, end)
    Just end -> return ([], end)
          

txnDetailBySourceSection :: Parser TransactionDetailBySource
txnDetailBySourceSection = do
  _ <- skipLinesThrough
       (P.try (P.string "YOUR TRANSACTION DETAIL BY SOURCE")
              <?> "transaction detail header line")
  begBal <- skipLinesThrough
            (P.try (txnsBySourceSummary "Beginning Balance")
                   <?> "Beginning balance line")
  (txns, gainLoss) <- parseLinesThrough
    (P.try txnBySource <?> "transaction by source")
    (P.try (txnsBySourceSummary "Gain or Loss This Quarter")
           <?> "transaction by source summary")
  endBal <- skipLinesThrough
    (P.try (txnsBySourceSummary "Ending Balance")
           <?> "transactions by source summary")
  return $ TransactionDetailBySource begBal txns gainLoss endBal

data TransactionDetailOneFund = TransactionDetailOneFund
  { tdofFundName :: [String]
  , tdofBeginningBal :: ByFundBeginningBal
  , tdofTxns :: [TxnByFund]
  , tdofGainLoss :: ByFundGainLoss
  , tdofEndingBal :: ByFundEndingBal
  } deriving (Eq, Show)

instance Pretty TransactionDetailOneFund where
  pretty x = Y.vcat
    [ label "Fund name" (tdofFundName x)
    , Y.hang "Beginning balance:" 2 (pretty . tdofBeginningBal $ x)
    , Y.hang "Transactions:" 2
             (Y.vcat . Y.punctuate "\n" . map pretty
                     . tdofTxns $ x)
    , Y.hang "Gain or loss:" 2 (pretty . tdofGainLoss $ x)
    , Y.hang "Ending balance:" 2 (pretty . tdofEndingBal $ x)
    ]

txnDetailOneFund :: Parser TransactionDetailOneFund
txnDetailOneFund = do
  name <- skipLinesThrough (P.try fundName <?> "fund name")
  begBal <- skipLinesThrough
    (P.try byFundBeginningBal <?> "By fund beginning balance")
  (txns, gainLoss) <- parseLinesThrough
    (P.try txnByFund <?> "transaction by fund")
    (P.try byFundGainLoss <?> "by fund gain or loss")
  endBal <- skipLinesThrough
    (P.try byFundEndingBal <?> "by fund ending balance")
  return $ TransactionDetailOneFund name begBal txns gainLoss endBal

txnDetailsAllFunds :: Parser [TransactionDetailOneFund]
txnDetailsAllFunds
  = skipLinesThrough
      (P.try (P.string "YOUR TRANSACTION DETAIL BY FUND")
             <?> "transaction detail by fund header")
  *> P.many (skipLinesThrough (P.try txnDetailOneFund
                                     <?> "transaction details section"))

data TspStatement = TspStatement
  { tspDetailBySource :: TransactionDetailBySource
  , tspDetailByFund :: [TransactionDetailOneFund]
  } deriving (Eq, Show)

instance Pretty TspStatement where
  pretty x = Y.vcat
    [ "DETAIL BY SOURCE"
    , pretty (tspDetailBySource x)
    , "\n"
    , Y.vcat . Y.punctuate "\n" . map pretty
      . tspDetailByFund $ x
    ]

parseTsp :: Parser TspStatement
parseTsp
  = TspStatement
  <$> txnDetailBySourceSection
  <*> txnDetailsAllFunds

readTspFile :: String -> IO String
readTspFile s = readProcess "pdftotext"
                            ["-layout", "-enc", "ASCII7", s, "-"] ""

parseTspFromFile
  :: String
  -- ^ Filename
  -> IO TspStatement
parseTspFromFile fn = do
  s <- readTspFile fn
  case P.parse parseTsp fn s of
    Left e -> fail . show $ e
    Right g -> return g

#ifdef test
runAllTests :: IO Bool
runAllTests = $quickCheckAll
#endif
