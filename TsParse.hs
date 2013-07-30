{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
             OverloadedStrings, CPP, TemplateHaskell #-}
-- | Parses U.S. federal Thrift Savings Plan (TSP) statements.
--
-- This module works with PDF TSP statements downloaded from the TSP
-- web site. It works with the statement format used as of July 2013.
-- The format recently changed to allow for Roth contributions.  This
-- works on civilian, FERS statements; maybe it works on others, but I
-- cannot test these (if you test these and find bugs, send me patches
-- and I will merge them.)
--
-- You need to have the pdftotext program installed and available on
-- your PATH.  This program is part of the poppler project.  On Debian
-- GNU/Linux systems, it is part of the poppler-utils package.
module TsParse

-- If in test mode, just export everything.
#ifdef test

  where

#else

  ( -- * Data types

    -- ** Basic types
    Dollars
  , Shares
  , TxnType

    -- ** Transaction Detail By Source
  , BySource(..)
  , BySourceSummary(..)
  , BySourceBeginningBal
  , BySourceGainLoss
  , BySourceEndingBal
  , BySourcePosting(..)

    -- ** Transaction Detail By Fund
  , FundName
  , ByFund(..)
  , ByFundBeginningBal(..)
  , ByFundGainLoss(..)
  , ByFundEndingBal(..)
  , ByFundPosting(..)

    -- ** TSP statement
  , TspStatement(..)

    -- * Parsing a TSP statement
  , parseTsp
  , parseTspFromFile

    -- * Pretty printing
  , Pretty(..)

  ) where
#endif


import qualified Data.Decimal as D
import qualified Data.Time as T
import Data.Decimal (Decimal)
import Prelude hiding (words)
import qualified Prelude
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
import qualified Test.QuickCheck.Property as QP
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck (Gen, Arbitrary(..))
import Data.List (intersperse)
import Data.List.Split (chunksOf)
#endif

-- | Any data type that is Dollars on the TSP statement.
type Dollars = Decimal

-- | Any data type that is a number of shares on the TSP statement.
type Shares = Decimal

#ifdef test

genMantissa :: Gen Integer
genMantissa = fmap fromIntegral
  $ Q.oneof [ Q.arbitrarySizedIntegral :: Gen Int
            , Q.arbitrarySizedBoundedIntegral
            ]

genDollars :: Gen Dollars
genDollars = D.Decimal <$> pure 2 <*> genMantissa

newtype DollarsRen = DollarsRen { unDollarsRen :: Rendered Dollars }
  deriving (Eq, Ord, Show)

instance Arbitrary DollarsRen where
  arbitrary = do
    dec <- genDollars
    ren <- showDecimalWithSign dec
    return . DollarsRen $ Rendered dec ren

genShares :: Gen Shares
genShares = D.Decimal <$> pure 4 <*> genMantissa

newtype SharesRen = SharesRen { unSharesRen :: Rendered Dollars }
  deriving (Eq, Ord, Show)

instance Arbitrary SharesRen where
  arbitrary = do
    dec <- genShares
    ren <- showDecimalWithSign dec
    return . SharesRen $ Rendered dec ren

#endif

isNonSpaceNonControl :: Char -> Bool
isNonSpaceNonControl c = c >= '!' && c <= '~'

-- | A single word in a text column.
word :: Parser String
word = P.many1
  (P.satisfy isNonSpaceNonControl
    <?> "non-space, non-control character")

#ifdef test

genChar :: Gen Char
genChar = Q.choose ('\0', '\127')

genNonSpacePrintable :: Gen Char
genNonSpacePrintable = Q.choose ('!', '~')

data Rendered a = Rendered
  { ast :: a
  , rendering :: String
  } deriving (Eq, Ord, Show)


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
  deriving (Eq, Ord, Show)

instance Arbitrary WordsRen where
  arbitrary = WordsRen <$> genWords

prop_words :: WordsRen -> QP.Result
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

prop_Decimal :: DecimalRen -> QP.Result
prop_Decimal = testRendered decimal . unDecimalRen

newtype DecimalRen = DecimalRen { unDecimalRen :: Rendered D.Decimal }
  deriving (Eq, Ord, Show)

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

testRendered :: (Eq a, Show a) => Parser a -> Rendered a -> QP.Result
testRendered p (Rendered tgt rend) =
  case P.parse p "" rend of
    Left e -> QP.failed { QP.reason = "parse failed: " ++ show e
                          ++ "target: " ++ show tgt
                          ++ "rendered: " ++ rend }
    Right g ->
      if g == tgt
      then QP.succeeded
      else QP.failed { QP.reason = "parsed not equal to original. "
                       ++ "original: " ++ show tgt
                       ++ " parsed: " ++ show g
                       ++ " rendered: " ++ rend }


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

#ifdef test

render2digits :: Int -> String
render2digits i = case show i of
  c:[] -> '0':c:[]
  c -> c

newtype DayRen = DayRen { unDayRen :: Rendered T.Day }
  deriving (Eq, Ord, Show)

instance Arbitrary DayRen where
  arbitrary =
    let lower = fromEnum $ T.fromGregorian 1900 1 1
        upper = fromEnum $ T.fromGregorian 2100 12 31
        f dtNum =
          let dt = toEnum dtNum
              (yr, mo, da) = T.toGregorian dt
              dtStr = render2digits mo ++ "/"
                      ++ render2digits da ++ "/"
                      ++ show yr
          in DayRen $ Rendered dt dtStr
    in fmap f $ Q.choose (lower, upper)

prop_day :: DayRen -> QP.Result
prop_day = testRendered date . unDayRen

#endif

safeRead :: Read x => String -> Parser x
safeRead s = case readMaybe s of
  Just x -> return x
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

-- | A list of words that indicates the transaction type.  Each string
-- in this list will not have any spaces in it.
type TxnType = [String]

data BySourcePosting = BySourcePosting
  { bspPayrollOffice :: String
  , bspPostingDate :: T.Day
  , bspTxnType :: TxnType
  , bspTraditional :: Dollars
  , bspRoth :: Dollars
  , bspAutomatic :: Dollars
  , bspMatching :: Dollars
  , bspTotal :: Dollars
  } deriving (Eq, Ord, Show)

#ifdef test

data RenTxnBySource = RenTxnBySource
  { unRenTxnBySource :: Rendered BySourcePosting }
  deriving (Eq, Ord, Show)


instance Arbitrary RenTxnBySource where
  arbitrary = do
    rPayroll <- genWord
    rPostingDate <- fmap unDayRen arbitrary
    rTxnType <- genWords
    rTrad <- fmap unDollarsRen arbitrary
    rRoth <- fmap unDollarsRen arbitrary
    rAuto <- fmap unDollarsRen arbitrary
    rMatch <- fmap unDollarsRen arbitrary
    rTot <- fmap unDollarsRen arbitrary
    let rAst = BySourcePosting (ast rPayroll) (ast rPostingDate)
          (ast rTxnType) (ast rTrad) (ast rRoth) (ast rAuto)
          (ast rMatch) (ast rTot)
    ren <- columns [ rendering rPayroll, rendering rPostingDate,
                     rendering rTxnType,
                     rendering rTrad,
                     rendering rRoth, rendering rAuto,
                     rendering rMatch, rendering rTot ]
    leader <- fmap (flip replicate ' ') Q.arbitrarySizedIntegral
    return . RenTxnBySource $ Rendered rAst (leader ++ ren ++ "\n")

prop_txnBySource :: RenTxnBySource -> QP.Result
prop_txnBySource = testRendered txnBySource . unRenTxnBySource

genInterleaved :: Gen a -> [a] -> Gen [a]
genInterleaved g = sequence . intersperse g . map return

columnSpacer :: Gen String
columnSpacer = fmap (f . abs) Q.arbitrarySizedIntegral
  where
    f i = "  " ++ replicate i ' '

columns :: [String] -> Gen String
columns = fmap concat . genInterleaved columnSpacer


#endif

label :: Pretty p => String -> p -> Y.Doc
label l p = Y.text l <> Y.text ": " <> pretty p

instance Pretty BySourcePosting where
  pretty x = Y.vcat
    [ label "Payroll office" (bspPayrollOffice x)
    , label "Posting date" (bspPostingDate x)
    , label "Transaction type" (bspTxnType x)
    , label "Traditional" (bspTraditional x)
    , label "Roth" (bspRoth x)
    , label "Automatic" (bspAutomatic x)
    , label "Matching" (bspMatching x)
    , label "Total" (bspTotal x)
    ]

txnBySource :: Parser BySourcePosting
txnBySource
  = BySourcePosting
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

-- | The TSP statement has several lines in the @YOUR TRANSACTION
-- DETAIL BY SOURCE@ section that contain summary data: @Beginning
-- Balance@, @Gain or Loss This Quarter@, and @Ending Balance@. Since
-- the columns in these lines are all the same they are all
-- represented by this single type. Type synonyms
-- 'BySourceBeginningBal', 'BySourceGainLoss', and
-- 'BySourceEndingBal' are used as appropriate.
data BySourceSummary = BySourceSummary
  { bssTraditional :: Dollars
  , bssRoth :: Dollars
  , bssAuto :: Dollars
  , bssMatching :: Dollars
  , bssTotal :: Dollars
  } deriving (Eq, Ord, Show)

#ifdef test

genTxnBySourceSummary
  :: [String]
  -- ^ Description. This is a list of words. It should not contain any
  -- spaces.
  -> Gen (Rendered BySourceSummary)
genTxnBySourceSummary desc = do
  rTrad <- fmap unDollarsRen arbitrary
  rRoth <- fmap unDollarsRen arbitrary
  rAuto <- fmap unDollarsRen arbitrary
  rMatch <- fmap unDollarsRen arbitrary
  rTot <- fmap unDollarsRen arbitrary
  leader <- fmap (flip replicate ' ') Q.arbitrarySizedIntegral
  let rAst = BySourceSummary (ast rTrad) (ast rRoth) (ast rAuto)
        (ast rMatch) (ast rTot)
  ren <- columns [ concat . intersperse " " $ desc,
                   rendering rTrad, rendering rRoth,
                   rendering rAuto, rendering rMatch,
                   rendering rTot ]
  return $ Rendered rAst (leader ++ ren ++ "\n")

prop_txnsBySourceSummary :: WordsRen -> QP.Property
prop_txnsBySourceSummary wr = do
  let ws = ast . unWordsRen $ wr
      wordsStr = concat . intersperse " " $ ws
  QP.forAll (genTxnBySourceSummary ws)
    (testRendered (txnsBySourceSummary wordsStr))

#endif

instance Pretty BySourceSummary where
  pretty x = Y.vcat
    [ label "Traditional" (bssTraditional x)
    , label "Roth" (bssRoth x)
    , label "Automatic" (bssAuto x)
    , label "Matching" (bssMatching x)
    , label "Total" (bssTotal x)
    ]

-- | @YOUR TRANSACTION DETAIL BY SOURCE@ Beginning Balance.
type BySourceBeginningBal = BySourceSummary

-- | @YOUR TRANSACTION DETAIL BY SOURCE@ Gain or Loss This Quarter.
type BySourceGainLoss = BySourceSummary

-- | @YOUR TRANSACTION DETAIL BY SOURCE@ Ending Balance.
type BySourceEndingBal = BySourceSummary


txnsBySourceSummary :: String -> Parser BySourceSummary
txnsBySourceSummary s
  = BySourceSummary
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

#ifdef test

newtype FundNameRen = FundNameRen
  { unFundNameRen :: Rendered [String] }
  deriving (Eq, Ord, Show)

instance Arbitrary FundNameRen where
  arbitrary = do
    ws <- fmap unWordsRen arbitrary
    let ws' = ws { ast = ast ws ++ ["Fund"] }
        ws'' = ws' { rendering = rendering ws ++ " Fund\n" }
    return $ FundNameRen ws''

prop_fundName :: FundNameRen -> QP.Result
prop_fundName = testRendered fundName . unFundNameRen

#endif

-- | A single posting in the @YOUR TRANSACTION DETAIL BY FUND@ section.
data ByFundPosting = ByFundPosting
  { bfpPostingDate :: T.Day
  , bfpTxnType :: [String]
  , bfpTraditional :: Dollars
  , bfpRoth :: Dollars
  , bfpTotal :: Dollars
  , bfpSharePrice :: Dollars
  , bfpNumShares :: Shares
  } deriving (Eq, Ord, Show)

#ifdef test

genTxnByFund :: Gen (Rendered ByFundPosting)
genTxnByFund = do
  rdy <- fmap unDayRen arbitrary
  rty <- genWords
  rtrad <- fmap unDollarsRen arbitrary
  rroth <- fmap unDollarsRen arbitrary
  rtot <- fmap unDollarsRen arbitrary
  rpri <- fmap unDollarsRen arbitrary
  rsha <- fmap unDollarsRen arbitrary
  leader <- fmap (flip replicate ' ') Q.arbitrarySizedIntegral
  let rAst = ByFundPosting (ast rdy) (ast rty) (ast rtrad) (ast rroth)
                       (ast rtot) (ast rpri) (ast rsha)
  ren <- columns [ rendering rdy,
                   rendering rty,
                   rendering rtrad, rendering rroth,
                   rendering rtot, rendering rpri, rendering rsha ]
  return $ Rendered rAst (leader ++ ren ++ "\n")

prop_txnByFund :: QP.Property
prop_txnByFund = QP.forAll genTxnByFund $ testRendered txnByFund

#endif

instance Pretty ByFundPosting where
  pretty x = Y.vcat
    [ label "Posting date" (bfpPostingDate x)
    , label "Transaction type" (bfpTxnType x)
    , label "Traditional" (bfpTraditional x)
    , label "Roth" (bfpRoth x)
    , label "Total" (bfpTotal x)
    , label "Share price" (bfpSharePrice x)
    , label "Number of shares" (bfpNumShares x)
    ]

txnByFund :: Parser ByFundPosting
txnByFund
  = ByFundPosting
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

-- | The beginning balance in a @YOUR TRANSACTION DETAIL BY FUND@
-- section.
data ByFundBeginningBal = ByFundBeginningBal
  { bfbbSharePrice :: Dollars
  , bfbbNumShares :: Shares
  , bfbbDollarBalance :: Dollars
  } deriving (Eq, Ord, Show)

#ifdef test

genByFundBeginningBal :: Gen (Rendered ByFundBeginningBal)
genByFundBeginningBal = do
  rpr <- fmap unDollarsRen arbitrary
  rsha <- fmap unSharesRen arbitrary
  rbal <- fmap unDollarsRen arbitrary
  leader <- fmap (flip replicate ' ') Q.arbitrarySizedIntegral
  let rAst = ByFundBeginningBal (ast rpr) (ast rsha) (ast rbal)
  ren <- columns [ "Beginning Balance",
                   rendering rpr, rendering rsha, rendering rbal ]
  return $ Rendered rAst (leader ++ ren ++ "\n")

prop_byFundBeginningBal :: QP.Property
prop_byFundBeginningBal = QP.forAll genByFundBeginningBal
  $ testRendered byFundBeginningBal

#endif

instance Pretty ByFundBeginningBal where
  pretty x = Y.vcat
    [ label "Share price" (bfbbSharePrice x)
    , label "Number of shares" (bfbbNumShares x)
    , label "Dollar balance" (bfbbDollarBalance x)
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

-- | Gain or Loss This Quarter in the @YOUR TRANSACTION DETAIL BY
-- FUND@ section.
data ByFundGainLoss = ByFundGainLoss
  { bfglDollarBalance :: Dollars }
  deriving (Eq, Ord, Show)

#ifdef test

genByFundGainLoss :: Gen (Rendered ByFundGainLoss)
genByFundGainLoss = do
  rbal <- fmap unDollarsRen arbitrary
  leader <- fmap (flip replicate ' ') Q.arbitrarySizedIntegral
  let rAst = ByFundGainLoss (ast rbal)
  ren <- columns [ "Gain or Loss This Quarter", rendering rbal ]
  return $ Rendered rAst (leader ++ ren ++ "\n")

prop_byFundGainLoss :: QP.Property
prop_byFundGainLoss = QP.forAll genByFundGainLoss
  $ testRendered byFundGainLoss

#endif

instance Pretty ByFundGainLoss where
  pretty x = label "Dollar balance" (bfglDollarBalance x)

byFundGainLoss :: Parser ByFundGainLoss
byFundGainLoss
  = ByFundGainLoss
  <$ P.many (P.char ' ')
  <* P.string "Gain or Loss This Quarter"
  <* columnBreak
  <*> decimal
  <* P.char '\n'

-- | Ending balance in the @YOUR TRANSACTION DETAIL BY FUND@ section.
data ByFundEndingBal = ByFundEndingBal
  { bfebSharePrice :: Dollars
  , bfebNumShares :: Shares
  , bfebDollarBalance :: Dollars
  } deriving (Eq, Ord, Show)

#ifdef test

genByFundEndingBal :: Gen (Rendered ByFundEndingBal)
genByFundEndingBal = do
  rpr <- fmap unDollarsRen arbitrary
  rsha <- fmap unSharesRen arbitrary
  rbal <- fmap unDollarsRen arbitrary
  leader <- fmap (flip replicate ' ') Q.arbitrarySizedIntegral
  let rAst = ByFundEndingBal (ast rpr) (ast rsha) (ast rbal)
  ren <- columns [ "Ending Balance",
                   rendering rpr, rendering rsha, rendering rbal ]
  return $ Rendered rAst (leader ++ ren ++ "\n")

prop_byFundEndingBal :: QP.Property
prop_byFundEndingBal = QP.forAll genByFundEndingBal
  $ testRendered byFundEndingBal

#endif

instance Pretty ByFundEndingBal where
  pretty x = Y.vcat
    [ label "Share price" (bfebSharePrice x)
    , label "Number of shares" (bfebNumShares x)
    , label "Dollar balance" (bfebDollarBalance x)
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

-- | Represents the entire @YOUR TRANSACTION DETAIL BY SOURCE@ section.
data BySource = BySource
  { bsBeginningBal :: BySourceBeginningBal
  , bsTxns :: [BySourcePosting]
  , bsGainLoss :: BySourceGainLoss
  , bsEndingBal :: BySourceEndingBal
  } deriving (Eq, Ord, Show)

#ifdef test

maxSize :: Int -> Gen a -> Gen a
maxSize i g = Q.sized (\s -> Q.resize (min s i) g)

vectorMaxOf :: Int -> Gen a -> Gen [a]
vectorMaxOf i g = Q.sized $ \s -> do
  let maxLen = min s i
  len <- Q.choose (0, maxLen)
  Q.vectorOf len g

vectorMaxOf1 :: Int -> Gen a -> Gen [a]
vectorMaxOf1 i g = Q.sized $ \s -> do
  let maxLen = min s i
  len <- Q.choose (1, (max 1 maxLen))
  Q.vectorOf len g

-- | Generates a garbage line, which should be discarded. Some of
-- these are lines with only a form feed (ASCII 0x0C) as pdftotext
-- generates some of these.
genGarbageLine :: Gen String
genGarbageLine = Q.oneof
  [ fmap (++ "\n")
    $ maxSize 30 (Q.listOf
        (genChar `Q.suchThat` (not . (`elem` "\n\x0C"))))
  , return "\x0C"
  ]


addJunkLines :: [String] -> Gen [String]
addJunkLines ls = do
  firstLines <- maxSize 3 (Q.listOf genGarbageLine)
  restLines <- genInterleaved genGarbageLine ls
  return $ firstLines ++ restLines


genTxnDetailsBySource :: Gen (Rendered BySource)
genTxnDetailsBySource = do
  rBeginningBal <- genTxnBySourceSummary
    (Prelude.words "Beginning Balance")
  rTxnList <- Q.listOf arbitrary
  let rTxns = map unRenTxnBySource rTxnList
  rGainLoss <- genTxnBySourceSummary
    (Prelude.words "Gain or Loss This Quarter")
  rEndingBal <- genTxnBySourceSummary
    (Prelude.words "Ending Balance")
  let hdr = "YOUR TRANSACTION DETAIL BY SOURCE\n"
      renLines = hdr : rendering rBeginningBal : map rendering rTxns
                 ++ [rendering rGainLoss, rendering rEndingBal]
  renWithJunk <- addJunkLines renLines
  let rAst = BySource (ast rBeginningBal)
              (map ast rTxns) (ast rGainLoss) (ast rEndingBal)
  return $ Rendered rAst (concat renWithJunk)

prop_transactionDetailsBySource :: QP.Property
prop_transactionDetailsBySource = QP.forAll genTxnDetailsBySource
  $ testRendered txnDetailBySourceSection

#endif

instance Pretty BySource where
  pretty x = Y.vcat . Y.punctuate (Y.text "\n") $
    [ Y.hang "Beginning balance:" 2
             (pretty . bsBeginningBal $ x)

    , Y.hang "Transactions:" 2
             (Y.vcat . Y.punctuate (Y.text "\n")
                     . map pretty . bsTxns $ x)

    , Y.hang "Gain or loss:" 2
             (pretty . bsGainLoss $ x)

    , Y.hang "Ending balance:" 2
             (pretty . bsEndingBal $ x)
    ]

skipLine :: Parser ()
skipLine
  = P.many (P.noneOf "\n\x0C")
  >> (P.char '\n' <|> P.char '\x0C')
  >> return ()
  <?> "skip line"

-- | Runs the given parser. If it fails without consuming any input,
-- skip the current line. Keeps running the given parser until it
-- succeeds.
--
-- Do not wrap this parser in the Parsec many or many1 parsers or the
-- like; it probably will not do what you expect. When it gets to end
-- of file, it will consume the remaining junk lines, and then fail
-- after consuming input. This will cause many to fail while consuming
-- input, and it will not return the items that skipLinesThrough has
-- parsed so far.  You can wrap skipLinesThrough in 'try', but
-- remember that then the trailing last junk lines will not be parsed
-- (which might be what you want anyway.)
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
          

txnDetailBySourceSection :: Parser BySource
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
  return $ BySource begBal txns gainLoss endBal

-- | The name of a fund, eg @C Fund@. This is a list of words; each
-- word will not contain any spaces.
type FundName = [String]

-- | A single fund in the @YOUR TRANSACTION DETAIL BY FUND@ section
-- (e.g. the @G Fund@, @L 2040 Fund@, etc.)
data ByFund = ByFund
  { bfFundName :: FundName
  , bfBeginningBal :: ByFundBeginningBal
  , bfPostings :: [ByFundPosting]
  , bfGainLoss :: ByFundGainLoss
  , bfEndingBal :: ByFundEndingBal
  } deriving (Eq, Ord, Show)

#ifdef test

genTxnDetailOneFund :: Gen (Rendered ByFund)
genTxnDetailOneFund = do
  rFund <- fmap unFundNameRen arbitrary
  rBeg <- genByFundBeginningBal
  rTxns <- Q.listOf genTxnByFund
  rGain <- genByFundGainLoss
  rEnd <- genByFundEndingBal
  let rAst = ByFund (ast rFund) (ast rBeg)
             (map ast rTxns) (ast rGain) (ast rEnd)
      renLines = rendering rFund : rendering rBeg
                 : map rendering rTxns
                 ++ [ rendering rGain, rendering rEnd]
  renWithJunk <- addJunkLines renLines
  return $ Rendered rAst (concat renWithJunk)

prop_transactionDetailOneFund :: QP.Property
prop_transactionDetailOneFund = QP.forAll genTxnDetailOneFund
  $ testRendered txnDetailOneFund

#endif

instance Pretty ByFund where
  pretty x = Y.vcat
    [ label "Fund name" (bfFundName x)
    , Y.hang "Beginning balance:" 2 (pretty . bfBeginningBal $ x)
    , Y.hang "Transactions:" 2
             (Y.vcat . Y.punctuate "\n" . map pretty
                     . bfPostings $ x)
    , Y.hang "Gain or loss:" 2 (pretty . bfGainLoss $ x)
    , Y.hang "Ending balance:" 2 (pretty . bfEndingBal $ x)
    ]

txnDetailOneFund :: Parser ByFund
txnDetailOneFund = do
  name <- skipLinesThrough (P.try fundName <?> "fund name")
  begBal <- skipLinesThrough
    (P.try byFundBeginningBal <?> "By fund beginning balance")
  (txns, gainLoss) <- parseLinesThrough
    (P.try txnByFund <?> "transaction by fund")
    (P.try byFundGainLoss <?> "by fund gain or loss")
  endBal <- skipLinesThrough
    (P.try byFundEndingBal <?> "by fund ending balance")
  return $ ByFund name begBal txns gainLoss endBal

#ifdef test

genTxnDetailsAllFunds :: Gen (Rendered [ByFund])
genTxnDetailsAllFunds = do
  rFunds <- vectorMaxOf1 5 genTxnDetailOneFund
  let hdr = "YOUR TRANSACTION DETAIL BY FUND\n"
      renLines = hdr : map rendering rFunds
      rAst = map ast rFunds
  withJunk <- addJunkLines renLines
  return $ Rendered rAst (concat withJunk)

prop_txnDetailsAllFunds :: QP.Property
prop_txnDetailsAllFunds = QP.forAll genTxnDetailsAllFunds
  $ testRendered txnDetailsAllFunds

#endif

txnDetailsAllFunds :: Parser [ByFund]
txnDetailsAllFunds
  = skipLinesThrough
      (P.try (P.string "YOUR TRANSACTION DETAIL BY FUND")
             <?> "transaction detail by fund header")
  *> P.many (P.try (skipLinesThrough (P.try txnDetailOneFund
                                     <?> "transaction details section")))

#ifdef test

genTspStatement :: Gen (Rendered TspStatement)
genTspStatement = do
  rBySource <- genTxnDetailsBySource
  rByFund <- genTxnDetailsAllFunds
  let rAst = TspStatement (ast rBySource) (ast rByFund)
      renLines = [rendering rBySource, rendering rByFund]
  withJunk <- addJunkLines renLines
  moreJunk <- vectorMaxOf 3 genGarbageLine
  return $ Rendered rAst ((concat withJunk) ++ concat moreJunk)

prop_parseTsp :: QP.Property
prop_parseTsp = QP.forAll genTspStatement
  $ testRendered parseTsp

#endif

-- | All data that is parsed from the TSP statement is in this
-- type. The parser does not attempt to parse any of the data that is
-- on Page 1 of the PDF; most of this data all appears elsewhere on
-- the statement and can be calculated using the data that is in this
-- type (and besides, the data on Page 1 is in a multi-column format
-- that would be difficult to parse; since the data is all elsewhere,
-- it's not worth the effort.) One exception is the investment
-- allocation for future contributions, which does not appear
-- elsewhere.
--
-- In addition, the statement contains a quarterly account summary.
-- This also is not parsed because it can be derived from all the data
-- that is elsewhere on the statement.
data TspStatement = TspStatement
  { tspDetailBySource :: BySource
  , tspDetailByFund :: [ByFund]
  } deriving (Eq, Ord, Show)

instance Pretty TspStatement where
  pretty x = Y.vcat
    [ "DETAIL BY SOURCE"
    , pretty (tspDetailBySource x)
    , "\n"
    , Y.vcat . Y.punctuate "\n" . map pretty
      . tspDetailByFund $ x
    ]

-- | Parses a plain text TSP statement.  The input must be generated
-- by the pdftotext program.  This library was tested against
-- pdftotext version 0.18.4, which came with Debian Wheezy.
parseTsp :: Parser TspStatement
parseTsp
  = TspStatement
  <$> txnDetailBySourceSection
  <*> txnDetailsAllFunds

readTspFile :: String -> IO String
readTspFile s = readProcess "pdftotext"
                            ["-layout", "-enc", "ASCII7", s, "-"] ""


-- | Parses a TSP statement from a file.  This function relies upon
-- the @pdftotext@ program.  This program must exist somewhere in your
-- PATH.  This library was tested against pdftotext version 0.18.4,
-- which came with Debian Wheezy.
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
