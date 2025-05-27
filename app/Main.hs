module Main (
  main,
) where

import Data.Aeson (decode)
import Data.Maybe (isNothing)
import Lib
import MapForest (MapForest (..))
import Query (Query (..))
import Relude hiding (ByteString)
import System.Console.GetOpt

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

main :: IO ()
main = do
  contents <- BL.getContents
  args <- getArgs
  parsedArgs <- parseArgs args
  let Options {optQuery = query} = parsedArgs
  when (isNothing query) (BL.putStr "no query provided")
  case (decode contents :: Maybe MapForest) of
    Just mapForest -> BL.putStr $ manipulateContents (getQuery query) mapForest
    Nothing ->
      putStrLn
        "fatal error occurred =( please consider reading the source code =) "

getQuery :: Maybe Query -> Query
getQuery = fromMaybe (SimpleQuery "")

parseArgs :: [String] -> IO Options
parseArgs argv =
  case getOpt Permute options argv of
    (opts, _, []) -> foldl' (>>=) (pure startOptions) opts
    (_, _, errors) ->
      withFrozenCallStack . error $ T.pack (intercalate ", " errors)

newtype Options = Options
  { optQuery :: Maybe Query
  }

startOptions :: Options
startOptions = Options {optQuery = Nothing}

-- {'CreditSection': {'CreditId': 'd2e68808-bafb-4907-a358-bd4d96ffe1a4', 'CreditIssueDate': '2024-06-19', 'ExpectedEndDate': '2027-06-24', 'Country': 'SE', 'LendingCompany': 'SBL Finans', 'CreditType': 'ConsumerLoans80', 'Originator': 'Loanstep80 (SBL Finans -> Consumer loans)', 'ActualEndTime': '2024-10-28T08:44:33.537448Z', 'ExpectedAnnualInterest': 0.09, 'AnnualInterest': 0.1609, 'Term': {'From': 5, 'To': 36}, 'ExpectedReturnRange': {'From': 0.06, 'To': 0.06}, 'CreditDocuments': [], 'ProjectName': null, 'BuyBackAsFraction': 0.8, 'CreditProjectId': null}, 'HoldingSection': {'HoldingInfo': {'Claim': 0.0, 'FirstInvestmentTime': '2024-06-19T10:17:10.810333Z', 'CreditIssueDate': '2024-06-19', 'CreditExpectedEndDate': '2027-06-24', 'InterestAsFraction': 0.1609, 'IsListedForSale': False, 'CanBeListedForSale': False, 'Status': 'CreditLoss', 'LatenessDescriptor': null}, 'Repayments': [{'Time': '2024-10-28T08:44:33.54798Z', 'Parts': {'Interest': 0.0, 'RepaidPrincipal': 159.1717, 'CreditLoss': 39.7929, 'ServiceFee': 0.0, 'Tax': 0.0, 'GrossReturn': -39.7929}}], 'OriginalPaymentPlan': []}, 'ImportedPostingsSection': null}
options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      "s"
      ["simple-getter"]
      ( ReqArg
          (\arg opt -> pure opt {optQuery = pure (SimpleQuery $ T.pack arg)})
          "SIMPLE GETTER QUERY"
      )
      "Simple Getter"
  , Option
      "a"
      ["advanced-getter"]
      ( ReqArg
          (\arg opt -> pure opt {optQuery = pure (AdvancedQuery $ T.pack arg)})
          "ADVANCED GETTER"
      )
      "Advanced Getter"
  ]
