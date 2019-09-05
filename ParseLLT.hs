import Data.Time
import Text.Regex.PCRE
import Data.Text
import Data.Bits ((.|.))
import Data.Array
import Text.Read (readMaybe)
import Control.Applicative 
import Data.Csv
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 as BLU
import qualified Data.Vector as V

example = "P agrees to pay $60 on 02/02/2000. P will vacate the premises on 01/01/2000. P will then go on vacation."

type Line = String
type Money = Maybe Integer 
data Token = GenericToken Line
   | VacateToken [Maybe Day] Line
   | PaymentToken [Money] Line

instance Show Token where 
    show (GenericToken l) = "Generic:" ++ show l
    show (VacateToken ds l) = "Vacate:" ++ show ds
    show (PaymentToken ms l) = "Payment:" ++ show ms

type TokenAST = [Token]

splitLines :: String -> [Line]
splitLines t = fmap unpack $ splitOn (pack ".") (pack t)

tokenize :: Line -> Token
tokenize l
    | l =~ "pay" = PaymentToken (extractPayments l)  l
    | l =~ "vaca" = VacateToken (extractDates l) l
    | otherwise = GenericToken l

--regex options
compilationOptions = defaultCompOpt .|. compCaseless

caselessMoneyRegex :: Regex
caselessMoneyRegex = makeRegexOpts compilationOptions defaultExecOpt "\\$[0-9]+(\\.[0-9]+)?"

caselessDayRegex :: Regex
caselessDayRegex = makeRegexOpts compilationOptions defaultExecOpt "\\d{2}/\\d{2}/\\d{2,4}"


extractPayments :: Line -> [Money]
extractPayments l = fmap (readMaybe . removeDollarSign) (extractMatches matches) :: [Money]
    where matches = matchAllText caselessMoneyRegex l

removeDollarSign s = [ c | c <- s, not (c `elem` "$")]

extractMatches matches = fmap (fst . Prelude.head . elems) matches 

extractDates :: Line -> [Maybe Day]
extractDates l = fmap (parseTimeM True defaultTimeLocale "%m/%d/%Y") (extractMatches matches) :: [Maybe Day]
    where matches = matchAllText caselessDayRegex l


sumPayments :: [Money] -> Money
sumPayments ps = Prelude.foldr (liftA2 (+)) (Just 0) ps

data JBARow = JBARow {
                        id :: !String
                      , jba_substance_additional :: !String
                     }

instance FromNamedRecord JBARow where
    parseNamedRecord r = JBARow <$> r .: (BLU.fromString "id") <*> r .: (BLU.fromString "jba_substance_additional")

main = do
    putStrLn $ show $ fmap tokenize (splitLines example)
    putStrLn "Now opening csv"
    csvData <- BL.readFile "/home/nvogel/Downloads/jbaAdditional.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
          putStrLn $ Main.id p ++ ": Total Payments: " ++ ((show . sumPayments . extractPayments . jba_substance_additional) p) 
    putStrLn "Done" 
    


