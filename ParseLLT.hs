
import Data.Time
import Text.Regex.PCRE
import Data.Text


example = "P agrees to pay $60 on 2019-01-01. P will vacate the premises on 2019-02-02. P will then go on vacation."

type Line = String
type Money = Float
data Token = GenericToken Line
	   | VacateToken [Day] Line
	   | PaymentToken [Money] Line

instance Show Token where 
	show (GenericToken l) = "Generic:" ++ show l
	show (VacateToken ds l) = "Vacate:" ++ show ds
	show (PaymentToken ms l) = "Payment:" ++ show ms

splitLines t = fmap unpack $ splitOn (pack ".") (pack t)

tokenize :: Line -> Token
tokenize l
	| l =~ "pay" = PaymentToken [0] l
	| l =~ "vaca" = VacateToken [fromGregorian 2019 1 1] l
	| otherwise = GenericToken l

main = do
    putStrLn $ show $ fmap tokenize (splitLines example)

