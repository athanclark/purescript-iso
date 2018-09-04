{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Aeson.JSONInteger where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (decimal, signed)
import Data.Scientific (Scientific, coefficient, base10Exponent)
import qualified Data.Text as T
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (scale, elements, listOf1)
import System.IO.Unsafe (unsafePerformIO)


newtype JSONInteger = JSONInteger
  { getJSONInteger :: Scientific
  } deriving (Eq, Ord{-, Enum-}, Show, Read, Generic, Num, Real{-, Integral-})

instance ToJSON JSONInteger where
  toJSON (JSONInteger x) = toJSON $
    let c = coefficient x
        e = let g | c > 0 = length (show c) - 1
                  | c == 0 = 0
                  | otherwise = length (show c) - 2
            in  base10Exponent x + g
        q | c > 0 = dropZeros (show c)
          | c == 0 = "0"
          | otherwise = "-" ++ dropZeros (drop 1 $ show c)
        c' | c > 0 =
             if read q < 10
             then q
             else take 1 q ++ "." ++ drop 1 q
           | c == 0 = show c
           | otherwise = dropZeros $
             if read q > -10
             then q
             else take 2 q ++ "." ++ drop 2 q
        dropZeros = reverse . dropWhile (== '0') . reverse
    in  c' ++ "e" ++ (if e >= 0 then "+" else "") ++ show e

instance FromJSON JSONInteger where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Just x -> pure (JSONInteger x)
      _ -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "JSONInteger" json

instance Arbitrary JSONInteger where
  arbitrary = JSONInteger {-. go-} <$> {-scale (^ 10)-} arbitraryInt
    where
      arbitraryInt = do
        s <- listOf1 (elements ['0'..'9'])
        case readMaybe s of
          Just x -> pure x
      go x = unsafePerformIO $ do
        print x
        pure x
