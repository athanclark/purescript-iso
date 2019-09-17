{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.PureScriptIso.Integer (Integer, jsonInteger, getInteger) where

import Prelude hiding (Integer)
import qualified Prelude as P
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Scientific (Scientific, coefficient, base10Exponent, scientific)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, listOf1)


newtype Integer = Integer Scientific
  deriving (Eq, Ord, Show, Read, Generic, Num, Real, NFData)

instance Enum Integer where
  toEnum = jsonInteger . fromIntegral
  fromEnum = fromIntegral . getInteger

instance Integral Integer where
  toInteger = getInteger
  quotRem x y =
    let (a,b) = quotRem (toInteger x) (toInteger y)
    in  (jsonInteger a, jsonInteger b)

jsonInteger :: P.Integer -> Integer
jsonInteger i = Integer (scientific i 0)

getInteger :: Integer -> P.Integer
getInteger (Integer x) = coefficient x * (10 ^ base10Exponent x)

instance ToJSON Integer where
  toJSON (Integer x) = toJSON $
    let c = coefficient x
        e | c == 0 = 0
          | otherwise =
            let g | c > 0 = length (show c) - 1
                  | otherwise = length (show c) - 2
            in  base10Exponent x + g
        cShownReducedExp
          | c == 0 = "0"
          | otherwise = dropZerosFromRight (show c)
        c' | c > 0 =
             if read cShownReducedExp < (10 :: Integer)
             then cShownReducedExp
             else take 1 cShownReducedExp ++ "." ++ drop 1 cShownReducedExp
           | c == 0 = "0"
           | otherwise = dropZerosFromRight $
             if read cShownReducedExp > (-10 :: Integer)
             then cShownReducedExp
             else take 2 cShownReducedExp ++ "." ++ drop 2 cShownReducedExp
    in  c' ++ "e" ++ (if e >= 0 then "+" else "") ++ show e
    where
      dropZerosFromRight :: String -> String
      dropZerosFromRight = reverse . dropWhile (== '0') . reverse

instance FromJSON Integer where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Just x -> pure (Integer x)
      _ -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "Integer" json

instance Arbitrary Integer where
  arbitrary = Integer {-. go-} <$> {-scale (^ 10)-} arbitraryInt
    where
      arbitraryInt = do
        s <- listOf1 (elements ['0'..'9'])
        case readMaybe s of
          Just x -> pure x
          Nothing -> error $ "Can't parse Integer: " ++ s
