{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Aeson.JSONScientific where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Scientific (Scientific, coefficient, base10Exponent)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, listOf1, listOf)


newtype JSONScientific = JSONScientific
  { getJSONScientific :: Scientific
  } deriving (Eq, Ord, Show, Read, Generic, Num, Real, NFData, Fractional)

instance ToJSON JSONScientific where
  toJSON (JSONScientific x) = toJSON $
    let c = coefficient x
        e | c == 0 = 0 -- if coefficient is 0, then the whole value is 0
          | otherwise =
            let g :: Int -- decimal places in coefficient alone
                g | c > 0 = length (show c) - 1
                  | otherwise = length (show c) - 2
            in  base10Exponent x + g
        -- coefficient shown, but without trailing zeros (exponent)
        cShownReducedExp :: String
        cShownReducedExp
          | c == 0 = "0"
          | otherwise = dropZerosFromRight (show c)
        c' :: String -- reduced coefficient
        c' | c > 0 =
             if read cShownReducedExp < 10
             then cShownReducedExp
             else take 1 cShownReducedExp ++ "." ++ drop 1 cShownReducedExp
           | c == 0 = "0"
           | otherwise = dropZerosFromRight $
             if read cShownReducedExp > -10
             then cShownReducedExp
             else take 2 cShownReducedExp ++ "." ++ drop 2 cShownReducedExp
    in  c' ++ "e" ++ (if e >= 0 then "+" else "") ++ show e
    where
      dropZerosFromRight :: String -> String
      dropZerosFromRight = reverse . dropWhile (== '0') . reverse

instance FromJSON JSONScientific where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Just x -> pure (JSONScientific x)
      _ -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "JSONScientific" json

instance Arbitrary JSONScientific where
  arbitrary = JSONScientific <$> arbitraryFloat
    where
      arbitraryFloat = do
        s <- listOf1 (elements ['0'..'9'])
        p <- listOf (elements ['0'..'9'])
        case readMaybe (s ++ (if null p then "" else "." ++ p)) of
          Just x -> pure x
