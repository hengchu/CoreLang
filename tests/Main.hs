{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import CoreLang.Parser
import CoreLang.Lexer (coreKeywords)
import CoreLang.PPrint
import CoreLang.Language

import Control.Applicative

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type Defn = (Name, CoreExpr)

arbitraryNum :: Gen a -> Int -> Gen [a]
arbitraryNum gen num | num <= 0 = return []
                     | otherwise = do
  g <- gen
  gs <- arbitraryNum gen (num-1)
  return (g:gs)

arbitraryInt :: Gen Int
arbitraryInt = choose (1, 10)

arbitraryIntRange :: (Int, Int) -> Gen Int
arbitraryIntRange (lowbound, highbound) = choose (lowbound, highbound)

genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z']

genSafeString :: Gen String
genSafeString = suchThat (resize 5 $ listOf1 genSafeChar) (\str -> str `notElem` coreKeywords)

instance Arbitrary CoreProgram where
  arbitrary = resize 5 $ listOf1 arbitrary

instance Arbitrary CoreScDefn where
  arbitrary = do
    name <- genSafeString
    vars <- resize 5 $ listOf1 genSafeString
    e <- arbitrary
    return (name, vars, e)

instance Arbitrary CoreExpr where
  arbitrary = do
    t <- arbitraryIntRange (0, 14)
    switch t

    where arbitraryEVar = EVar <$> genSafeString
          arbitraryENum = ENum <$> arbitraryIntRange (0, maxBound)
          arbitraryEConstr = EConstr <$> arbitraryInt <*> arbitraryInt

          arbitraryEAp :: Gen CoreExpr
          arbitraryEAp = EAp <$> arbitrary <*> arbitrary

          arbitraryDefn :: Gen Defn          
          arbitraryDefn = do
            name <- genSafeString
            e <- arbitrary
            return (name, e)

          arbitraryDefns :: Gen [Defn]
          arbitraryDefns = resize 3 $ listOf1 arbitraryDefn

          arbitraryELet :: Gen CoreExpr
          arbitraryELet =
            ELet <$> arbitrary <*> arbitraryDefns <*> arbitrary

          arbitraryAlter :: Gen CoreAlt
          arbitraryAlter = do
            n1 <- arbitraryInt
            vars <- resize 3 $ listOf1 genSafeString
            e <- arbitrary
            return (n1, vars, e)

          arbitraryAlters :: Gen [CoreAlt]
          arbitraryAlters = resize 3 $ listOf1 arbitraryAlter

          arbitraryECase :: Gen CoreExpr
          arbitraryECase =
            ECase <$> arbitrary <*> arbitraryAlters

          arbitraryELam :: Gen CoreExpr
          arbitraryELam = do
            ELam <$> (resize 3 (listOf1 genSafeString)) <*> arbitrary

          switch t | t `elem` [0..4] = arbitraryEVar
                   | t `elem` [5..9] = arbitraryENum
                   | t == 10 = arbitraryEConstr
                   | t == 11 = arbitraryEAp
                   | t == 12 = arbitraryELet
                   | t == 13 = arbitraryECase
                   | t == 14 = arbitraryELam
                   | otherwise = error "Error: Impossible constructor type -- Arbitrary CoreExpr."

-- | 'pprint' an ast and then 'parseCoreProg' the
-- same program should produce the same ast.
prop_parsePPrint :: CoreProgram -> Bool
prop_parsePPrint prog =
  case parseCoreProg (pprint prog) of
    Left _ -> False
    Right ast -> prog == ast

tests :: [TF.Test]
tests =
  [
  TF.testGroup "QuickCheck CoreLang" [
    testProperty "pprintParse" prop_parsePPrint
  ]
  ]

main :: IO ()
main = TF.defaultMain tests
