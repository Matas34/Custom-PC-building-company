{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

--import Data.List
--import Data.Ord
import Data.Char (isAlpha, isAlphaNum)

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1, Lib2 unit tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Invalid operation" $
      Lib2.parseQuery "Ad CPU intel i9 6GHz" @?= Left ("Unknown operation: Ad"),
    testCase "No operation provided" $
      Lib2.parseQuery " " @?= Left ("Unknown operation: "),
    testCase "Missing component type" $
      Lib2.parseQuery "Add intel i9 6GHz" @?= (Left "Failed to parse component: Incorrect component type."),
    testCase "Incorrect CPU speed entry" $
      Lib2.parseQuery "Add CPU intel i9 6" @?= (Left "Failed to parse component: Speed must end with 'GHz' or 'MHz'"),
    testCase "Incorrect RAM capacity entry" $
      Lib2.parseQuery "Add RAM corsair DDR5 32 6000MHz" @?= (Left "Failed to parse component: Capacity must end with 'GB' or 'TB'"),
    testCase "Missing closing bracket" $
      Lib2.parseQuery "Add PC [PSU corsair 999W" @?= (Left "Failed to parse component: Cannot find the character ']' in an empty input")
  ]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 property tests"
  [
    QC.testProperty "parseStatements (renderStatements s) = s" $
  \statements -> Lib3.parseStatements (Lib3.renderStatements statements) == Right (statements, "")
  
  ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof
    [ Lib3.Batch <$> (QC.resize 10 $ QC.listOf arbitrary)
    , Lib3.Single <$> arbitrary
    ]

instance Arbitrary Lib2.Query where
  arbitrary = oneof 
    [ Lib2.Add <$> arbitrary
    , Lib2.Remove <$> arbitrary
    , pure Lib2.ShowBuild
    , Lib2.Search <$> arbitrary
    , Lib2.Compare <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Lib2.PC where
  arbitrary = Lib2.PC <$> arbitrary

instance Arbitrary Lib2.Components where
  arbitrary = Lib2.Components <$> (QC.resize 10 $ suchThat (QC.listOf arbitrary) (not . null))


instance Arbitrary Lib2.Component where
  arbitrary = oneof
    [ Lib2.CPUComponent <$> arbitrary
    , Lib2.GPUComponent <$> arbitrary
    , Lib2.MotherboardComponent <$> arbitrary
    , Lib2.RAMComponent <$> arbitrary
    , Lib2.StorageComponent <$> arbitrary
    , Lib2.PSUComponent <$> arbitrary
    , Lib2.CaseComponent <$> arbitrary
    , Lib2.PCComponent <$> arbitrary
    ]

instance Arbitrary Lib2.CPU where
  arbitrary = Lib2.CPU <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Lib2.GPU where
  arbitrary = Lib2.GPU <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Lib2.Motherboard where
  arbitrary = Lib2.Motherboard <$> arbitrary <*> arbitrary

instance Arbitrary Lib2.RAM where
  arbitrary = Lib2.RAM <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Lib2.Storage where
  arbitrary = Lib2.Storage <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Lib2.PSU where
  arbitrary = Lib2.PSU <$> arbitrary <*> arbitrary

instance Arbitrary Lib2.Case where
  arbitrary = Lib2.Case <$> arbitrary <*> arbitrary

instance Arbitrary Lib2.Brand where
  arbitrary = Lib2.Brand <$> suchThat (listOf1 arbitraryChar) (all isAlpha)
    where
      arbitraryChar = choose ('a', 'z')

instance Arbitrary Lib2.Model where
  arbitrary = Lib2.Model <$> suchThat (listOf1 arbitraryChar) (all isAlphaNum)
    where
      arbitraryChar = choose ('0', 'z')

instance Arbitrary Lib2.Speed where
  arbitrary = Lib2.Speed <$> arbitrary `suchThat` (\x -> x >= 1 && x <= 10000)

instance Arbitrary Lib2.Vram where
  arbitrary = Lib2.Vram <$> arbitrary `suchThat` (> 0)

instance Arbitrary Lib2.DDR where
  arbitrary = Lib2.DDR <$> arbitrary `suchThat` (\x -> x >= 4 && x <= 6)

instance Arbitrary Lib2.Socket where
  arbitrary = Lib2.Socket <$> elements ["LGA1700", "LGA1200", "AM4", "AM5"]

instance Arbitrary Lib2.Capacity where
  arbitrary = Lib2.Capacity <$> arbitrary `suchThat` (> 0)

instance Arbitrary Lib2.StorageType where
  arbitrary = Lib2.StorageType <$> elements ["SSD", "HDD"]

instance Arbitrary Lib2.Wattage where
  arbitrary = Lib2.Wattage <$> arbitrary `suchThat` (> 0)

instance Arbitrary Lib2.FormFactor where
  arbitrary = Lib2.FormFactor <$> elements ["ATX", "Micro-ATX"]