-- import Test.HUnit hiding (Test)
import Test.Hspec
import Test.QuickCheck
import Data.Bson
import FileLocation (debug)

main :: IO ()
main = hspec spec

instance Arbitrary ObjectId where
  arbitrary = do
      t <- arbitrary
      p <- arbitrary
      m <- arbitrary
      i <- arbitrary
      return $ Oid t $ composite m p i

spec :: Spec
spec = do
  describe "ObjectId" $ do
    it "read <-> show" $ property $ \objId ->
      (debug . read . show . debug) objId == (objId :: ObjectId)

  describe "roundTo" $ do
    it "round" $ property $ \d ->
      let r =  roundTo (1 / 10) (d :: Double)
      in  r == roundTo (1 / 10) r
