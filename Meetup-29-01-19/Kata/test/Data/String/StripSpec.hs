module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.String.Strip

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "le serveur marque le point" $ do
    it "score de 0:0, il passe à 15:0" $ do
      let result = serveurScored (Score { scoreServeur = P0, scoreReceveur = P0 })
      result `shouldBe` Score { scoreServeur = P15, scoreReceveur = P0 }
    it "score de 15:15, il passe à 30:15" $ do
      let result = serveurScored (Score { scoreServeur = P15, scoreReceveur = P15 })
      result `shouldBe` Score { scoreServeur = P30, scoreReceveur = P15 }
    it "score de 30:0, il passe à 40:0" $ do
      let result = serveurScored (Score { scoreServeur = P30, scoreReceveur = P0 })
      result `shouldBe` Score { scoreServeur = P40, scoreReceveur = P0 }
    it "score de 40:40, il passe à Avantage Serveur" $ do
      let result = serveurScored (Score { scoreServeur = P40, scoreReceveur = P40 })
      result `shouldBe` Avantage Serveur
    it "score de 40:15, il passe à Winner Serveur" $ do
      let result = serveurScored (Score { scoreServeur = P40, scoreReceveur = P15 })
      result `shouldBe` Winner Serveur
    it "score de Avantage Serveur, il passe à Winner Serveur" $ do
      let result = serveurScored (Avantage Serveur)
      result `shouldBe` Winner Serveur
    it "score de Avantage Receveur, il passe à 40:40" $ do
      let result = serveurScored (Avantage Receveur)
      result `shouldBe` Score { scoreServeur = P40, scoreReceveur = P40 }
  describe "le receveur marque le point" $ do
    it "score de 0:0, il passe à 0:15" $ do
      let result = receveurScored (Score { scoreServeur = P0, scoreReceveur = P0 })
      result `shouldBe` Score { scoreServeur = P0, scoreReceveur = P15 }
    it "score de 15:15, il passe à 15:30" $ do
      let result = receveurScored (Score { scoreServeur = P15, scoreReceveur = P15 })
      result `shouldBe` Score { scoreServeur = P15, scoreReceveur = P30 }
    it "score de 0:30, il passe à 0:40" $ do
      let result = receveurScored (Score { scoreServeur = P0, scoreReceveur = P30 })
      result `shouldBe` Score { scoreServeur = P0, scoreReceveur = P40 }
    it "score de 40:40, il passe à Avantage Receveur" $ do
      let result = receveurScored (Score { scoreServeur = P40, scoreReceveur = P40 })
      result `shouldBe` Avantage Receveur
    it "score de 30:40, il passe à Winner Receveur" $ do
      let result = receveurScored (Score { scoreServeur = P30, scoreReceveur = P40 })
      result `shouldBe` Winner Receveur
    it "score de 0:40, il passe à Winner Receveur" $ do
      let result = receveurScored (Score { scoreServeur = P0, scoreReceveur = P40 })
      result `shouldBe` Winner Receveur
    it "score de Avantage Receveur, il passe à Winner Receveur" $ do
      let result = receveurScored (Avantage Receveur)
      result `shouldBe` Winner Receveur 
    it "score de Avantage Serveur, il passe à 40:40" $ do
      let result = receveurScored (Avantage Serveur)
      result `shouldBe` Score { scoreServeur = P40, scoreReceveur = P40 }