module Phoityne.Example.MathSpec where

import SpecHelper
import Data.List
import Data.List.Split

data Argument = Url | Port Int | Verbose deriving (Show, Eq)

parseItem :: (String, String) -> Argument
parseItem (name, value) 
 | name == "-p" || name == "--port" = Port portNumber 
    where portNumber = read value :: Int
parseItem ("-v", _) = Verbose 

normalizeArguments :: [String] -> [(String, String)]
normalizeArguments [] = []
normalizeArguments (name:next) 
  | "=" `isInfixOf` name =  (head args, args!!1):normalizeArguments next
    where args = splitOn "=" name  
normalizeArguments (name:value:next) =
  (name, value):normalizeArguments next

normalizeFlags :: [String] -> [String]
normalizeFlags [] = []
normalizeFlags ("-v":tail) = "-v":"True":normalizeFlags tail
normalizeFlags (head:tail) = head:normalizeFlags tail

parseCommand :: [String] -> [Argument]
parseCommand = (parseItem <$>) . normalizeArguments . normalizeFlags

spec :: Spec
spec = 
  describe "parseCommands" $ do
    context "should return verbose" $ do
      it "when contains argument -v" $
        parseCommand ["-v"] `shouldBe` [Verbose]
      it "when not contains argument -v" $
        parseCommand [] `shouldBe` []
    context "should return port number" $ do
      it "when contains argument -p" $ do
        parseCommand ["-p", "23"] `shouldBe` [Port 23]
        parseCommand ["-p", "8080"] `shouldBe` [Port 8080]
      it "when contains argument --port=8080" $
        parseCommand ["--port=8080"] `shouldBe` [Port 8080]
  
    context "should return name value" $
      it "when normalize" $ do
        normalizeArguments ["-p", "23"] `shouldBe` [("-p", "23")]
        normalizeArguments ["--port=23"] `shouldBe` [("--port", "23")]
  
    context "should add True value" $
      it "when normalizeFlags" $ do
        normalizeFlags ["-v"] `shouldBe` ["-v", "True"]
        normalizeFlags ["-u", "hello", "-v", "-p", "23"] `shouldBe` ["-u", "hello", "-v", "True", "-p", "23"]
  

main :: IO ()
main = hspec spec