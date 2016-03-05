import qualified Data.Map as Map

-- http://lpaste.net/105251841490550784
-- http://lpaste.net/4604658056066760704

type Digit = Int

type Presses = Int

type DaPhone = Map.Map Char [Char]


digits :: DaPhone
digits = Map.fromList $ [('#', ",."), ('*', "*"), ('1', "1"), ('2', "abc2"), ('3', "def3"), ('4', "ghi4"), ('5', "jkl5"),('6', "mno6"), ('7', "pqrs7"), ('8', "tuv8"), ('9', "wxyz9"), ('0', " 0")]


convo :: [String]
convo = ["Wanna play 20 questions",
              "Ya",
              "U 1st haha",
              "Lol ok. Have u ever tasted alcohol lol",
              "Lol ya",
              "Wow ur cool haha. Ur turn",
              "Ok. Do u think I am pretty Lol",
              "Lol ya",
              "Haha thanks just making sure rofl ur turn"]


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph [] = []
cellPhonesDead ph (x:xs) = 

