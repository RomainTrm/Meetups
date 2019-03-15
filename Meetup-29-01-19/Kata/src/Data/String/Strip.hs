module Data.String.Strip ( Player(..)
                         , Score(..)
                         , Point(..)
                         , receveurScored
                         , serveurScored
                         )  where

data Player = Serveur | Receveur deriving (Eq, Show)

data Score = Score { scoreServeur :: Point, scoreReceveur :: Point }
            | Avantage Player
            | Winner Player
                deriving (Eq, Show)

data Point = P0 | P15 | P30 | P40 deriving (Eq, Show)

serveurScored :: Score -> Score
serveurScored s = case s of
                    Score P40 P40 -> Avantage Serveur
                    Score P40 _ -> Winner Serveur
                    Avantage Serveur -> Winner Serveur
                    Avantage Receveur -> Score P40 P40
                    _ -> s { scoreServeur = nextPoint (scoreServeur s) }

receveurScored :: Score -> Score
receveurScored s = dispatch Receveur Serveur scoreReceveur s

dispatch :: Player -> Player -> (Score -> Point) -> Score -> Score
dispatch subject oponent pointField currentScore
    | currentScore == Score P40 P40 = Avantage subject
    | pointField currentScore == P40 = Winner subject
    | currentScore == Avantage oponent = Score P40 P40
    | currentScore == Avantage subject = Winner subject
    | _ = (s { pointField = nextPoint (pointField s) })

nextPoint :: Point -> Point
nextPoint p = case p of
                P0 -> P15
                P15 -> P30
                P30 -> P40