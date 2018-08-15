module BA where
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Positive boolean formula in conjunctive normal form ((_ ∨ _ ∨ ... ) ∧ ...)
data PBFormula a = PBFormula [[a]] deriving (Show, Eq)

pbTrue = PBFormula []
pbFalse = PBFormula [[]]

pbAnd (PBFormula x) (PBFormula y) = PBFormula (x ++ y)
pbOr (PBFormula x) (PBFormula y) = PBFormula [ a ++ b | a <- x, b <- y]

pbNegate :: (a -> a) -> PBFormula a -> PBFormula a
pbNegate atomNeg (PBFormula x) = dnf2cnf [map atomNeg a | a <- x]
  where dnf2cnf :: [[a]] -> PBFormula a
        dnf2cnf = foldr (\a b -> pbOr (PBFormula (map (\t -> [t]) a)) b) pbFalse

-- Alphabet, possible states, starting state, transitions, accepting states
data AltAutomaton alphabet state = AltAutomaton (S.Set alphabet) [state] state (M.Map state (alphabet -> PBFormula state)) [state]

instance (Show state, Show sigma) => Show (AltAutomaton sigma state) where
  show (AltAutomaton sigma states start trans acc) = 
    show sigma ++ "\n" ++ show states ++ "\n" ++ show start ++ "\n" ++ show acc ++ "\n Transitions:" ++ showTransitions
    where showTransitions = foldr (\s res -> show s ++ " : " ++ "\n" ++ res) "" states
