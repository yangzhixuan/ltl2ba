module BA where
import Text.Printf
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Positive boolean formula in conjunctive normal form ((_ ∨ _ ∨ ... ) ∧ ...)
data PBFormula a = PBFormula {unPBFormula :: [[a]]} deriving (Eq)

pbTrue = PBFormula []
pbFalse = PBFormula [[]]

pbAnd y (PBFormula [[]]) = pbFalse
pbAnd (PBFormula [[]]) y = pbFalse
pbAnd (PBFormula x) (PBFormula y) = PBFormula (nub (x ++ y))

pbOr (PBFormula []) y = pbTrue
pbOr y (PBFormula []) = pbTrue
pbOr (PBFormula x) (PBFormula y) = PBFormula (nub [ nub (a ++ b) | a <- x, b <- y])

pbNegate :: Eq a => (a -> a) -> PBFormula a -> PBFormula a
pbNegate atomNeg (PBFormula x) = dnf2cnf [map atomNeg a | a <- x]
  where dnf2cnf :: (Eq a) => [[a]] -> PBFormula a
        dnf2cnf = foldr (\a b -> pbOr (PBFormula (map (\t -> [t]) a)) b) pbFalse

-- satisfyings univ fml: all subsets of univ that makes fml true
satisfyings :: (Eq a) => [a] -> PBFormula a -> [[a]]
satisfyings univ (PBFormula []) = powerOfList univ
satisfyings univ (PBFormula (disj:rest)) = filter (\m -> (not . null) (intersect disj m)) (satisfyings univ (PBFormula rest))

-- 
minimalSatisyings :: [[a]] -> [[a]]
minimalSatisyings [x] = [x]
minimalSatisyings (x:xs) = undefined

instance (Show a) => Show (PBFormula a) where
  show (PBFormula []) = "⊤"
  show (PBFormula cs) = foldr (\disj res -> "(" ++ showDisj disj ++ ")" ++ (if res == "" then "" else " ∧ " ++ res)) "" cs
    where showDisj [] = "⊥"
          showDisj ds = foldr1 (\d r -> d ++ " ∨ " ++ r) (map show ds)

-- Alphabet, possible states, starting state, transitions, accepting states
data AltAutomaton alphabet state = AltAutomaton { aSigma :: (S.Set alphabet),
                                                  aStates :: [state],
                                                  aStart :: state,
                                                  aTrans :: (M.Map state (alphabet -> PBFormula state)),
                                                  aAccepts :: [state]}

instance (Show state, Ord state, Show sigma) => Show (AltAutomaton sigma state) where
  show (AltAutomaton sigma states start trans acc) =
    "Σ: " ++ show sigma ++ "\n"
    ++ "States: " ++ show states ++ "\n"
    ++ "Starting: " ++ show start ++ "\n"
    ++ "Accepting: " ++ show acc
    ++ "\nTransitions:\n" ++ showTransitions
      where showTransitions = foldr showStateTrans "" states
            showStateTrans s res = maybe res
                                         (\sTrans -> "    " ++ show s ++ ": " ++
                                                     foldr (\c res -> (if sTrans c == pbFalse then "" else show c ++ " ==> " ++ show (sTrans c) ++ " , ") ++ res)
                                                           "" sigma
                                                     ++ "\n" ++ res)
                                         (M.lookup s trans)

powerOfList :: [a] -> [[a]]
powerOfList [] = [[]]
powerOfList (x:xs) = let l = powerOfList xs
                     in map (x : ) l ++ l

alt2ba :: (Eq s, Ord s) => AltAutomaton a s -> AltAutomaton a ([s], [s])
alt2ba (AltAutomaton sigma states start trans acc) =
  removeUnreachable (AltAutomaton sigma
                                  newStates
                                  ([start], [])
                                  newTrans
                                  [([], y) | y <- stateSets])
  where stateSets = powerOfList states
        newStates = [(x, y) | x <- stateSets, y <- stateSets]
        newTrans = M.fromList $ map (\s -> (s, stateTrans s)) newStates

        stateTrans ([],v) w = PBFormula [[ (y \\ acc, intersect y acc) |
                                     y <- satisfyings states (foldr (\v1 f -> pbAnd (lookup' v1 w) f) pbTrue v) ]]

        stateTrans (u,v) w = PBFormula [nub [ (x \\ acc, nub (y ++ intersect x acc)) |
                                   x <- satisfyings states (foldr (\u1 f -> pbAnd (lookup' u1 w) f) pbTrue u),
                                   y <- satisfyings states (foldr (\v1 f -> pbAnd (lookup' v1 w) f) pbTrue v) ]]

        lookup' u = fromMaybe (const pbFalse) (M.lookup u trans)

removeUnreachable (AltAutomaton sigma states start trans acc) =
  AltAutomaton sigma reachable start trans (intersect acc reachable)
  where deadEnds = S.fromList $ filter (\s -> all (\w -> fromMaybe (const pbFalse) (M.lookup s trans) w == pbFalse) sigma) states
        reachable = S.elems (traverse start S.empty)
        traverse s visited | S.member s visited = visited
        traverse s visited | S.member s deadEnds = S.insert s visited
        traverse s visited = foldr (\suc vis -> traverse suc vis) (S.insert s visited) succs
          where succs = concat (map (\w -> maybe [] (\pb -> (concat . unPBFormula) (pb w)) (M.lookup s trans)) (S.elems sigma))
