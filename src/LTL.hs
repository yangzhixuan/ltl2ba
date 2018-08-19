module LTL where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import BA

data TFormula a = TAtomic a | TTrue | TFalse
                | TAnd (TFormula a) (TFormula a) | TOr (TFormula a) (TFormula a)
                | TNot (TFormula a)
                | TNext (TFormula a) | TUntil (TFormula a) (TFormula a)
     deriving (Eq, Ord)

atomics :: TFormula a -> [a]
atomics a =
    case a of TAtomic p -> [p]
              TTrue -> []
              TFalse -> []
              f@(TAnd x y) -> atomics x ++ atomics y
              f@(TOr x y) -> atomics x ++ atomics y
              f@(TNot x) -> atomics x
              f@(TNext x) -> atomics x
              f@(TUntil x y) -> atomics x ++ atomics y

subFormulas :: TFormula a -> [TFormula a]
subFormulas a =
    case a of TAtomic p -> [TAtomic p]
              TTrue -> [TTrue]
              TFalse -> [TFalse]
              f@(TAnd x y) -> f : subFormulas x ++ subFormulas y
              f@(TOr x y) -> f : subFormulas x ++ subFormulas y
              f@(TNot x) -> f : subFormulas x
              f@(TNext x) -> f : subFormulas x
              f@(TUntil x y) -> f : subFormulas x ++ subFormulas y

tNegate :: TFormula a -> TFormula a
tNegate (TNot a) = a
tNegate TTrue = TFalse
tNegate TFalse = TTrue
tNegate a = (TNot a)

automaton :: (Ord a) => TFormula a -> AltAutomaton (S.Set a) (TFormula a)
automaton fml = removeUnreachable (AltAutomaton sigma states fml trans accept)
  where sigma = S.fromList $ map S.fromList (powerOfList (atomics fml))
        states = let subs = subFormulas fml
                 in nub (subs ++ map tNegate subs)

        accept = filter (\x -> case x of TNot (TUntil _ _) -> True
                                         _ -> False) states
        trans = let pStateTrans = M.assocs $ mkTrans fml      -- transitions for subformulas of fml
                    nStateTrans = [ (tNegate f, pbNegate tNegate . t) | (f, t) <- pStateTrans ] -- transitions for negations of subformulas of fml
                in M.fromList $ pStateTrans ++ nStateTrans

        mkTrans f@(TAtomic q) = M.fromList [(f, \w -> if S.member q w then pbTrue else pbFalse)]
        mkTrans f@(TTrue) = M.fromList [(f, const pbTrue)]
        mkTrans f@(TFalse) = M.fromList [(f, const pbFalse)]
        mkTrans f@(TAnd x y) = let xTrans = mkTrans x
                                   yTrans = mkTrans y
                                   xyTrans = M.fromList $ M.assocs xTrans ++ M.assocs yTrans
                               in M.insert f (\w -> pbAnd ((fromJust $ M.lookup x xyTrans) w)
                                                          ((fromJust $ M.lookup y xyTrans) w)) xyTrans
        mkTrans f@(TOr x y) = let xTrans = mkTrans x
                                  yTrans = mkTrans y
                                  xyTrans = M.fromList $ M.assocs xTrans ++ M.assocs yTrans
                              in M.insert f (\w -> pbOr ((fromJust $ M.lookup x xyTrans) w)
                                                        ((fromJust $ M.lookup y xyTrans) w)) xyTrans
        mkTrans f@(TNot x) = let xTrans = mkTrans x
                             in M.insert f (\w -> pbNegate tNegate ((fromJust $ M.lookup x xTrans) w)) xTrans
        mkTrans f@(TNext x) = let xTrans = mkTrans x
                              in M.insert f (\w -> PBFormula [[x]]) xTrans
        mkTrans f@(TUntil x y) = let xTrans = mkTrans x
                                     yTrans = mkTrans y
                                     xyTrans = M.fromList $ M.assocs xTrans ++ M.assocs yTrans
                              in M.insert f (\w -> pbOr ((fromJust $ M.lookup y xyTrans) w)
                                                        (pbAnd ((fromJust $ M.lookup x xyTrans) w)
                                                               (PBFormula [[f]]))) xyTrans

-- f1 = TNot (TUntil TTrue (TAtomic "q"))
--
-- f2 = TUntil (TNext (TNot (TAtomic "p"))) (TAtomic "q")
