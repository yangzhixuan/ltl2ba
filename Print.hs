{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Print where

import Data.Maybe
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import BA
import LTL

drawBuchi :: (Show a, Eq a, Ord s, Show s) => AltAutomaton a s -> String
drawBuchi (AltAutomaton sigma states start trans acc) = T.unpack $ printDotGraph g
  where g =  DotGraph { strictGraph = False
                      , directedGraph = True
                      , graphID = Just (Str (T.pack "G"))
                      , graphStatements = DotStmts { attrStmts = []
                                                   , subGraphs = []
                                                   , nodeStmts = nodes
                                                   , edgeStmts = edges
                                                   }
                      }
        nodesID = zip states ([1 .. ] :: [Int])
        nodes = DotNode 0 [shape PointShape]
                  : map (\(s, id) -> DotNode id (Label (StrLabel (T.pack $ show s)) : (if s `elem` acc then [shape DoubleCircle] else []))) nodesID

        edges = DotEdge 0 (lookupNode start) [] : map (\(s1, s2, a) -> DotEdge (lookupNode s1) (lookupNode s2) [Label (StrLabel (T.pack $ show a))]) transitions

        transitions = concatMap (\(s, f) -> concatMap (\a -> map (\t -> (s, t, a)) (targets (f a))) sigma) (M.assocs trans)

        targets :: PBFormula a -> [a]
        targets (PBFormula [a]) = a
        targets _ = error "Buchi automata should only have dijunctive transitions."

        lookupNode s = fromMaybe (error ("Not found: " ++ show s)) (lookup s nodesID)

instance Show (S.Set String) where
  show s | S.null s = "{}"
  show s = "{" ++ S.foldr (\s r -> s ++ (if r == "}" then "" else ", ") ++ r) "}" s


instance Show a => Show (TFormula a) where
  show t = show' 100 t
    where show' p TTrue = "true"
          show' p TFalse = "false"
          show' p (TAtomic a) = show a
          show' p (TNot a) = "!" ++ show' 7 a
          show' p (TNext a) = "X" ++ show' 7 a
          show' p (TUntil a b) = addParen p 8 (show' 8 a ++ " U " ++ show' 8 b)
          show' p (TAnd a b) = addParen p 9 (show' 9 a ++ " & " ++ show' 9 b)
          show' p (TOr a b) = addParen p 10 (show' 10 a ++ " | " ++ show' 10 b)
          addParen :: Int -> Int -> String -> String
          addParen outerPreced myPreced s = if myPreced > outerPreced then "(" ++ s ++ ")" else s

instance Show (TFormula String) where
  show t = show' 100 t
    where show' p TTrue = "true"
          show' p TFalse = "false"
          show' p (TAtomic a) = a
          show' p (TNot a) = "!" ++ show' 7 a
          show' p (TNext a) = "X" ++ show' 7 a
          show' p (TUntil a b) = addParen p 8 (show' 8 a ++ " ∪  " ++ show' 8 b)
          show' p (TAnd a b) = addParen p 9 (show' 9 a ++ " & " ++ show' 9 b)
          show' p (TOr a b) = addParen p 10 (show' 10 a ++ " | " ++ show' 10 b)
          addParen :: Int -> Int -> String -> String
          addParen outerPreced myPreced s = if myPreced > outerPreced then "(" ++ s ++ ")" else s

instance Show a => Show ([TFormula a]) where
  show l | null l = "{}"
  show l = "{" ++ foldr (\s r -> show s ++ (if r == "}" then "" else ",  ") ++ r) "}" l

showWriteFile f s = do putStrLn s
                       writeFile f s
