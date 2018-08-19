module Main where
import Data.Maybe (fromMaybe)
import Print
import Parser
import LTL
import BA
import qualified Options.Applicative as OA
import Options.Applicative ((<$>), (<*>))
import Data.Semigroup ((<>))

data Options = Options { opOutputFile :: Maybe String, opTextMode :: Bool, opFormula :: String }

withDefaultValue a = fmap (fromMaybe a)

optsParser = Options <$> OA.optional (OA.strOption ( OA.long "output" <> OA.short 'o' <> OA.metavar "FILE" <> OA.help "Output filename"))
                     <*> OA.switch (OA.long "text-output" <> OA.short 't' <> OA.help "Output the automaton in plain text")
                     <*> OA.argument OA.str (OA.metavar "FORMULA")

optsWithInfo = OA.info (OA.helper <*> optsParser) (OA.fullDesc <> OA.progDesc "Converts a linear logic formula to its corresponding Buchi automaton in GraphViz's .dot format.\nGrammar of formulas:\nF = true | false | <lower letters> | F & F | F '|' F | F U F | X F. " <> OA.header "ltl2ba -- converts linear temporal logic formulas to Buchi automata")

main :: IO ()
main = do options <- OA.execParser optsWithInfo
          let fml = parseString (opFormula options)
          let auto = alt2ba (automaton fml)
          let result = if opTextMode options then show auto else drawBuchi auto
          case opOutputFile options of
            Nothing -> putStrLn result
            (Just filename) -> writeFile filename result
