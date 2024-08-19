module Parser (runArgument) where

import Data.List (break)
import Text.Megaparsec.Debug
import Text.Megaparsec hiding (manyTill_, manyTill)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr
import Types
import Control.Applicative.Combinators (manyTill_, manyTill)


{-


data ParsecT e s m a
ParsecT e s m a is a parser with custom data component of error e,
stream type s, underlying monad m and return type a.

type Parsec e s = ParsecT e s Identity

What is an Identity Monad ? Lol I don't know. Let's find out.
newtype Identity a = Identity {runIdentity :: a}
  deriving (Bits, Bounded, Enum, Eq, Floating}
ahh category theory, the high iq thing i don't quite get...
But, I can see, the instance for Functor, Applicative,
and monads...
instance Monad Identity where
  m >>= k = k (runIdentity m)
Identity functor and monad. (a non-strict monad)
The custom data component of error e is Void. for our Parser.
Why? I am not sure.
It was in the tutorial.
I should find out more some day.

-}

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space hspace1 empty empty


symbol = L.symbol spaceConsumer
------------------------------------------------------------
              {-- Parsing Propositions --}
pAtomic :: Parser Proposition
pAtomic = (Var <$> (lexeme (letterChar :: Parser Char))) <|> pTop <|> pBottom

{-
   L.symbol :: MonadParsec e s m => m () -> Tokens s -> m (Tokens s)
   Text.Megaparsec.Char.string :: MonadParsec e s m => Tokens s -> m (Tokens s)
   L.lexeme :: MonadParsec e s m => m () -> m a-> m a
-}

pTop :: Parser Proposition
pTop = (\_ -> Top) <$> (symbol "\x22A4")

pBottom :: Parser Proposition
pBottom = (\_ -> Bottom) <$> (symbol "\x22A5")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Proposition
pTerm =  (parens pProposition) <|> pAtomic

pProposition :: Parser Proposition
pProposition = (makeExprParser pTerm operatorTable)

operatorTable :: [[Operator Parser Proposition]]
operatorTable =
  [ [prefix ["~" , "\x00AC" , "!"] Not ],
    [binaryLeft ["&&", "&", "\x2227", "\x00B7","\x0026"] And],
    [binaryLeft ["\x2228", "x002B","\x2225"] Or],
    [binaryLeft ["\x22BB", "\x2295", "\x21AE","\x2262"] Xor],
    [binaryLeft ["\x22BC"] Nand],
    [binaryLeft ["\x22BD"] Nor],
    [binaryRight ["->", "\x2192", "\x21D2","\x2283"] IfThen],
    [binaryRight ["\x2190"] If],
    [binaryN     ["<->", "\x2194" , "\x21D4", "\x2261"] Iff]
  ]

prefix :: [String] -> (Proposition -> Proposition) -> Operator Parser Proposition
prefix names f = Prefix (f <$ (choice $ map symbol names))

binaryN :: [String]
        -> (Proposition -> Proposition -> Proposition)
        -> Operator Parser Proposition
binaryN names f = InfixN (f <$ (choice $ map symbol names))

binaryRight :: [String]
            -> (Proposition -> Proposition -> Proposition)
            -> Operator Parser Proposition
binaryRight names f = InfixR (f <$ (choice $ map symbol names))

binaryLeft :: [String]
           -> (Proposition -> Proposition -> Proposition)
           -> Operator Parser Proposition
binaryLeft names f = InfixL (f <$ (choice $ map symbol names))
----------------------------------------------------------------


parseArgument :: Parser (Premises, Conclusion)
parseArgument = do
        premises        <- parseOnlyProps

        e               <- pTherefore *>  (eitherP (newline) (parseOnlyConclusions))

        conclusions     <- case e of
                            (Left _)            -> parseOnlyConclusions
                            (Right conclusions) -> return $ conclusions

        conclusion      <- case conclusions of
                            [x]                 -> return x
                            (x : xs)            -> return $ foldl (And) (head conclusions) xs
        return $ (premises, conclusion)
        where pTherefore = (symbol "%") <|> (symbol "\x2234") <|> (symbol "\x22A8")

parseOnlyProps :: Parser Premises
parseOnlyProps = do
     premises <- endBy1 (pProposition) (newline)
     return premises

parseOnlyPropsNotArg :: Parser Premises
parseOnlyPropsNotArg = do
     props <- endBy1 (pProposition) (eitherP newline eof)
     return props


parseOnlyConclusions :: Parser [Conclusion]
parseOnlyConclusions = do
     conclusions <- endBy1 (pProposition) (eitherP newline eof)
     return conclusions

parseArg :: Parser (Either Argument Premises)
parseArg =
    eitherP ( (try parseArgument) <* eof) (parseOnlyPropsNotArg <* eof)

-------------------------------------------------------------
{--        THIS IS FOR YOU MAIN         --}


{-
runArgument :: String -> (Either (ParseErrorBundle String Void) POA, [String])
runArgument argument = (parse pArgument "parse argument" argument, filter (/= "") $ (lines argument)) -}


runArgument :: String -> (Either (ParseErrorBundle String Void) (Either Argument Premises), [String])
runArgument props = do
          --PARSE NATURAL ARGUMENT BEFORE PARSING. KIND OF WEIRD
          -- MUST CREATE PREMS CONC FROM USER INPUT
          -- THEN SEE IF THEY REALLY ARE INDEED PREMISES AND CONCLUSION OR JUST PREMISES
          let dropEmptyLines = filter (not . all (== ' ') )
              dropLeadingWhiteSpace = dropWhile (== ' ')
              allProps = map dropLeadingWhiteSpace $ dropEmptyLines $ lines props
              formatedProps = unlines $ allProps
          let e   = parse parseArg "parse argument" formatedProps
          case e of
           (Left _ )  -> (e, [])

           (Right argOrPrem) ->  case argOrPrem of
                                   (Left (premises, conclusion)) -> (e, ( map show premises) ++ ["\x2234 "  ++ show conclusion])
                                   (Right premises') -> (e, map show premises')


