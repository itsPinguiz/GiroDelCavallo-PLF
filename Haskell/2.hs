import Data.List (elemIndex)

data Formula = Predicate String [Term] | Quantifier QuantifierType String Formula deriving Show
data Term = Constant String | Variable String deriving Show
instance Eq Term where
    (Constant c1) == (Constant c2) = c1 == c2
    (Variable v1) == (Variable v2) = v1 == v2
    _ == _ = False

data QuantifierType = Universal | Existential deriving Show

-- Function to parse a string into a Formula
parseFormula :: String -> Formula
parseFormula input = parseFormula' (words input) []

parseFormula' :: [String] -> [String] -> Formula
parseFormula' [] [] = error "Empty input"
parseFormula' [] _ = error "Syntax error: missing terms or predicate"
parseFormula' (x:xs) stack
    | x `elem` ["forall", "exists"] = parseQuantifier xs stack
    | elem x ["and", "or", "implies", "iff"] = error "Syntax error: infix notation not allowed"
    | otherwise = parsePredicate (x:xs) []

parseQuantifier :: [String] -> [String] -> Formula
parseQuantifier [] _ = error "Syntax error: missing quantifier variable"
parseQuantifier (x:xs) stack
    | elem x ["and", "or", "implies", "iff"] = error "Syntax error: infix notation not allowed"
    | otherwise = Quantifier qType var (parseFormula' xs (var:stack))
    where
        qType = if x == "forall" then Universal else Existential
        var = head xs

parsePredicate :: [String] -> [String] -> Formula
parsePredicate [] _ = error "Syntax error: missing predicate name"
parsePredicate (x:xs) stack = Predicate predName terms
    where
        predName = x
        terms = map (\t -> if t `elem` stack then Variable t else Constant t) xs

-- Function to convert a formula to Skolem normal form
toSkolemNormalForm :: Formula -> Formula
toSkolemNormalForm (Predicate pName terms) = Predicate pName terms
toSkolemNormalForm (Quantifier qType var formula) = toSkolemNormalForm' (Quantifier qType var formula) []

toSkolemNormalForm' :: Formula -> [String] -> Formula
toSkolemNormalForm' (Predicate pName terms) _ = Predicate pName terms
toSkolemNormalForm' (Quantifier qType var formula) stack =
    case qType of
        Universal -> Quantifier qType var (toSkolemNormalForm' formula (var:stack))
        Existential -> replaceQuantifierWithSkolemFunction var formula stack

replaceQuantifierWithSkolemFunction :: String -> Formula -> [String] -> Formula
replaceQuantifierWithSkolemFunction var formula stack =
    let skolemConstants = generateSkolemConstants (reverse stack) formula
        skolemizedFormula = substituteVariableWithConstants formula var skolemConstants
    in skolemizedFormula

generateSkolemConstants :: [String] -> Formula -> [Term]
generateSkolemConstants _ (Predicate _ _) = []
generateSkolemConstants stack (Quantifier _ var formula) =
    let newStack = filter (/= var) stack
    in case formula of
        Predicate _ _ -> [Constant $ "sk_" ++ var ++ "_" ++ show i | i <- [1..length newStack]]
        Quantifier _ _ _ -> generateSkolemConstants newStack formula

substituteVariableWithConstants :: Formula -> String -> [Term] -> Formula
substituteVariableWithConstants (Predicate pName terms) var constants =
    Predicate pName (map (substituteInTerm var constants) terms)
substituteVariableWithConstants (Quantifier qType quantVar formula) var constants =
    Quantifier qType quantVar (substituteVariableWithConstants formula var constants)

substituteInTerm :: String -> [Term] -> Term -> Term
substituteInTerm _ _ (Constant c) = Constant c
substituteInTerm var constants (Variable v) =
    case elemIndex (Variable v) (map (Variable . (\(Constant c) -> c)) constants) of
        Just index -> constants !! index
        Nothing -> Variable v

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a predicate logic formula in prenex normal form:"
    input <- getLine
    let parsedFormula = parseFormula input
        skolemizedFormula = toSkolemNormalForm parsedFormula
    putStrLn $ "Skolem Normal Form: " ++ show skolemizedFormula
