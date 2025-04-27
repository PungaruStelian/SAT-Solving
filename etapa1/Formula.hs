module Formula where

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

{-
A boolean variable is represented by a positive integer: 1, 2, etc.

type introduces a type synonym, similar to typedef in C.
-}
type Variable = Int

{-
A boolean literal designates a variable or its negation.

It is represented as a non-zero integer: 1, 2, -1, -2, etc. The polarity
of the literal is captured by the sign of the integer; for example, the literal 1
designates variable 1, and the literal -1, its negation. The integer 0 is
excluded because it would prevent distinguishing polarities.
-}
type Literal = Int

{-
A clause is a disjunction of literals.

It is represented as an ordered set of literals. This representation
naturally captures the uniqueness and irrelevance of the order of literals in a
clause, and allows for efficient searching.
-}
type Clause = Set Literal

{-
A CNF formula is a conjunction of clauses.

It is represented as an ordered set of clauses.
-}
type Formula = Set Clause
-- ex: [[1, 2], [-1, 3]] = (1 ∨ 2) ∧ (¬1 ∨ 3)

{-
An interpretation is a set of literals assumed to be true.

For example, the interpretation {1, -2} assumes that literals 1 and -2 are true,
meaning variable 1 is true, and variable 2 is false.
-}
type Interpretation = Set Literal

{-
Builds a clause from a list of literals.
-}
toClause :: [Literal] -> Clause
-- creates a set from a list: orders and removes duplicates
toClause = Set.fromList

{-
Builds a formula from a list of lists of literals.
-}
toFormula :: [[Literal]] -> Formula
-- a set is a list of unique elements, ordered, and from the way it is represented in
-- string format: fromList appears in front of the set.
-- Formula is a set of sets of literals,
-- each set is unique and sorted by size and then by elements
toFormula = Set.fromList . map toClause

{-
Transforms a formula into a list of lists of literals, for readability.
-}
toLiteralLists :: Formula -> [[Literal]]
-- step by step we go from a set of sets to a list of sets and then a list of lists
toLiteralLists = map Set.toList . Set.toList

{-
*** TODO ***

Implement the literalVariable function, which determines the variable referenced by a
literal.

CONSTRAINTS:

* Use point-free style.

Examples:

>>> literalVariable 1
Prelude.undefined:\Users\punga\AppData\Local\Temp\extF7A5: withFile: does not exist (No such file or directory):\Users\punga\AppData\Local\Temp\extF7A5: withFile: permission denied (Permission denied)

>>> literalVariable (-1)
Prelude.undefinedrelude.undefinedrelude.undefined

Notice the placement of (-1) in parentheses. Their absence, as in
literalVariable -1, would have resulted in subtracting 1 from literalVariable,
an operation without meaning.
-}
literalVariable :: Literal -> Variable
literalVariable = abs

{-
*** TODO ***

Implement the isPositive function, which checks if the polarity of a literal
is positive.

CONSTRAINTS:

* Use point-free style.

Examples:

>>> isPositive 1
True

>>> isPositive (-1)
False
-}
isPositive :: Literal -> Bool
isPositive = (> 0)

{-
*** TODO ***

Implement the isNegative function, which checks if the polarity of a literal
is negative.

CONSTRAINTS:

* Use point-free style.

Examples:

>>> isNegative 1
False

>>> isNegative (-1)
True
-}
isNegative :: Literal -> Bool
isNegative = (0 >)

{-
*** TODO ***

Implement the complement function, which determines the complementary literal.

CONSTRAINTS:

* Use point-free style.

Examples:

>>> complement 1
-1

>>> complement (-1)
1
-}
complement :: Literal -> Literal
-- lambda function
complement = negate

{-
*** TODO ***

Implement the formulaLiterals function, which determines the set of all
literals in a formula.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style.

Examples:

>>> formulaLiterals $ toFormula [[1, -2, 3], [-1, 2, 3]]
fromList [-2,-1,1,2,3]

Notice how literals are implicitly ordered in the set.
-}
formulaLiterals :: Formula -> Set Literal
-- Set.union takes 2 sets and combines them: initially the empty set with the first set,
-- and then the result with the next set
formulaLiterals = Set.foldl Set.union Set.empty

{-
*** TODO ***

Implement the clauseVariables function, which determines the set of all
variables in a clause.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style.

Examples:

>>> clauseVariables $ toClause [1, -2, -1]
fromList [1,2]
-}
clauseVariables :: Clause -> Set Variable
-- updates the initial set with the results of applying
-- the function to each element and subsequently eliminates duplicates
clauseVariables = Set.map literalVariable -- abs

{-
*** TODO ***

Implement the formulaVariables function, which determines the set of all
variables in a formula.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style.

Examples:

>>> formulaVariables $ toFormula [[1, -2], [-1, 2, 3]]
fromList [1,2,3]
-}
formulaVariables :: Formula -> Set Variable
-- the result of set.map will be a set of sets, so we need to combine them
formulaVariables = formulaLiterals . Set.map clauseVariables

{-
*** TODO ***

Implement the isPureLiteral function, which checks if a literal is pure
in a formula, in the sense of the absence of its complement from that formula.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style for the formula parameter; you may still explicitly
  specify the literal parameter.

Examples:

>>> isPureLiteral 1 $ toFormula [[1, -2, 3], [-1, 2, 3]]
False

>>> isPureLiteral 2 $ toFormula [[1, -2], [-1, 2, 3]]
False

>>> isPureLiteral 3 $ toFormula [[1, -2, 3], [-1, 2, 3]]
True

>>> isPureLiteral 3 $ toFormula [[1, -2], [-1, 2, 3]]
True
-}
isPureLiteral :: Literal -> Formula -> Bool
-- it is assumed that the literal appears in the formula
-- and returns a bool as a result of applying to all booleans
-- with Set.notMember I check if the opposite of the literal doesn't exist and
-- returns a bool for each set with the help of Set.map
-- and [] = True
isPureLiteral literal = and . Set.map (Set.notMember (complement literal))

{-
*** TODO ***

Implement the isUnitClause function, which checks if a clause is a unit clause,
i.e., contains a single literal.

CONSTRAINTS:

* Use point-free style.

Examples:

>>> isUnitClause $ toClause [1]
True

>>> isUnitClause $ toClause [1, 2]
False
-}
isUnitClause :: Clause -> Bool
isUnitClause = (1 ==) . Set.size

{-
*** TODO ***

Implement the isValidClause function, which checks if a clause is
always true, in the sense that it contains both a literal and
its complement.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use sections, i.e., partial infix applications, of the form (x `f`) or
  (`f` y).

Hint: the any functional.

Examples:

>>> isValidClause $ toClause []
False

>>> isValidClause $ toClause [-1, 1, 2]
True

>>> isValidClause $ toClause [1, 2]
False
-}
isValidClause :: Clause -> Bool
-- any f clause = true if at least one element satisfies f
-- Set.member x clause
-- without `` Set.member would have taken the first parameter as the set instead of the element,
-- now it creates a curried function that waits only for the element

-- I create the set with the complementary elements of the clause with map
-- I check if any element from the created set is in the initial set with any
isValidClause clause = any (`Set.member` clause) (Set.map complement clause)

{-
*** TODO ***

Implement the isValidFormula function, which checks if a formula is
always true, in the sense that all its clauses are always
true.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style.

Hint: the all functional.

Examples:

>>> isValidFormula $ toFormula []
True

>>> isValidFormula $ toFormula [[1, -1, 2], [-2, 2, 3]]
True

>>> isValidFormula $ toFormula [[1, -1, 2], [-2, 3]]
False
-}
isValidFormula :: Formula -> Bool
isValidFormula = all isValidClause

{-
*** TODO ***

Implement the satisfiesFormula function, which checks if an interpretation
satisfies a formula, in the sense that, assuming the literals in the
interpretation are true, the formula is true.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style for the formula parameter; you may still explicitly
  specify the interpretation parameter.

Examples:

>>> satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2]]
True

>>> satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2], [2]]
False

>>> satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2], [-2]]
True
-}
satisfiesFormula :: Interpretation -> Formula -> Bool
-- in interpretation there are numbers in their truth value,
-- this means that if the opposite of a number appears it represents false
-- I take first each clause and then each element from the clause
-- which I look for in the interpretation to see if it is true
-- If I found one then the whole clause is true.
-- If all clauses are true then the formula is satisfied
satisfiesFormula interpretation = all (any (`Set.member` interpretation))

{-
*** TODO ***

Implement the interpretations function, which generates the list of all
interpretations for a set of variables.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.
* Use point-free style.
* Use list comprehensions.

Examples:

>>> interpretations $ Set.empty
[fromList []]

>>> interpretations $ Set.fromList [1]
[fromList [1],fromList [-1]]

>>> interpretations $ Set.fromList [1, 2]
[fromList [1,2],fromList [-2,1],fromList [-1,2],fromList [-2,-1]]
-}
interpretations :: Set Variable -> [Interpretation]
-- Tolist only format the set in the list
-- The Lambda function transforms a number in the list of 2 numbers adding the complementary
-- Sequency receives a list of lists and returns all possible combinations
-- format back in the set
interpretations = map Set.fromList . sequence . (\x -> [[-y, y] | y <- x]) . Set.toList

{-
*** TODO ***

Implement the isSatisfiable function, which checks if a formula is
satisfiable, i.e., true in at least one interpretation.

CONSTRAINTS:

* Avoid explicit recursion, leveraging set functionals
  and functions defined above.

Examples:

>>> isSatisfiable $ toFormula [[1, 2], [-2]]
True

>>> isSatisfiable $ toFormula [[1], [-1]]
False

>>> isSatisfiable $ toFormula []
True

>>> isSatisfiable $ toFormula [[]]
False

QUESTIONS:

1. How does lazy evaluation contribute to the efficient exploration of interpretations?
2. What would have happened if interpretations returned a set (Set) of 
   interpretations instead of a list? Use the Debug.Trace module, already 
   imported, to visualize the interpretations generated in both situations.
   
   To simulate returning a set from interpretations, it is sufficient
   to apply Set.fromList on the list returned by the current implementation in
   the body of isSatisfiable.

ANSWER: ...
-}
isSatisfiable :: Formula -> Bool
-- checks if there exists at least one interpretation that makes the formula true
-- first gets all variables in the formula using formulaVariables
-- then generates all possible interpretations for those variables
-- finally checks if any of these interpretations satisfies the formula
isSatisfiable formula = any (`satisfiesFormula` formula) $ interpretations $ formulaVariables formula
