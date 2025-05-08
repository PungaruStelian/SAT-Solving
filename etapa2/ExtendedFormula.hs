module ExtendedFormula where

import Formula

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow ((&&&), second)
import Data.List (intercalate)

{-
Extended formula representation is necessary as we will begin to remove
Literals from formulas but solving algorithms require both current form
of the clauses as well as the original form.

Therefore, instead of a simple set of clauses, as in stage 1, we use a 
Associative painting (MAP), in which the keys are the current clauses,
subject to the process 
elimination, and values ​​are the original clauses. Every current clause 
corresponds to the original clause from which it was obtained by elimination.

Next, we will name the formulas in stage 1 (formula) formula
Simple, in opposition to the extended formulas defined here (extendedformula).
-}
type ExtendedFormula = Map Clause Clause

{-
An action performed on a formula can be:

* Literal units clause: elimination of the unique literal from a unitary clause.
  The "clause" represents the original clause from which the unitary clause was obtained.
  For example, if from the original clause {1, 2} the unitary clause {1} was obtained
  through a previous action,
the current unitary clause elimination action 
  {1} is united 1 {1, 2}, and not united 1 {1}, as the information would have become
  redundancy!
* Pure literal: elimination of a pure literal.
* Decides literally: elimination of some literal.
* Night: Operation without effect.
-}
data Action
    = Unit   { getLiteral :: Literal, getClause :: Clause }
    | Pure   { getLiteral :: Literal }
    | Decide { getLiteral :: Literal }
    | NOP
    deriving (Show, Eq)

{-
The history of the actions performed, starting with the latest action (meaning
anticronological).

Each action appears in pairs with the formula it has produced. We use it 
stack list, and not a lot, as the temporal order is
relevance. The first pair corresponds to the latest action.
-}
type History = [(Action, ExtendedFormula)]

{-
Determines the extended representation of a simple formula. Initially, before any 
elimination, keys and values ​​coincide.

>>> extendFormula $ toFormula [[1], [-2, 3]]
fromList [(fromList [-2,3],fromList [-2,3]),(fromList [1],fromList [1])]
-}
extendFormula :: Formula -> ExtendedFormula
extendFormula = Map.fromSet id

{-
Toformula's counterpart in stage 1.

>>> toExtendedFormula [[1], [-2, 3]]
fromList [(fromList [-2,3],fromList [-2,3]),(fromList [1],fromList [1])]
-}
toExtendedFormula :: [[Literal]] -> ExtendedFormula
toExtendedFormula = extendFormula . toFormula

{-
Determines the simple formula related to an extended formula. Of interest is
the current variant of the extended formula, surprised by the keys that reflect the actions 
Made, not the original version, surprised by values.

>>> baseFormula $ toExtendedFormula [[1], [-2, 3]]
fromList [fromList [-2,3],fromList [1]]
-}
baseFormula :: ExtendedFormula -> Formula
baseFormula = Map.keysSet

{-
Allows more legible visualization of an action history, applying
HV data builder on the historian. Formulas produced are highlighted
of actions, surprised by the keyboard keys.

>>> HV [(NOP, toExtendedFormula [[1], [-2, 3]]), (NOP, toExtendedFormula [[4]])]
NOP => [[-2,3],[1]]
NOP => [[4]]
-}
newtype HistoryVisualizer = HV History
instance Show HistoryVisualizer where
    show (HV history) =
        intercalate "\n" $
        map (\(action, formula) ->
            show action ++ " => " ++
            show (toLiteralLists (baseFormula formula)))
        history

{-
*** TODO ***

Implement the promoted function that promotes a function operating on a
A simple formula, at a function operating on an extended formula. The functional
helps us reuse certain functions defined on simple formulas without being
We need to redefine them from scratch for extended formulas.

Constraints:

* Use the point-free style.

Hint: Write the Point-Wise feature first, and then see how you can remove
formal parameters.

Example:

>>> promote formulaLiterals $ toExtendedFormula [[1, 2], [-2]]
fromList [-2,1,2]
-}
promote :: (Formula -> a) -> ExtendedFormula -> a
-- In the input I have a function that receives a formula and returns something, and an extendedformula
-- I want to return all that something but the function is applied from the formula to the extended
-- Therefore, transform the extendedform into the formula, and apply the function on it.
-- practically apply only on keys
promote = (. baseFormula)

{-
*** TODO ***

Implement the eliminated function, which eliminates a true literal from a 
extended formula. At the level of the keys, this means that:

* All clauses containing the literal disappear.
* All the appearances of the literal complement in the remaining clauses disappear.

Constraints:

* Avoid explicit recussion,
capitalizing on functional on paintings
  and the functions defined above.
* Use the point-free style in the formula prame; You can explicit though
  the literal parameter.

Example:

>>> eliminate 1 $ toExtendedFormula [[1, 2], [-1, 3]]
fromList [(fromList [3],fromList [-1,3])]

Mai sus, clauza {1, 2} dispare, iar din caluza { -1, 3} dispare literalul -1.

>>> eliminate (-1) $ toExtendedFormula [[1, 2], [-1, 3]]
fromList [(fromList [2],fromList [1,2])]

Mai sus, clauza { -1, 3} dispare, iar din caluza {1, 2} dispare literalul 1.
-}
eliminate :: Literal -> ExtendedFormula -> ExtendedFormula
-- fromList [
--     (fromList [1,2], fromList [1,2]),
--     (fromList [-1,3], fromList [-1,3])
-- ]
-- literal = 1
-- =>
-- fromList [
--     (fromList [3], fromList [-1,3])
-- ]

-- Map .filterwithkey eliminates the keys and values ​​for which the key does not respect the Lambda function,
-- That's why we are not interested in 2 parameter
-- I then apply each key (being a set) and eliminate only the presence of the literal
eliminate literal = Map.mapKeys (Set.delete (-literal)) . Map.filterWithKey (\key _ -> Set.notMember literal key)

{-
*** TODO ***

Implement the FIRSTPURELITERAL function, which determines the first pure literal one
Extended formula, if it exists. The first literal means the smallest, from 
the perspective of the ordered crowds.

Reflect whether your implementation performs calculations only up to 
the determination of the pure first literal or determines all of them.
Answer: Until the pure first

Constraints:

* Avoid explicit recursion by capitalizing on sets by crowds
  and the functions defined above.
* Use the spiureliteral function in stage 1.
* Use the above promoted feature.

Hint: Functions on crowds visit the elements in the natural order.

Example:

>>> firstPureLiteral $ toExtendedFormula [[1], [-1]]
Nothing

>>> firstPureLiteral $ toExtendedFormula [[1, 2, 3], [2, -3]]
Just 1

Mai sus, 1 și 2 sunt literali puri, dar primul (cel mai mic) este 1.

>>> firstPureLiteral $ toExtendedFormula [[1, -2, -3], [-1, -2, -3]]
Just (-3)

Mai sus, -3 și -2 sunt literali puri, dar primul (cel mai mic) este -3.
-}
firstPureLiteral :: ExtendedFormula -> Maybe Literal
-- The input is already ordered ascending (and in the key elements, and the keys between them)
-- the last pair of brackets, being also applied to,
-- combine all the literals in the extendedformula (only the keys: set of sets) in a set
-- For each literal of this set I check apply the ISSI-associated Ispureliteral function
-- and I return the first literal to respect it
firstPureLiteral extFormula = find (\lit -> promote (isPureLiteral lit) extFormula) (promote formulaLiterals extFormula)

{-
*** TODO ***

Implement the FIRSTUNITCLAUSA function, which determines the first unitary clause 
from an extended formula, if it exists. The first clause means the most 
Little, from the perspective of the ordered crowds. More precisely, if so, 
The function returns a pair with the only literal of the unitary clause, and the clause
original from which the unitary clause was obtained.
The unitary clause is key, and 
The original one, value.

Reflect whether your implementation performs calculations only up to 
the determination of the first unitary clause or determines them all.

Answer: Because of Map.Foldrwithkey only stops at the end of

Constraints:

* Avoid explicit recussion,
capitalizing on functional on paintings
  and the functions defined above.
* Use the Point-Free Stitle.
* Use the ISUNITCLAUSA function from Stage 1.

Hints:

* Functions on paintings visit the entrances in the natural order of the keys.
* MAP.Foldrwithkey function.

Example:

>>> firstUnitClause $ toExtendedFormula [[1, 2], [1, 3]]
Nothing

>>> firstUnitClause $ toExtendedFormula [[1], [-1, 2]]
Just (1,fromList [1])

>>> firstUnitClause $ eliminate 1 $ toExtendedFormula [[1], [-1, 2]]
Just (2,fromList [-1,2])

Above, after removing 1, we obtain the unitary clause {2}, which corresponds
The original clause { -1, 2}.
-}
firstUnitClause :: ExtendedFormula -> Maybe (Literal, Clause)
-- Set.Findmin returns the first item in a set, not being the list cannot use head
-- with the help of Foldr we override each clause until we reach the first convenient in the MAP (the smallest)
-- Map.foldrwithkey :: (K -> a -> b -> b) -> b -> map k a -> b
--  b = acc type, k = key type, a = type of value
-- From the declaration of the above function we realize what arguments the function has and the function
-- every step, function returns the new battery
firstUnitClause = Map.foldrWithKey funct Nothing
    where
        funct key value acc
            |   isUnitClause key = Just (Set.findMin key, value)
            |   otherwise = acc

{-
*** TODO ***

The following three functions must all be implemented as applications of the function 
process:

* decides
* Procespureliterals
* Processunitclauses.

Determine the formal parameters that they should receive process for 
to have all the necessary information. Ideally, if you capture cover in
process the common parts of the three functions above, they should 
become one-liners. The process process is not self -punished.

Hint: For the management of a content of a (Maybe a), your function may be useful 
Maybe.
-}
process :: (ExtendedFormula -> Maybe a) -> (a -> Action) -> (a -> ExtendedFormula -> ExtendedFormula) -> Bool -> History -> History
-- | Function 'process' is a higher-order function that generalizes the pattern
-- of finding elements in an extended formula, performing an action, and possibly
-- repeating this process.
--
-- Parameters:
--   * finder: Function that finds an element of interest in a formula (literal, unit clause, etc)
--   * actionCreator: Function that creates an Action from the found element
--   * eliminator: Function that eliminates elements from the formula
--   * repeat: Boolean flag indicating whether to repeat the process
--   * history: The current history of actions
--
-- The function handles two main cases:
--   1. When nothing is found (by the finder function), it returns the unchanged history
--   2. When something is found, it creates a new action, transforms the formula,
--      updates the history, and either repeats or returns based on the 'repeat' flag
process finder actionCreator eliminator repeat ((action, formula):rest) = 
    case finder formula of
        -- If nothing is found, return the unchanged history
        Nothing -> (action, formula):rest
        Just found -> 
            -- Create a new action from what was found
            let newAction = actionCreator found
                -- Transform the formula using the eliminator function
                newFormula = eliminator found formula
                -- Add the new action and formula to the history
                newHistory = (newAction, newFormula) : (action, formula) : rest
            -- Either process again or return the updated history
            in if repeat 
               then process finder actionCreator eliminator repeat newHistory
               else newHistory
-- Handle the empty history case
process _ _ _ _ history = history

{-
*** TODO ***

Implement the function decides, which eliminates the first (lowest) literally from a
extended formula.

The function receives as a parameter a historian of actions, possessing on the first 
the most recent entry position, and returns the new historian, obtained by 
adding an action decides, along with the extended formula on 
which produces it.
Constraints:

* The whole implementation must be a partial application of the function
  process.
* Use the point-free style.

Example:

>>> HV $ decide [(NOP, toExtendedFormula [[1, -2, 3], [-3, -1, 2]])]
Decide {getLiteral = -3} => [[-2,1]]
NOP => [[-3,-1,2],[-2,1,3]]

Mai sus, se decide eliminarea celui mai mic literal, -3.
-}
decide :: History -> History
-- | Function 'decide' implements a decision step in the DPLL algorithm.
-- It selects the smallest literal from the formula and eliminates it.
--
-- The function creates a 'Decide' action and adds it to the history.
-- Unlike pure literal and unit clause elimination, decide is non-deterministic
-- and may require backtracking if it leads to an unsatisfiable formula.
--
-- Implementation details:
-- - Uses process with a custom 'findSmallestLiteral' helper that:
--   1. Extracts all literals from the formula
--   2. Returns the smallest literal if any exist
-- - Creates a 'Decide' action for the found literal
-- - Eliminates the literal from the formula
-- - Does not repeat (repeat = False)
decide = process findSmallestLiteral Decide eliminate False
    where 
        -- Helper function to find the smallest literal in the formula
        -- Returns Nothing if the formula is empty
        findSmallestLiteral formula = 
              let literals = promote formulaLiterals formula
              in if Set.null literals then Nothing else Just (Set.findMin literals)

{-
*** TODO ***

Implement the procedureliterals function, which, repeatedly, determines and eliminates
The first (lowest) pure (lowest) purely of an extended formula. Continuous processing 
until no pure literal is found. It is possible as literals that
Initially they were not pure to become pure after the elimination of others.

The function receives as a parameter a history of actions,
possessing 
the most recent entry position, and returns the new historian, obtained by 
adding a proper pure action together with the extended formula on 
which produces it.

Constraints:

* The whole implementation must be a partial application of the function
  process.
* Use the point-free style.
* Use the FirstPureliteral function.

Example:

>>> HV $ processPureLiterals [(NOP, toExtendedFormula [[1, 2], [2, 3], [-3, 4], [-4, 3]])]
Pure {getLiteral = 2} => [[-4,3],[-3,4]]
Pure {getLiteral = 1} => [[-4,3],[-3,4],[2,3]]
NOP => [[-4,3],[-3,4],[1,2],[2,3]]

Above, the first pure literal is 1, and is eliminated. Then the first pure literal
It becomes 2, eliminated too.

>>> HV $ processPureLiterals [(NOP, toExtendedFormula [[1, 2], [1]])]
Pure {getLiteral = 1} => []
NOP => [[1],[1,2]]

Above, the first pure literal is removed, 1, which also leads to 
the default disappearance of the pure literal 2. Therefore, no more is added (pure 2)
to the history.

>>> HV $ processPureLiterals [(NOP, toExtendedFormula [[1, 2], [-2, 3], [-3, -2]])]
Pure {getLiteral = -2} => []
Pure {getLiteral = 1} => [[-3,-2],[-2,3]]
NOP => [[-3,-2],[-2,3],[1,2]]

Above, 1 is initially the only pure literal, and is eliminated. Then -2, which
Initially it was not pure, becomes pure, and is eliminated. The example demonstrates 
the need for repeated processing of the formula.
-}
processPureLiterals :: History -> History
-- | Function 'processPureLiterals' repeatedly finds and eliminates 
-- pure literals from the formula until none remain.
--
-- A pure literal appears with only one polarity in the entire formula.
-- Eliminating pure literals preserves satisfiability.
--
-- Implementation details:
-- - Uses process with firstPureLiteral as the finder
-- - Creates a 'Pure' action for each pure literal found
-- - Eliminates the literal from the formula
-- - Repeats until no more pure literals are found (repeat = True)
--
-- This process can be iterative because eliminating some literals
-- may cause others to become pure (as shown in the examples).
processPureLiterals = process firstPureLiteral Pure eliminate True

{-
*** TODO ***

Implement the process of Processunitclauses, which repeatedly determines and eliminates
The first (lowest) unitary clause from an extended formula. processing 
Continue until no unitary clause is found. It is possible that 
clauses that were initially not unitary to become unitary after eliminating some 
literal.
The function receives as a parameter a historian of actions, possessing on the first 
the most recent entry position, and returns the new historian, obtained by 
adding an appropriate united action along with the extended formula on 
which produces it.

Constraints:

* The whole implementation must be a partial application of the function
  process.
* Use the point-free style.
* Use the FIRSTUNITCLAUSE function.

Example:

>>> HV $ processUnitClauses [(NOP, toExtendedFormula [[1, 2], [1, 3], [2], [3]])]
Unit {getLiteral = 3, getClause = fromList [3]} => []
Unit {getLiteral = 2, getClause = fromList [2]} => [[1,3],[3]]
NOP => [[1,2],[1,3],[2],[3]]

Above, the first unitary clause is {2}, and is eliminated. Then the first clause 
The unitary becomes {3}, eliminated.

>>> HV $ processUnitClauses [(NOP, toExtendedFormula [[1], [-2, -1]])]
Unit {getLiteral = -2, getClause = fromList [-2,-1]} => []
Unit {getLiteral = 1, getClause = fromList [1]} => [[-2]]
NOP => [[-2,-1],[1]]

Above, {1} is initially the only unitary clause, and is eliminated. Then, 
Clause { -2, -1}, which initially was not unitary, becomes unitary after elimination
literal 1 of the formula, and is eliminated. Once again, notice that in 
The united action is stored the original clause { -2, -1} from which it was obtained 
the unitary clause { -2},
Do not clause itself. The example demonstrates 
the need for repeated processing of the formula.
-}
processUnitClauses :: History -> History
-- | Function 'processUnitClauses' repeatedly finds and eliminates
-- literals from unit clauses until no unit clauses remain.
--
-- A unit clause contains exactly one literal, which must be true
-- for the formula to be satisfiable.
--
-- Implementation details:
-- - Uses process with firstUnitClause as the finder, which returns both:
--   1. The literal from the unit clause
--   2. The original clause from which the unit clause was derived
-- - Creates a 'Unit' action that captures both the literal and original clause
-- - Eliminates the literal (not the clause) from the formula
-- - Repeats until no more unit clauses are found (repeat = True)
--
-- The unit propagation process continues because eliminating literals
-- can cause other clauses to become unit clauses.
processUnitClauses = process firstUnitClause (\(lit, clause) -> Unit lit clause) (\(lit, _) -> eliminate lit) True

{-
Example of a simple formula.
-}
formulaExample :: Formula
formulaExample = toFormula [[-4, -2, 3], [-3, 1, 2], [-1, 4]]

{-
Example of a history of actions performed on the formulaExample.

>>> HV historyExample
Pure {getLiteral = -3} => []
Unit {getLiteral = -1, getClause = fromList [-1,4]} => [[-3,2]]
Decide {getLiteral = -4} => [[-3,1,2],[-1]]
NOP => [[-4,-2,3],[-3,1,2],[-1,4]]

Initially, there are neither unitary, nor pure literal clauses; therefore, 
The first action must be a decision. It is observed that, as a result of the three 
Actions, the Vid formula is obtained, which is satisfying, as the formula 
original. It was sufficient to assume true literals -4, -3 and -1.
-}
historyExample :: History
historyExample =
    processPureLiterals $
    processUnitClauses $
    decide [(NOP, extendFormula formulaExample)]

{-
*** TODO ***

Implement the backtracktounitclause function, which returns to the history to the one
more distant point of the past in which the clue received as a parameter is unitary. 
In other words, if we go through the chronological list, from the temporal origin 
(last entry) to the present (first entry), the point of interest is the first 
met with the above property
. In addition, in the new historian obtained, the function 
Adds the new clause as if it were present from the beginning to the formula. 
The effect of the actions kept in the history must be reproduced on the new clause.

Constraints:

* Avoid explicit recursion by capitalizing on lists
  and the functions defined above.
* Use the Point Style-
free in the historical prame; You can explicit though
  the clause parameter.

Example:

>>> HV $ backtrackToUnitClause (toClause [4, 5]) historyExample
Decide {getLiteral = -4} => [[-3,1,2],[-1],[5]]
NOP => [[-4,-2,3],[-3,1,2],[-1,4],[4,5]]

Above, returns to the action (decides (-4)), canceling the effect of actions 
more recent (pure (-3)) and (units (-1) { -1, 4}). This is the first action in
Chronological sense that makes the clause {4, 5} become a unitary. She would have left
unitary and after more recent actions (units (-1) { -1, 4}) and (pure (-3)) but
We want to return as much as possible in the past. In addition, the versions 
The clause {4, 5} are added to the versions of the initial formula.
-}
backtrackToUnitClause :: Clause -> History -> History
-- | Function 'backtrackToUnitClause' implements a conflict-driven clause learning approach.
-- When given a clause, it:
-- 1. Finds the earliest point in history where this clause would become a unit clause
-- 2. Returns to that point, discarding later actions
-- 3. Inserts the clause into the formula at each step in the partial history
--
-- Parameters:
-- - clause: The clause to track through history (typically a learned conflict clause)
-- - history: The current history of actions
--
-- Implementation details:
-- - Reverses the history to process chronologically
-- - Defines applyAction to simulate the effect of actions on the clause
-- - Uses scanl to apply actions step-by-step to the clause
-- - Finds the first point where the clause becomes a unit clause
-- - Builds a new history up to that point, inserting the appropriate 
--   version of the clause at each step
--
-- The function handles several edge cases:
-- - Empty history: Returns empty list
-- - No unit clause found: Returns empty list
-- - Multiple possible points: Returns the earliest one
backtrackToUnitClause _ [] = []
backtrackToUnitClause clause history =
    let 
        -- Reverse history to process chronologically (from past to present)
        revHistory = reverse history
        
        -- Helper function that simulates how an action affects a clause
        -- This captures the transformative effects of different actions:
        applyAction :: Action -> Clause -> Clause
        -- If the literal is in the clause, the clause is satisfied (empty)
        -- Otherwise, remove its complement from the clause
        applyAction (Decide lit) c = if Set.member lit c then Set.empty else Set.delete (-lit) c
        applyAction (Pure lit) c = if Set.member lit c then Set.empty else Set.delete (-lit) c
        applyAction (Unit lit _) c = if Set.member lit c then Set.empty else Set.delete (-lit) c
        -- NOP doesn't change the clause
        applyAction NOP c = c
        
        -- Apply each action to the clause sequentially, tracking how it evolves
        -- The first element is the original clause, followed by transformed versions
        resultClauses = tail $ scanl (\c (a, _) -> applyAction a c) clause revHistory
        
        -- Find the first index where the clause becomes a unit clause
        unitIndex = findIndex isUnitClause resultClauses
    in case unitIndex of
        -- If the clause never becomes a unit clause, return empty list
        Nothing -> []
        Just idx -> 
            -- Take only the relevant portion of history (up to the unit clause point)
            let relevantHistory = take (idx+1) revHistory
                -- The unit clause at the found point
                unitClause = resultClauses !! idx
                
                -- For each point in the relevant history:
                -- 1. Get the transformed clause at that point
                -- 2. Insert it into the formula
                -- This effectively adds the clause to all formulas in the history
                updatedHistory = zipWith (\(a, f) i -> 
                    let transformedClause = resultClauses !! i
                    in (a, Map.insert transformedClause clause f)) 
                    relevantHistory [0..]
                
            -- Reverse back to anti-chronological order (newest first)
            in reverse updatedHistory