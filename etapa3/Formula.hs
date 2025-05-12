module Formula where

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

{-
O variabilă booleană este reprezentată printr-un întreg pozitiv: 1, 2 etc.

type introduce un sinonim de tip, similar cu typedef din C.
-}
type Variable = Int

{-
Un literal boolean desemnează o variabilă sau negația acesteia.

Este reprezentat printr-un întreg nenul: 1, 2, -1, -2 etc. Polaritatea 
literalului este surprinsă de semnul întregului; de exemplu, literalul 1 
desemnează variabila 1, iar literalul -1, negația acesteia. Întregul 0 este
exclus pentru că ar împiedica distingerea polarităților.
-}
type Literal = Int

{-
O clauză este o disjuncție de literali.

Este reprezentată ca o mulțime ordonată de literali. Această 
reprezentare surprinde natural unicitatea și irelevanța ordinii literalilor din 
clauză, și permite căutarea eficientă.
-}
type Clause = Set Literal

{-
O formulă CNF este o conjuncție de clauze.

Este reprezentată ca o mulțime ordonată de clauze.
-}
type Formula = Set Clause

{-
O interpretare este o mulțime de literali asumați adevărați.

De exemplu, interpretarea {1, -2} asumă că literalii 1 și -2 sunt adevărați,
adică variabila 1 este adevărată, și variabila 2, falsă.
-}
type Interpretation = Set Literal

{-
Construiește o clauză dintr-o listă de literali.
-}
toClause :: [Literal] -> Clause
-- face din lista un set: ordoneaza, elimina duplicatele
toClause = Set.fromList

{-
Construiește o formulă dintr-o listă de liste de literali.
-}
toFormula :: [[Literal]] -> Formula
-- un set este o lista de elemente unice, ordonate si din modul in care este reprezentat in
-- sir de caractere: apare fromList in fata setului.
-- Formula este un set de seturi de literali,
-- fiecare set este unic si sortat dupa marime si apoi dupa elemente
toFormula = Set.fromList . map toClause
-- EX:
-- [[1, 2], [-1, 3]]
-- (1 ∨ 2) ∧ (¬1 ∨ 3)

{-
Transformă o formulă într-o listă de liste de literali, pentru lizibilitate.
-}
toLiteralLists :: Formula -> [[Literal]]
-- pe rand ajungem de la un set de seturi la o lista de seturi si apoi o lista de liste
toLiteralLists = map Set.toList . Set.toList

{-
*** TODO ***

Implementați funcția literalVariable, care determină variabila referită de un
literal.

CONSTRÂNGERI:

* Utilizați stilul point-free.

Exemple:

>>> literalVariable 1
Prelude.undefined:\Users\punga\AppData\Local\Temp\extF7A5: withFile: does not exist (No such file or directory):\Users\punga\AppData\Local\Temp\extF7A5: withFile: permission denied (Permission denied)

>>> literalVariable (-1)
Prelude.undefinedrelude.undefinedrelude.undefined

Observați plasarea lui (-1) între paranteze. Absența lor, ca în
literalVariable -1, ar fi determinat scăderea lui 1 din literalVariable, 
operație fără sens.
-}
literalVariable :: Literal -> Variable
literalVariable = abs

{-
*** TODO ***

Implementați funcția isPositive, care verifică dacă polaritatea unui literal 
este pozitivă.

CONSTRÂNGERI:

* Utilizați stilul point-free.

Exemple:

>>> isPositive 1
True

>>> isPositive (-1)
False
-}
isPositive :: Literal -> Bool
isPositive = (> 0)

{-
*** TODO ***

Implementați funcția isNegative, care verifică dacă polaritatea unui literal 
este negativă.

CONSTRÂNGERI:

* Utilizați stilul point-free.

Exemple:

>>> isNegative 1
False

>>> isNegative (-1)
True
-}
isNegative :: Literal -> Bool
isNegative = (0 >)

{-
*** TODO ***

Implementați funcția complement, care determină literalul complementar.

CONSTRÂNGERI:

* Utilizați stilul point-free.

Exemple:

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

Implementați funcția formulaLiterals, care determină mulțimea tuturor 
literalilor dintr-o formulă.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free.

Exemple:

>>> formulaLiterals $ toFormula [[1, -2, 3], [-1, 2, 3]]
fromList [-2,-1,1,2,3]

Observați cum literalii sunt implicit ordonați în mulțime.
-}
formulaLiterals :: Formula -> Set Literal
-- Set.union primeste 2 multimi si le combina: initial setul gol cu primul set,
-- si apoi rezultatul cu urmatorul set
formulaLiterals = Set.foldl Set.union Set.empty

{-
*** TODO ***

Implementați funcția clauseVariables, care determină mulțimea tuturor 
variabilelor dintr-o clauză.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free.

Exemple:

>>> clauseVariables $ toClause [1, -2, -1]
fromList [1,2]
-}
clauseVariables :: Clause -> Set Variable
-- actualizeaza setul intial cu rezultatele aplicari
-- functiei pe fiecare element si ulterior elimina duplicatele
clauseVariables = Set.map literalVariable -- abs

{-
*** TODO ***

Implementați funcția formulaVariables, care determină mulțimea tuturor 
variabilelor dintr-o formulă.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free.

Exemple:

>>> formulaVariables $ toFormula [[1, -2], [-1, 2, 3]]
fromList [1,2,3]
-}
formulaVariables :: Formula -> Set Variable
-- rexultatul lui set.map va fi un set de seturi, deci trebuie sa le unim
-- Set.unions aici este echivalent cu formulaLiterals
formulaVariables = Set.unions . Set.map clauseVariables

{-
*** TODO ***

Implementați funcția isPureLiteral, care verifică dacă un literal este pur 
într-o formulă, în sensul absenței complementului său din acea formulă.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free în parametrul formulă; puteți totuși explicita
  parametrul literal.

Exemple:

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
-- se presupune ca literal se afla in formula
-- and intoarece un bool ca rezultat la aplicarea pe toate boolurile
-- cu Set.notMember caut daca nu exista opusul lui literal si
-- returneaza un bool pentru fiecare set cu ajutorul lui Set.map
-- and [] = True
isPureLiteral literal = and . Set.map (Set.notMember (complement literal))

{-
*** TODO ***

Implementați funcția isUnitClause, care verifică dacă o clauză este unitară,
i.e., conține un singur literal.

CONSTRÂNGERI:

* Utilizați stilul point-free.

Exemple:

>>> isUnitClause $ toClause [1]
True

>>> isUnitClause $ toClause [1, 2]
False
-}
isUnitClause :: Clause -> Bool
isUnitClause = (1 ==) . Set.size

{-
*** TODO ***

Implementați funcția isValidClause, care verifică dacă o clauză este 
întotdeauna adevărată, în sensul că ea conține atât un literal, cât și 
complementul acestuia.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați secțiuni, adică aplicații parțiale infixate, de forma (x `f`) sau
  (`f` y).

Hint: funcționala any.

Exemple:

>>> isValidClause $ toClause []
False

>>> isValidClause $ toClause [-1, 1, 2]
True

>>> isValidClause $ toClause [1, 2]
False
-}
isValidClause :: Clause -> Bool
-- any f clause = adevarat daca cel putin un element respecta f
-- Set.member x clause
-- fara `` Set.member ar fi luat primul parametru setul in loc de element,
-- acum creeaza o functie curry care asteapta doar elementul

-- creez setul cu elementele complementare ale lui clause cu map
-- verific cate un element din setul creat daca se afla in setul initial cu any
isValidClause clause = any (`Set.member` clause) (Set.map complement clause)

{-
*** TODO ***

Implementați funcția isValidFormula, care verifică dacă o formulă este 
întotdeauna adevărată, în sensul că toate clauzele sale sunt întotdeauna
adevărate.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free.

Hint: funcționala all.

Exemple:

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

Implementați funcția satisfiesFormula, care verifică dacă o interpretare 
satisface o formulă, în sensul că, asumând adevărați literalii din 
interpretare, formula este adevărată.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free în parametrul formulă; puteți totuși explicita
  parametrul interpretare.

Exemple:

>>> satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2]]
True

>>> satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2], [2]]
False

>>> satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2], [-2]]
True
-}
satisfiesFormula :: Interpretation -> Formula -> Bool
-- in interpretation sunt numere in valoarea lor de adevar,
-- asta inseamana ca daca apare opusul numarului reprezinta fals
-- Iau mai intai fiecare clauza si apoi fiecare element din clauza
-- pe care il caut in interpretation penttru a vedea daca este adevarat
-- Daca am gasit unul atunci toata clauza este adevarata.
-- Daca toate clauzele sunt adevarate atuncti formula este satisfacuta
satisfiesFormula interpretation = all (any (`Set.member` interpretation))

{-
*** TODO ***

Implementați funcția interpretations, care generează lista tuturor 
interpretărilor aferente unei mulțimi de variabile.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.
* Utilizați stilul point-free.
* Utilizați list comprehensions.

Exemple:

>>> interpretations $ Set.empty
[fromList []]

>>> interpretations $ Set.fromList [1]
[fromList [1],fromList [-1]]

>>> interpretations $ Set.fromList [1, 2]
[fromList [1,2],fromList [-2,1],fromList [-1,2],fromList [-2,-1]]
-}
interpretations :: Set Variable -> [Interpretation]
-- toList doar formateaza setul in lista
-- functia lambda transforma un numar in lista de 2 numere adaugand si complementarul
-- sequence primeste o lista de liste si returneaza toate combinatiile posibile
-- formatam inapoi in set
interpretations = map Set.fromList . sequence . (\x -> [[-y, y] | y <- x]) . Set.toList

{-
*** TODO ***

Implementați funcția isSatisfiable, care verifică dacă o formulă este 
satisfiabilă, adică adevărată în cel puțin o interepretare.

CONSTRÂNGERI:

* Evitați recursivitatea explicită, valorificând funcționalele pe mulțimi
  și funcțiile definite mai sus.

Exemple:

>>> isSatisfiable $ toFormula [[1, 2], [-2]]
True

>>> isSatisfiable $ toFormula [[1], [-1]]
False

>>> isSatisfiable $ toFormula []
True

>>> isSatisfiable $ toFormula [[]]
False

ÎNTREBĂRI:

1. Cum contribuie evaluarea leneșă la explorarea eficientă a interpretărilor?
2. Ce s-ar fi întâmplat dacă interpretations întorcea o mulțime (Set) de 
   interpretări în locul unei liste? Utilizați modulul Debug.Trace, deja 
   importat, pentru a vizualiza interpretările generate în cele două situații.
   
   Pentru a simula întoarcerea unui mulțimi din interpretations este suficient 
   să aplicați Set.fromList asupra listei întoarse de implementarea curentă în 
   corpul lui isSatisfiable.

RĂSPUNS: ...
-}
isSatisfiable :: Formula -> Bool
-- Verificări dacă există cel puțin o interpretare care face ca formula să fie adevărată
-- Mai întâi primește toate variabilele în formulă folosind formulavariabile
-- apoi generează toate interpretările posibile pentru acele variabile
-- În sfârșit, verifică dacă oricare dintre aceste interpretări satisface formula
isSatisfiable formula = any (`satisfiesFormula` formula) $ interpretations $ formulaVariables formula
