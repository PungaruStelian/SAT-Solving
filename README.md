# SAT-Solving

## Stage 1:

In SAT, we work with boolean formulas in conjunctive normal form (CNF), which denote conjunctions of disjunctions of boolean literals. Specifically, we use the following terms:

- **Boolean variable**: x₁, x₂, etc.
- **Boolean literal**: a variable or its negation: x₁, ¬x₂, etc.
- **Clause**: disjunction of literals: (x₁ ∨ ¬x₂ ∨ x₃), etc.
- **CNF formula**: conjunction of clauses: (x₁ ∨ ¬x₂ ∨ x₃) ∧ (¬x₄ ∨ x₅), etc.
- **Interpretation**: set of literals assumed to be true: {x₁, ¬x₂}, meaning variable x₁ is assumed true, and x₂ is false.

Additionally, we use the following terms:
- **Unit clause**: a clause with a single literal: (x₁), etc.
- **Pure literal** with respect to a formula: a literal whose complement does not appear in that formula: ¬x₁ is pure with respect to the formula (¬x₁ ∨ ¬x₂) ∧ (¬x₁ ∨ x₂), as x₁ does not appear, but x₂ and ¬x₂ are not pure.

A SAT instance, represented by a CNF formula, aims to determine the satisfiability of the formula, meaning the existence of an interpretation with respect to which the entire formula is true. If such an interpretation exists, we also want to identify it.

Essentially, a CNF formula is satisfiable if we can identify at least one true literal in each clause (property P).

Examples:

- The formula (x₁ ∨ ¬x₂ ∨ x₃) ∧ (¬x₄ ∨ x₅) is **satisfiable**, with multiple interpretations fulfilling property (P): {x₁, ¬x₄}, {x₁, ¬x₂, x₅}, etc.
- The formula (x₁ ∨ ¬x₁ ∨ x₂) ∧ (¬x₃ ∨ x₃) is **valid/tautological**, meaning it's true in all interpretations, therefore satisfiable.
- The formula (x₁) ∧ (¬x₁) is **contradictory**, meaning it's false in all interpretations, therefore unsatisfiable.

Special Cases:

- The empty formula is **satisfiable**, as property (P) is trivially fulfilled by the absence of clauses.
- A formula containing the empty clause is **unsatisfiable**, as the empty clause cannot contain a literal assumed to be true in an interpretation.

The first stage of this assignment addresses the Haskell representations of the above concepts, defining basic functions that operate on these representations, and implementing a naive mechanism for checking satisfiability based on enumerating interpretations.

We use the following representations:

- A **variable** is represented by a positive integer: x₁ becomes `1`.
- A **literal** is represented by a non-zero integer, whose sign reflects the polarity of the literal: x₁ becomes `1`, and ¬x₁ becomes `-1`.
- A **clause** is represented by a set of literals: (x₁ ∨ ¬x₂ ∨ x₃) becomes `{1, -2, 3}`.
- A **formula** is represented by a set of clauses: (x₁ ∨ ¬x₂ ∨ x₃) ∧ (¬x₄ ∨ x₅) becomes `{{1, -2, 3}, {-4, 5}}`.
- An **interpretation** is represented by a set of literals: {x₁, ¬x₂} becomes `{1, -2}`.

Given that, within a clause, a literal appears at most once, and the order of literals is irrelevant, we use ordered sets of literals instead of lists for representing a clause. For this, we use the `Data.Set` module.

```haskell
import Data.Set (Set)
import qualified Data.Set as Set
```

The significance is as follows:

- The first line states that only the Set type constructor can be used as is.
- The second line states that all other entities imported from the module must be prefixed with `Set.`. For example, `Set.map` denotes the functional on sets, while `map` continues to refer to the standard functional on lists.

The language constructs and mechanisms you will leverage in the solution are:

- Sets and lists
- Functionals on sets and lists, to observe the universality of these constructions
- Sections, meaning partially applied infix applications, such as `(x `f`)` or ``(`f` y)``
- List comprehensions, for concise descriptions of operations
- Lazy evaluation, implicit in Haskell, for conceptual decoupling of transformations within a sequence
- Local variable binding constructs (`let` or `where`)

The module of interest from the skeleton is `Formula`, which contains the representation of the above concepts, as well as the operations you need to implement. You can find details about functionality and implementation constraints, as well as examples, directly in the file. You need to complete the definitions that start with `*** TODO ***`.

To run the tests, load the `TestFormula` module into the interpreter and evaluate `main`.

It is sufficient for the archive for vmchecker to contain only the `Formula` module.

### Penalties

The comments of most functions in the skeleton contain CONSTRAINTS, the non-compliance with which attracts partial deductions.

In addition, direct operation on the set representation of clauses, formulas, and interpretations is required. **Intermediate conversions to lists and back to sets will result in the total deduction of points for the functions implemented in this way!** The predefined function `toLiteralLists` in the skeleton is provided only to more easily visualize formulas; it is not intended to be used in implementations.

## Stage 2:

Stage 1 involved the implementation of a simple mechanism of verifying satisfyability, based on the enumeration of interpretations. A disadvantage of this approach is the complete generation of an interpretation, in the sense of covering all the variables in the formula,
Although the assumptions about the true literals made up to a given time can already invalidate the formula, regardless of the assumptions made later for the rest of the variables. For example, for formula `(x1) ∧ (¬x1 ∨ x2)`, assumption that the literal ¬x1 is true (called, in short, the assumption ¬x1) invalidates the formula (more precisely, the first clause),
regardless of the assumption made later for the variable X2.

Therefore, it would be more advantageous to observe the state of the formula after each new assumption. Thus, once we assume a true literal, the corresponding variable can be eliminated from the formula, according to the following principles:

- All clauses containing the literal become true and can be removed from the formula.
- In the remaining clauses, all the occurrences of the literal complement become false and, not being able to contribute to the satisfaction of the clauses that contain them, can be removed.

For example, for the formula `(x1 ∨ x2) ∧ (¬x1 ∨ x3)` and assumption x1, we have the following:

- The clauses containing the literal x1, namely (x1 ∨ x2), are eliminated, obtaining the formula (¬x1 ∨ x3).
- The appearances of the literal complement, namely ¬x1, can be eliminated from the remaining clauses, obtaining the formula (x3).

Following an elimination sequence, the following situations described in stage 1 can be obtained:
- The empty formula, following the satisfaction and removal of all the clauses, which means that the original formula is satisfied on the basis of the assumptions made.
- The formula that contains the empty clause, following the removal of all the literals in a clause on the grounds of their falsity, which means that the clause can no longer be satisfied, and so the original formula can no longer be satisfied on the basis of current assumptions.

For example, in the formula above:
- If after assignment X1, which led to the formula (x3), we make the additional assumption X3, we obtain the empty formula, so the interpretation {x1, x3} satisfies the original formula, regardless of the assumption for the variable X2.
- If instead of assignment X3, we make its opposite, ¬x3, we obtain a formula with the empty clause, (), which can no longer be satisfied.

In the second case, conflict, we must return through backtracking to a previous assumption and try its opposite. The essence of a more efficient resolution is the strategy of choosing the next assumption, both at present, when a new variable is eliminated and in the past, when it is necessary to return to a variable already eliminated.

In order to better understand the situations encountered in making an assumption at present, to take the formula from the beginning of this stage: (x1) ∧ (¬x1 ∨ x2). If we accidentally choose the following assumption, for example, ¬x1, we risk carrying out a unnecessary calculation process, as the clause (X1) would immediately become false. Instead,
At a closer analysis, we notice that there is only one way to meet a unitary clause, and so the X1 assumption is imposed. Therefore, it is a good idea to first detect the presence of unitary clauses, and to satisfy them.

It should be noted that the satisfaction of a unitary clause can generate new unitary clauses,
who did not have this property initially. For example, in the previous formula, assumption X1 leads by elimination to formula (x2), in which the unitary clause (x2) appears, absent from the original formula. Therefore, the processing of unitary clauses must be repeated until their absence from the formula.

Another interesting situation concerns pure literals.
Since the complement of a literal does not appear in the formula, it is always advantageous to assume that literal, without fear of inducing any conflict. As with the unitary clauses, the elimination of a pure literal can generate new pure literals, so the processing must be repeated. For example, in the formula `(x1 ∨ x2) ∧ (¬x2 ∨ x3) ∧ (¬x2 ∨ ¬x3)`,
There are no unitary clauses, but there is only one pure literal, x1. By eliminating it, the formula `(¬x2 ∨ x3) ∧ (¬x2 ∨ ¬x3)` is obtained, in which the pure literal ¬x2 appears, which was not pure in the original formula. By eliminating this, the empty formula is obtained.
It should be noted that the two processing can interact: the satisfaction of a unitary clause can introduce not only other unitary clauses, but also pure literals. If both options are available, **the unitary clauses are preferred first**, as they can lead to conflicts,
And it is desirable to highlight them as early as possible, in order to save another calculation effort that would not prevent the generation of a conflict anyway. Only if the formula does not contain neither unit, nor pure literal clauses, it is pointless to resort to any assumptions.

In the final example, we demonstrate all three types of processing on the formula `(¬x1 ∨ x4) ∧ (x1 ∨ x2 ∨ ¬x3) ∧ (¬x2 ∨ x3 ∨ ¬x4)`:

- Initially there are no unitary, nor pure literal clauses, so we accidentally assume the literal ¬x4, which leads by elimination to the formula (¬x1) ∧ (x1 ∨ x2 ∨ ¬x3).
- Now, the unitary clause (¬x1) is highlighted, and we prioritize its satisfaction, although there are pure literals x2 and ¬x3. The elimination of ¬x1 leads to the formula (x2 ¬ ¬x3).
- Finally, there are no unitary clauses, so we eliminate the pure literal ¬x3 and obtain the empty formula, which corresponds to the satisfaction of the original formula under the interpretation {¬x1, ¬x3, ¬x4}.

Conflicts (empty clauses) and more efficient choices in case of return to previous assumptions will be addressed in stage 3.

The final resolution algorithm uses both the current variant of the formula, which captures the eliminations made up to a given time, as well as the original version. Therefore,
The mere representation of a formula like a lot of clauses, as in stage 1, becomes insufficient for modeling the entire necessary information. Fortunately, a small extension covers the new need: instead of a crowd, we use an associative painting (`Data.map`, consult this tutorial), in which `the keys are the current clauses, and the values, original clauses`. Each current clause (keys) corresponds to the original clause (value) from which it was obtained by eliminations.

For example, the formula (x1 ∨ x2) ∧ (¬x1 ∨ x3) is initially represented, before any elimination, by the painting {({1, 2}, {1, 2}), ({-1, 3}, {-1, 3})}, where I used the notation (clause-current, clause-
original) to designate a key-value correspondence. Obviously, before any elimination, the keys and values ​​coincide. Eliminating the literal 1, as demonstrated above, the new painting is obtained, {({3}, {-1, 3})}, in which the original clause {1, 2} was removed, and in which the current variant of the original clause {-1, 3} is {3}.
The second stage of the theme addresses these extended representations of the formulas, the mechanism of elimination of literals, and the processing of unitary clauses and pure literals.

Similar to stage 1, the functions on paintings must be prefixed with MAP. We note, however, that the functions with standard names on paintings (map.map, map.filter, etc.) operate on values.
You will rather need the functions that also consider the keys (map.mapkeys, map.filterwithkey, map.foldrwithkey, etc.), as they are subject to transformations, while values ​​remain unchanged.

The constructions and language mechanisms that you will exploit in the resolution, besides the ones in stage 1, are:

- **associative paintings**
- **functional** on matrices
- **user data types** (DATE)
- **pattern matching**.

The module of interest in the skeleton is extendedformula, which contains the extended representation of the formulas, as well as the operations you need to implement. Find the details about functionality and implementation constraints, as well as examples, directly in the file. You have to complete the definitions that start with *** Todo ***.
For testing tests, load in the interpreter Testextendedformula module and evaluate Main.

It is sufficient for the Vmchecker Archive to contain the ExtendFormula and Formula 1 modules.

### Penalties

The comments of most functions in the skeleton contain constraints, whose non -compliance attracts partial deposits.

In addition, it is necessary to operate directly on the representations of crowds or paintings of clauses, formulas and interpretations.
**The intermediate conversions to the lists and back to the crowds or paintings attract the total deposition of the functions implemented in this way!**

## Stage 3:

Stage 2 has addressed the situations in which a formula can be satisfied only going "before", choosing one of the three types of elimination actions, in this order, of the unitary clauses, the pure literals and the literals. It remains to be determined how to proceed when, at a time,
The elimination of a literal produces a vacuum clause (conflict).

For example, starting from the formula (¬x7 ∨ x1) ∧ (¬x6 ¬ ¬x2 ∨ x1) ∧ (¬x5 ∨ x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1 ∨ x6) ∧ (¬x2 ∨ x5 ∨ x7) ∧ (¬x1 x2) ∧ (x1 ∨ x2), the following sequence of actions is applied first, as discussed in stage 2:

1. For example, starting from the formula (¬x7 ∨ x1) ∧ (¬x6 ¬ ¬x2 ∨ x1) ∧ (¬x5 ∨ x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1 ∨ x6) ∧ (¬x2 ∨ x5 ∨ x7) ∧ (¬x1 x2) ∧ (x1 ∨ x2), the following sequence of actions is applied first, as discussed in stage 2:
Cum nu există clauze unitare sau literali puri,
It is first decide to eliminate the some literal ¬x7, which produces the formula: (¬x6 ¬ ¬x2 ∨ x1) ∧ (¬x5 ∨ x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1 ∨ x6) ∧ (¬x2 ∨ x5) ∧ (¬ x2) ∧ ∧ ∧ (x1 ∨ x2).
2. Similarly, the elimination of some literal ¬x6 produces the formula: (¬x5 ∨ x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1) ∧ (¬x2 ∨ x5) ∧ (¬x1 ∨ x2) ∧ (x1 ∨ x2).
3. Then, the elimination of some literal ¬x5 produces the formula: (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1) ∧ (¬x2) ∧ (¬x1 ∨ x2) ∧ (x1 ∨ x2).
4. At this moment the unitary clause (¬x2) is highlighted, which leads by elimination to the formula: (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x1) ∧ (x1).
5. Further, any of the unitary clauses (¬x1) or (x1) would be removed, s
-You get a vacuum clause (conflict).

What should be done in this case? Intuitive, it should be returned by backtracking to an previous assumption and explored its opposite. But this raises another question: At what decision point should we return? Obviously, the unit clauses impose the choice, and it would not make sense to try the opposite; for example,
The return to the assumption ¬x2 (step 4 above), dictated by the unitary clause (¬x2), and the exploration of the X2 variant would be useless. Therefore, we should return to the latest decision -making point in which there is a viable alternative; For example, we could return to the assumption ¬x5 (step 3), arbitrary, and explore the assumption X5. Unfortunately,
And this path would lead to conflict, and a level should be returned to the assumption ¬x6 (step 2).

The following interesting question arises: could we return to a more precise point in the past, so that we avoid trying at least a few alternatives that would ultimately lead to the conflict? In the above example,
Both the initial sequence ¬x7 - ¬x6 - ¬x5, as well as the alternative sequence ¬x7 - ¬x6 - x5 led to conflicts. Could we completely avoid the second?

The basis of the answer is the observation that a conflict can be induced early, by a certain combination of incompatible assumptions made in the past, but manifested late.
In the example above, the combination of incompatible assumptions is {¬x7, ¬x5}, which leads to the unitary clause (¬x2), from the original clause (¬x2 ∨ x5 ∨ x7). Its elimination leads to both ways above conflicts. Therefore, it should be returned to the most distant point in the past where the incompatibility is removed,
so that all the assumptions made later on other variables are harmonized with the modification made. This does not completely prevent the emergence of other conflicts as new assumptions are made, but it reduces the search space.

We note that since the unitary clauses impose the eliminated literals, as a result of previous assumptions,
They will not appear in the conflict combination ("we had only to eliminate them"). We are interested in identifying the literals for which the opposite assumption was viable.

Obviously the questions remain:
1. How do we determine incompatible assumptions?
2. How do we determine the point where we return?

### Learning new clauses

To answer the question (1), we first enter the concept of resolution of two clauses, which will be formally presented in the logical programming chapter. For the theme, it is sufficient to define it as follows: if a C1 clause contains the XI literal, and a C2 clause, its complement, ¬xi (or vice versa),
The two clauses can be resolved in relation to the respective literals, obtaining a new clause, R (resolved), which contains all the literals of the two clauses, less XI and ¬XI. In addition, from a logical point of view, we have that (C1 ∧ C2) ⟹ R, ie R is true whenever C1 and C2 are.

For example:
- The clauses (x1 ∨ x2) and (¬x1 ∨ x3 ∨ x4) can be solved in relation to the variable x1, producing the clause (x2 ∨ x3 ∨ x4).
- The clauses (x1 ∨ x2) and (x1 ∨ x3 ∨ x4) cannot be solved in relation to any variables, as there is no literal in the first clause whose complement will appear in the second.

Using the resolution, we introduce the following algorithm to determine incompatible assumptions:

1. When obtaining a empty clause, the original clause from which it came, which serves as a current clause, is determined.
2. The history of assumptions made in the anticronological (present-reaches) and:
- If the current action is to eliminate a unit clause (units), we solve (if possible,
According to the above definition of the resolution) the clause stored in action with the current clause, in relation to the literal stored in the action, resulting in a new current clause.
- If the current action is of a different nature, the current clause remains unchanged.
3. The final variant of the current clause summarizes the reason for the conflict.

By solving only in relation to the literals imposed by the elimination of the unitary clauses, we obtain the desired effect that they do not appear in the combination of incompatible assumptions. The need to operate on the original clauses justifies the extended formulas of stage 2.

Let's apply the algorithm to the example above, operating this time in reverse order, from Step 5
, which generated the conflict, towards step 1:

- Assuming that it was decided to eliminate the unitary clause (¬x1), which led to the clause (X1) (conflict), the original clause is determined from which this vacuum clause was obtained, namely (x1 ∨ x2), which becomes the current clause. Then, it begins to consult the actions already performed, according to the algorithm above. First,
The most recent action is analyzed, to eliminate the unitary clause (¬x1), which stores in its representation ¬x1 and the original clause from which this unitary clause was obtained, namely (¬x1 ∨ x2). Solving this with the current clause (x1 ∨ x2) in relation to the ¬x1 literal, the resolution (x2) is obtained,
representing the new current clause.

- Returning another step in the past, the action of eliminating the unitary clause (¬x2) is encountered, which stores in its representation ¬x2 and the original clause from which this unitary clause was obtained (¬x2 ∨ x5 ∨ x7). Solving this with the current clause (x2) in relation to the ¬x2 literal, the resolution (x5 ∨ x7) is obtained,
representing the new current clause.

- The remaining actions are not to eliminate a unitary clause, and therefore the current clause remains (x5 ∨ x7).

In order to understand the significance of this clause, (x5 ∨ x7), let us remember the combination of incompatible assumptions detected above, {¬x7, ¬x5}, according to which both variables, x7 and x5, have been assumed false. In order to overcome the incompatibility, at least one of the variables must be assumed, that is exactly what the clause codes (x5 ∨ x7).
The property of the resolution states that this is always true, if the clauses on which it has been obtained are true. Therefore, if we want the empty clause (x1 ∨ x2) and the clauses that have become unitary and eliminated (¬x1 ∨ x2) and (¬x2 ∨ x5 ∨ x7) be simultaneously true (in a certain interpretation),
Then the clause (x5 ∨ x7) must be true (in that interpretation). Notice how the clause really refers only for variables on which the arbitrary assumptions were made, such as X5 and X7, not variables for which the assumptions were imposed, as X2.

(x5 ∨ x7) bears the name of learned clause (Learned Clause),
whereas is not part of the original formula. If the clause learned itself is empty, it means that it is impossible for the clauses from which it was obtained is true simultaneously, and so the original formula is unsatisfied.

To ensure that the new clause, (x5 ∨ x7), will never be emptied by the assumptions made,
We can add it to the original formula, and we can return to the past to the most distant point where this clause becomes unitary (the backtracktounitclause function in stage 2), so that the next action will satisfy it immediately. The return can be done with an arbitrary number of steps in the past,
which is why it is called necronological backtracking. The return to the most distant point, and not the most recent, decreases the probability of failure of the current path, due to other assumptions made in the previous failed path. Thus, we also answer the question (2). We notice that,
Since the resolution derives logically from certain clauses of the original formula, the formula resulting by adding the clause learned to the original formula is equivalent to the original formula.

In the example above, the learned clause (x5 ∨ x7) is added to the original formula, obtaining
se formula (¬x7 ∨ x1) ∧ (¬x6 ∨ ¬x2 ∨ x1) ∧ (¬x5 ∨ x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1 ∨ x6) ∧ (¬x2 ∨ x5 ∨ x7) ∧ (¬x1 ∨ x2) ∧ (x5 ∨ x7):

1. Necronological backtracking returns to step 1, immediately after eliminating the literal ¬x7, after which the clause learned, (x5 ∨ x7), becomes unitary (x5). The new formula is (¬x6 ¬ ¬x2 ∨ x1) ∧ (¬x5 ∨ x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1 ∨ x6) ∧ (¬x2 ∨ x5) ∧ (¬x1 ∨ x2) ∧ (x1 ∨ x2) ∧ (x2) ∧ (x2).
2. The new step 2 is different now,
whereas the presence of the unitary clause (x5) determines its elimination and obtaining the formula (¬x6 ¬ ¬x2 ∨ x1) ∧ (x1) ∧ (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ ¬x1 ∨ x6) ∧ (¬x1 ∨ x2) ∧ (x1 ∨ x2). This is the point where the learned clause produces its effects.
3. The unitary clause (x1) is removed and the formula (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (¬x2 ∨ x6) ∧ (x2) is obtained.
4. The unitary clause (x2) is removed and the formula (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) ∧ (x6) is obtained.
5. The unitary clause (x6) is removed and the formula (¬x4 ∨ x3) ∧ (¬x3 ∨ x4) is obtained.
6. The some literal ¬x4 is assumed and the formula (¬x3) is obtained.
7. The unitary clause (¬x3) is removed and the vacuum formula is obtained. SUCCESS!

Note how the unproductive alternative combination mentioned above, ¬x7 - ¬x6 - x5, has been avoided, by the achievement of assumption X5 before assumption for variable X6.

### The complete satisfaction algorithm

We have reached the point where we can integrate all the concepts discussed so far, in the complete satisfaction algorithm:

1. All unitary clauses are processed.
2. If the empty formula is obtained, the original formula is satisfying and the interpretation is built using the current history. STOP.
3. If the formula contains the vacuum clause (conflict), a new clause is learned.
- If the learned clause is empty, the formula is unsatisfied. STOP.
- Otherwise, it returns to the history at the most distant point where the clause learned is unitary, and is jumping in step 1.
4. All pure literals are processed and jump in step 1.
5. Only if there are no pure literals, a literal is assumed and it is jumping in step 1.

### Application to the 3-Coloring problem

The problem 3-collar of an unorieved graph follows the association of a color of three (Red, Green, Blue) each node in the graph, so that any two adjacent nodes are colored differently. The problem can be solved by discount on the village, traveling the stages:

1. Coding the 3-coloring court in a CNF formula.
2. Satisfying the formula, if possible.
3. Decoding the possible interpretation that satisfies the formula to obtain the coloring of the graph.

We will use the next scheme of coding a graph in a CNF formula:

1. Each node N of the graph corresponds to three Boolean variables, related to the three possible colors: x [n, r], x [n, g], x [n, b].
2. Each node N has at least one color: (x [n, r] ∨ x [n, g] ∨ x [n, b]).
3. Each node N has at most one color: (¬x [n, r] ¬ ¬x [n, g]) ∧ (¬x [n, r] ∨ ¬x [n, b]) ∧ (¬x [n, g] ∨ ¬x [n, b]).
4. Every edge (n,
o) has different colored ends: (¬x [n, r] ¬ ¬x [o, r]) ∧ (¬x [n, g] ∨ ¬x [o, g]) ∧ (¬x [n, b] ∨ ¬x [o, b]).

We consider that the graph nodes are represented by natural numbers larger or equal to 1, and the three Boolean variables related to one node are represented by the numbers 10N+1, 10N+2, 10N+3. For example, node 1 corresponds to variables 11, 12, 13, node 2, variables 21, 22, 23 etc.
Variable 11 corresponds to the sentence "Node 1 is red", variable 22, the sentence "Node 2 is green", variable 33, the sentence "Node 3 is blue", etc.

For example, for the unoriented graph with the nodes {1, 2} and the edge (1, 2), the formula is obtained with the following variables and clauses (the numbers of the items mirror the list above):

- Variables 11, 12, 13, 21, 22, 23.
- Type clauses at least one color: {11, 12, 13}, {21, 22, 23}.
- Type clauses at most one color: {-11, -12}, {-11, -13}, {-12, -13}, {-21, -22}, {-21, -23}, {-22, -23}.
- Different colored heads: {-11, -21}, {-12, -22}, {-13, -23}.

For the entire formula described above, the satisfaction algorithm could produce the interpretation {-23, -22, -13, -11, 12, 21}, which corresponds to coloring {(1, green), (2, red)}.

### Specifications

The third stage of the theme addresses the resolution of the clauses, the learning of the clauses, the satisfaction algorithm and the application to the 3-color problem.

The constructions and language mechanisms that you will exploit in the resolution, besides those in stages 1 and 2, are:

- **ad-hoc polymorphism**
- **classes.**

The module of interest in the skeleton is Solver, which contains the operations you need to implement. Find the details about functionality and implementation constraints, as well as examples, directly in the file. You have to complete the definitions that start with *** Todo ***.

For the testing of tests,
Upload the testsolver module into the interpreter and evaluate Main. The last test (stress) uses higher graphs and execution can take a few seconds.

It is sufficient for the Vmchecker Archive to contain the Solver, ExtendFormula modules in Stage 2 and the Formula in Stage 1.

Deposits follow the same principles as in the first two stages.