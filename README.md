# SAT-Solving

## Stage 1: Formula Representation and Satisfiability Checking

### Fundamental Concepts in SAT

In SAT, we work with boolean formulas in conjunctive normal form (CNF), which denote conjunctions of disjunctions of boolean literals. Specifically, we use the following terms:

- **Boolean variable**: x₁, x₂, etc.
- **Boolean literal**: a variable or its negation: x₁, ¬x₂, etc.
- **Clause**: disjunction of literals: (x₁ ∨ ¬x₂ ∨ x₃), etc.
- **CNF formula**: conjunction of clauses: (x₁ ∨ ¬x₂ ∨ x₃) ∧ (¬x₄ ∨ x₅), etc.
- **Interpretation**: set of literals assumed to be true: {x₁, ¬x₂}, meaning variable x₁ is assumed true, and x₂ is false.

Additionally, we use the following terms:
- **Unit clause**: a clause with a single literal: (x₁), etc.
- **Pure literal** with respect to a formula: a literal whose complement does not appear in that formula: ¬x₁ is pure with respect to the formula (¬x₁ ∨ ¬x₂) ∧ (¬x₁ ∨ x₂), as x₁ does not appear, but x₂ and ¬x₂ are not pure.

### Formula Satisfiability

A SAT instance, represented by a CNF formula, aims to determine the satisfiability of the formula, meaning the existence of an interpretation with respect to which the entire formula is true. If such an interpretation exists, we also want to identify it.

Essentially, a CNF formula is satisfiable if we can identify at least one true literal in each clause (property P).

#### Examples:

- The formula (x₁ ∨ ¬x₂ ∨ x₃) ∧ (¬x₄ ∨ x₅) is **satisfiable**, with multiple interpretations fulfilling property (P): {x₁, ¬x₄}, {x₁, ¬x₂, x₅}, etc.
- The formula (x₁ ∨ ¬x₁ ∨ x₂) ∧ (¬x₃ ∨ x₃) is **valid/tautological**, meaning it's true in all interpretations, therefore satisfiable.
- The formula (x₁) ∧ (¬x₁) is **contradictory**, meaning it's false in all interpretations, therefore unsatisfiable.

#### Special Cases:

- The empty formula is **satisfiable**, as property (P) is trivially fulfilled by the absence of clauses.
- A formula containing the empty clause is **unsatisfiable**, as the empty clause cannot contain a literal assumed to be true in an interpretation.

### Haskell Representation

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

### Language Constructs and Mechanisms

The language constructs and mechanisms you will leverage in the solution are:

- Sets and lists
- Functionals on sets and lists, to observe the universality of these constructions
- Sections, meaning partially applied infix applications, such as `(x `f`)` or ``(`f` y)``
- List comprehensions, for concise descriptions of operations
- Lazy evaluation, implicit in Haskell, for conceptual decoupling of transformations within a sequence
- Local variable binding constructs (`let` or `where`)

### Implementation Requirements

The module of interest from the skeleton is `Formula`, which contains the representation of the above concepts, as well as the operations you need to implement. You can find details about functionality and implementation constraints, as well as examples, directly in the file. You need to complete the definitions that start with `*** TODO ***`.

To run the tests, load the `TestFormula` module into the interpreter and evaluate `main`.

It is sufficient for the archive for vmchecker to contain only the `Formula` module.

### Penalties

The comments of most functions in the skeleton contain CONSTRAINTS, the non-compliance with which attracts partial deductions.

In addition, direct operation on the set representation of clauses, formulas, and interpretations is required. **Intermediate conversions to lists and back to sets will result in the total deduction of points for the functions implemented in this way!** The predefined function `toLiteralLists` in the skeleton is provided only to more easily visualize formulas; it is not intended to be used in implementations.
