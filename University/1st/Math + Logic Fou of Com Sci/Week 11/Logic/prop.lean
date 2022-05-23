-- Lean includes propositional logic
-- conjunction
#check and
#print and.intro
#print and.elim
#print and.left
#print and.right
-- disjunction, implication, negation
#check or
#check implies
#check not
#print not -- not(A) is defined as A->False

-- we declare some propositions
-- WARNING: introducing constants is not always safe (it introduces new axioms that might be invalid),
--   we only do it here for simplicity.
--   We could have used 'variables' instead, but we would have had to deal with P, Q parameters everywhere below
constants P Q R S T : Prop

-- let's prove that P → Q → P
lemma l1 : P → Q → P :=
begin -- this begins the proof
  intros hp hq, -- this moves P and Q to the hypotheses, i.e., [→R]
  assumption -- this is [Id]
end -- this ends the proof

-- let's prove the same proposition again using the 'assume' construct, again [→R]
lemma l2 : P → Q → P :=
  assume hp : P, -- we assume that we have a proof hp of P
  assume hq : Q, -- we assume that we have a proof hq of Q
  hp -- we prove P thanks to our proof hp of P

-- Let's write a shorter proof:
lemma l3 : P → Q → P :=
  assume (hp : P) (hq : Q), -- same as above on one line
  hp

-- Let's write an even shorter proof:
lemma l4 : P → Q → P :=
  assume hp hq, hp

-- What is the proof of l4?
#print l4 -- it's a function that takes a proof of P, and a proof of Q, and returns the proof of P
#print l3 -- all the above proofs are the same
#print l2
#print l1

-- we can write that directly:
lemma l5 : P → Q → P := λ hp hq, hp

-- we can even compute with it
-- assume p is a proof of P and q a proof of Q
constants (p : P) (q : Q)

#reduce l5 p q -- it returns p as expected

-- We can use any of the above techniques to prove lemmas


------------------------
-- QUESTION - try to prove
--
lemma q1 : P → Q → Q := _



-- Let's prove a slightly more complex proposition:
lemma l6 : P ∧ Q → Q ∧ P :=
begin
  intros pq,
  destruct pq, -- applies [∧L] to pq
  intros hp hq,
  constructor, -- applies [∧R]
  assumption, assumption -- those are [Id]
end

-- let's look at another way to prove this
lemma l7 : P ∧ Q → Q ∧ P :=
begin
  intros pq,
  apply and.elim pq, -- has the same effect than 'destruct pq'
  intros hp hq,
  apply and.intro, -- has the same effect than 'constructor'
  assumption, assumption
end

-- What is the proof of l6/l7?
#print l6 -- it takes a pair pq, destructs the pair into its two components hp and hq, and creates the swapped pair ⟨hq,hp⟩

-- we can achieve the same thing directly, in a more readable way:
lemma l8 : P ∧ Q → Q ∧ P := λ hp, let ⟨hp,hq⟩ := hp in ⟨hq,hp⟩
-- Again, this is just the swapping function


------------------------
-- QUESTION - try to prove:
-- (use 'left' to prove the left disjunct, and 'right' to prove the right disjunct)
--
lemma q2 : P ∨ Q → Q ∨ P := _


------------------------
-- QUESTION - let's prove a final example:
--
lemma l9 : (P → ¬Q → ¬R) → (P ∧ R) → ¬¬Q :=
begin
  intros i pr nq,
  destruct pr, intros hp hr,
  clear pr, -- Lean keeps the proof of P ∧ R but we can remove it if we want
  apply i,
  assumption, assumption, assumption,
end
