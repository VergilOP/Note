# Logic week 2

## Lecture 3

### Propositions 命题

**Propositional logic** is a **symbolic logic** to reason about logical statement called **propositions** that can (in principle) be true or  
命题逻辑是一种符号学逻辑，用来对一个逻辑命题进行推理，这个逻辑命题(原则上)可以是真或假

- Example
  - 8 x 7 = 42 **Yes**

### Arguments 论据

An **argument** is valid if and only if (iff) whenever the premises are true, then so is the conclusion  
当且仅当(iff)前提为真时，一个**论证**有效，那么结论也是真

- Example
  - 1. If John is at home, then his television is on.
    2. His television is not on.
    3. Therefore, John is not at home.  
    Valid? **Yes**

### Propositional logic  命题逻辑

- Symbols:  
  符号：
  - atomic propositions (true/false atomic statements)  
    原子命题（真假原子陈述）
  - combined using logical connectives
    使用逻辑连接语进行组合

- Atomic propositions (atoms)  
  原子命题
  - propositions that cannot be broken into smaller parts  
    不能被分解成更小的部分的命题
  - Let p, q, r, . . . be atomic propositions  
    让pqr…都是原子命题
  - two special atoms: T stands for True, ⊥ stands for False  
    两个特殊的原子：T代表True，⊥代表False

- Logical Connectives  
  逻辑连接
  - conjunction: ∧ (and)  
    连词
  - disjunction: ∨ (or)  
    或词
  - implication: → (if .... then / implies)  
    蕴含
  - negation: ¬ (not) — can be defined using → and ⊥  
    否认

- If P and Q are formulas, then
  - P ^ Q is a formula
  - P ∨ Q is a formula
  - P → Q is a formula
  - ¬P is a formula

Those are called **compound formulas**  
这些都被称为复合公式

### Connectives - informal semantics 连接-非正式语义

- Conjunction: P ^ Q, i.e., P and Q  
  连词
  - true if both individual propositions P and Q are true  
    如果个别命题P和Q都为真，则为真

- Disjunction: P v Q, i.e., P or Q  
  或词
  - true if one or both individual propositions P and Q are true  
    如果一个或两个单独的命题P和Q都为真，则为真
  - also sometimes called “inclusive or”  
    有时也被称为“包容性或”
  - Note: Or in English is often an “exclusive or” (i.e. where one or the other is true, but not both)  
    注：或者在英语中通常是“排他性的或”（即其中一个或另一个是真的，但不是两者都是）
  - e.g., “Your mark will be pass or fail
  - but logical disjunction is always defined as above

- Implication: P → Q, i.e., P implies Q  
  蕴含
  - means: if P is true then Q must be true too  
    如果P是真的，那么Q也必须是真的
  - if P is false, we can conclude nothing about Q  
    如果P是假的，我们就不能得出任何关于Q的结论
  - P is the antecedent, Q is the consequent  
    P是前因，Q是结果

- Negation: ¬P, i.e., not P  
  否认
  - it can be defined as P → ⊥
  - if P is true, then ⊥ (False)
  - true iff P is false

### Avoiding ambiguities 避免歧义

Precedence: in decreasing order of precedence ¬, ∧, ∨, →  
优先级：按优先级递减的¬、∧、∨、→

Associativity: all operators are right associative  
关联性：所有操作符都是从右往左的关联性

### Parse Trees 解析树

- Scope of a connective  
  连接器的范围
  - The connective itself, plus what it connects  
    连接器本身，再加上它所连接的东西
  - That is, the sub-tree of the parse tree rooted at the connective  
    连接器本身，再加上它所连接的东西
  - The scope of ^ in (P ^ Q) ∨ R is P ^ Q
    (P^Q)∨R中的^范围为P^Q

- Main connective of a formula  
  一个公式的主要连接器
  - The connective whose scope is the whole formula  
    其范围是整个公式的连接器
  - That is, the root node of the parse tree  
    即，解析树的根节点
  - The main connective of (P ^ Q) ∨ R is ∨  
    (P^Q)∨R的主要连接器是∨

## Lecture 4

### Natural Deduction

- Framework  
  框架
  - “natural” style of constructing a proof  
    构建证明的“自然”风格
  - start with the given premises  
    从给定的前提开始
  - repeatedly apply the given inference rules  
    反复应用给定的推理规则
  - until you obtain the conclusion  
    直到你得出结论

- Two key points:  
  两个关键点
  - Can work both forwards and backwards  
    可以向前和反向工作
  - Natural doesn’t mean there is unique proof  
    自然并不意味着有独特的证据

### Comprehensive set of inference rules 综合的推理规则集

- Rules for → (implication)  
![Rules for implication](../../Week%201/Logic/image/Rules%20for%20implication.png)

- Rules for ¬ (not)  
![Rules for not](../../Week%201/Logic/image/Rules%20for%20not.png)

- Rules for ∨ (or)  
![Rules for or](../../Week%201/Logic/image/Rules%20for%20or.png)

- Rules for ∧ (and)  
![Rules for and](../../Week%201/Logic/image/Rules%20for%20and.png)



