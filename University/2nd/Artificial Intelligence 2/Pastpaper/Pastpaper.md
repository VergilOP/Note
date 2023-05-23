# Pastpaper

## Main Summer Examinations 2021

1. As a data scientist in a telecommunication company, your task is to analyse a customer dataset to predict whether a customer will terminate his/her contract. The dataset consists of around 8000 customer records, each consisting of one binary dependent variable Y , indicating whether the customer terminates the contract (Y = 1) or not (Y = 0), and 19 independent variables, which include the customer's information, e.g., age, subscription plan, extra data plan, etc., and the consumer behaviour such as average numbers of calls and hours per week. Since your boss needs some actionable insights to retain customers, you decided to use interpretable machine learning methods. Design your interpretable machine learning method by answering the following questions:
    1. You have implemented a feature selection algorithm based on mutual information to select the most informative features from the 19 independent variables. To validate the implementation of your mutual information calculation function, you use a small subset of the data to calculate mutual information manually. You select one independent variable `subscription plan`, denoted as S, which takes two values, $S \in \{1,2\}$. Please use the following Probability Mass Function table
        | P(S,Y) | S=1 | S=2 |
        |--------|-----|-----|
        | Y=0    | $\frac{2}{12}$    |  $\frac{5}{12}$   |
        | Y=1    | $\frac{2}{12}$    |  $\frac{3}{12}$   |
        to calculate
        - Entropies H(S) and H(Y)
        - Conditional entropies H(S|Y) and H(Y|S)
        - Joint entropy H(S,Y)
        - Mutual information I(S;Y)
        Show all your working. Discuss what mutual information means and whether this feature will be selected or not.

        > $H(S) = -(\frac{4}{12}\times\log\frac{4}{12} + \frac{8}{12}\times\log\frac{8}{12})= 0.918 bits\text{(bits是以2为底数)}$  
        > $H(Y) = -(\frac{7}{12}\times\log\frac{7}{12} + \frac{5}{12}\times\log\frac{5}{12})= 0.980 bits$  
        > $H(S,Y) = -(\frac{2}{12}\times\log\frac{2}{12} + \frac{5}{12}\times\log\frac{5}{12}+\frac{2}{12}\times\log\frac{2}{12} + \frac{3}{12}\times\log\frac{3}{12}) = 1.888 bits$  
        > $H(S|Y) = H(S,Y) - H(Y) = 0.908 bits$  
        > $H(Y|S) = H(S,Y) - H(S) = 0.970 bits$  
        > $I(S;Y) = H(S) - H(S|Y) = 0.01 bits$  
        > Mutual information measures the information that S and Y share. This feature will not be selected as the mutual information is too small which means the feature is not useful at all.

    2. After applying your algorithm you selected two variables:  
        1) extra data plan E, which is a binary random variable that indicates whether the customer subscribes to the extra data plan (E = 1) or not (E = 0); and 
        2) averaged hours used per week H, which is a continuous random variable. 
        You then built a logistic regression model to classify customers into `low risk` or `high risk` of terminating the contract. The fitted model is
        $$
            \log(\frac{p}{1-p}) = -0.77 + 0.23H - 1.18E
        $$
        - Given a customer who has the extra data plan (E = 1) and spent on average 0.5 hours per week, calculate the odds and the probability the customer will terminate the contract (Y = 1). `(4 marks)`
            > $odds = \frac{p}{1-p} = e^{-0.77 + 0.23H - 1.18E} = e^{-0.77 + 0.23\times 0.5 - 1.18\times 1} = 0.1596$  
            > $sigmoid(Y = 0) = \frac{1}{1+e^{-0.77 + 0.23H - 1.18E}} = \frac{1}{1+e^{-0.77 + 0.23\times 0.5 - 1.18\times 1}} = 0.862$
            > $sigmoid(Y = 1) = 1- sigmoid(Y = 1) = 0.138 OR_E = \frac{\text{odds when E = 1}}{\text{odds when E = 0}} = \frac{e^{-0.77 + 0.23H - 1.18}}{e^{-0.77 + 0.23H}}$
        - Using this fitted model, explain to your boss what actions should be taken to retain customers. `(10 marks)`  
            > $OR_E = \frac{\text{odds when E = 1}}{\text{odds when E = 0}} = \frac{e^{-0.77 + 0.23H - 1.18}}{e^{-0.77 + 0.23H}}=e^{-1.18} = 0.31$  
            > $OR_H = \frac{\text{odds when} H = h+\Delta}{\text{odds when} H = h} = \frac{e^{-0.77 + 0.23(H+\Delta) - 1.18E}}{e^{-0.77 + 0.23H - 1.18E}} = {e^{(0.23)}}^{\Delta} = 1.25^{\Delta}$  
            > - If a customer add the extra data plan, the odds of terminating the contract will increase is 0.3, which means that odds the customer terminating the contract will decrease by a factor of 3  
            > - If a customer increase the average time by one hour, the odds of terminating the contract increase by a factor of 1.25  
            >
            > From the analysis,we can suggest to the boss that, the more hours the customers spent, the more likely the customers will terminate, which means the company should improve its telecomunication service/price. However, by simply persuade them to subscribe to the extra data plan, they are more likely to stay


## Main Summer Examinations 2022

1. As a machine learning expert for an AI cyber security company, your task is to design an automated network intrusion detection system. You have collected a large number of records of network activities. Each record includes the log information about network activity, such as protocol types, duration, number of failed logins, which are random variables, denoted as $X = [X_1, X_2, ..., X_n]^T$. Each record also includes a binary random variable Y called label that was labelled by cyber security experts as intrusions (Y=1) or normal connections(Y=0)
    1. Consider feature selection based on mutual information to reduce the number of independent variables.
       1. Explain to your colleague, who knows nothing about information theory, the concept of mutual information. 
            > Basic motivation behind mutual information is to measure the information that two random variables X and Y share. In other words, it measures how much knowing one of these variables reduces uncertainty about the other.
       2. Explain the loop, i.e., lines 4-7 of the pseudocode in Table 1. Note $I(Y;X_i)$ is the mutual information between $Y$ and $X_i$
            $$
            \begin{align*}
              &\text{Initialisation: Set } F \leftarrow X \text{ and } S \leftarrow ∅ \\
              &\quad f_{\max} = \argmax_{X_i \in X} I(Y; X_i) \\
              &\quad \text{Set } F \leftarrow F / \{f_{\max}\} \text{ and } S \leftarrow f_{\max} \\
              &\quad \text{Repeat until } |S| = K: \\
              &\quad \quad f_{\max} = \argmax_{X_i \in F}I(Y; X_i) - \beta\sum_{X_s\in S} I(X_s; X_i) \\
              &\quad \quad \text{set } F \leftarrow F \ \{f_{\max}\} \text{ and } S \leftarrow f_{\max}\\
              &\quad \text{End}\\
              &\text{End}
            \end{align*}
            $$  
            Table 1: Pseudocode of Mutual Information based Feature Selection Algorithm.  
            > The two lines in the loop are used to select K features. In this loop, we also nd the feature $f_{\max}$ which achieves the maximum mutual information I among all the remaining independent variables in set F . However, because some features highly correlated with each other, selecting them will increase the number of features but does not improve the prediction. Therefore, we need to make sure there must be minimal redundancy between the candidate feature $X_i$ and the set of selected features S. That's exactly the second term of the equation ($f_{\max}$) You then add this feature into S and then subtract it from set F and repeat until we got K features
    2. After applying your feature selection algorithm, assume you selected four random variables as features, denoted as $F_1, F_2, F_3, F_4$. Based on these features, you now work with a cyber security expert to construct a Bayesian network to harness the domain knowledge of cyber security. The expert first divides intrusions into three cyber attacks, $A_1, A_2, A_3$, which are marginally independent from each other. The expert suggests the presence of the four features are used to find the most probable type of cyber attacks. The four features are conditionally dependent on the three types cyber attacks as follows: $F_1$ depends only on $A_1, F_2$ depends on $A_1$ and $A_2$. $F_3$ depends on $A_1$ and $A_3$, whereas $F_4$ depends only on $A_3$. We assume all these random variables are binary, i.e., they are either 1 (true) or 0 (false).
       1. Draw the Bayesian network according to the expert?s description. [2 marks]  
            > ![Pastpaer_1](./images/PastPaper_1.png)
       2. Write down the joint probability distribution represented by this Bayesian network. [3 mark]  
            > $P(A_1, A_2, A_3, F_1, F_2, F_3, F_4)$  
            > $=P(A_1)P(A_2)P(A_3)P(F_1|A_1)P(F_2|A_1,A_2)P(F_3|A_1,A_3)P(F_4|A_3)$
       3. How many parameters are required to describe this joint probability distribution? Show your working. [5 marks]
            > | Conditional Probability | Number of parameters |
            > |-------------------------|----------------------|
            > | $P(A_1)$                | 1                    |
            > | $P(A_2)$                | 1                    |
            > | $P(A_3)$                | 1                    |
            > | $P(F_1\|A_1)$           | 2                    |
            > | $P(F_2\|A_1,A_2)$       | 4                    |
            > | $P(F_3\|A_1,A_3)$       | 4                    |
            > | $P(F_4\|A_3)$           | 2                    |
       4. Suppose in a record we observe $F_2$ is true, what does observing $F_4$ is true tell us? If we observe $F_3$ is true instead of $F_2$, what does observing $F_4$ is true tell us? [5 marks]
            > With $F_2 = 1$, obeserving $F_4 = 1$ still gives us information only about $A_3$  
            > If we observe $F_3 = 1$ instead of $F_2$, then oberving $F_4 = 1$ will give us information about $A_1$ and $A_3$ due to competing causes

2.  1. Consider the following minimax game tree. There are two players Max and Min; the player Max wants to maximise the utility and the player Min wants to minimise the utility. The tree has five layers and we can use Lm-n to denote the nth node from left to right in the layer m; for example, the root node can be denoted by L1-1, the first node at the bottom layer (with value 8) can be denoted by L5-1, and the fourth node at the fourth layer (with value 11) can be denoted by L4-4. Give the value of the root node (L1-1) and the values of the two nodes at the second layer (L2-1 and L2-2).[2 marks]
        ![Pastpaper_2](./images/PastPaper_2.png)
        > L1-1: 12  
        > L2-1: 12  
        > L2-2: 9
    2. We use the alpha-beta pruning algorithm to prune the tree. List all the pruned nodes. Assume that child nodes are visited from left to right. [10 marks]
        > L5-2, L4-4, L5-6, L3-5, L4-7, L4-8, L5-7, L5-8

## Information Theory Exercise Problems 1-4

### Exercise 1

1. Prove that the information measure is additive: that the informatin gained from observing the combination of N independent events, whose probabilities are $p_i$ for $i = 1...N$, is the $sum$ of the information gained from observing each one of these events separately and in any order.

    > The information measure assigns $\log _2(p)$ bits to the observation of an event whose probability is p. The joint probability of a combination of N independent events whose probabilities are $p_1...p_N$ is $\prod\limits_{i=1}^Np_i$. Thus the information content of such a combination is:
    > $$
    >     \log _2(\prod\limits_{i=1}^Np_i) = \log _2(p_1) + \log _2(p_2) + ... + \log _2(p_N)
    > $$
    > which is the sum of the information content of all of the separate events.

2. Calculate the entropy in bits for each of the following random variables:
   1. Pixel values in an image whose possible grey values are all the integers from 0 to 255 with uniform probability.
        > In this case each $p_i = \frac{1}{256}$ and the ensemble entropy summation extends over 256 such equiprobable grey values, so $H = -(256)\times \frac{1}{256} \times \log_2(\frac{1}{256}) = 8 bits$
   2. Humans classified according to whether they are, or are not, mammals.
        > Since all humans are in this category (humans ⊂ mammals), there is no uncertainty about this classification and hence the entropy is 0 bits.
   3. Gender in a tri-sexed insect population whose three genders occur with probabilities $\frac{1}{4}$, $\frac{1}{4}$, and $\frac{1}{2}$
        > The entropy of this distribution is $-\frac{1}{4}\times \log_2\frac{1}{4} -\frac{1}{4}\times \log_2\frac{1}{4} - \frac{1}{2}\times \log_2\frac{1}{2} = 1.5 bits$
   4. A population of persons classified by whether they are older, or not older, than the population’s median age
        > By the definition of median, both classes have probability 0.5, so the entropy is 1 bit.$- 2\times \frac{1}{2}\times \log_2\frac{1}{2} = 1 bits$

3. Consider two independent integer-valued random variables, X and Y . Variable X takes on only the values of the eight integers {1, 2, ..., 8} and does so with uniform probability. Variable Y may take the value of any positive integer k, with probabilities $P{Y = k} = 2^ {−k} , k = 1, 2, 3, ...$
    1. Which random variable has greater uncertainty? Calculate both entropies $H(X)$ and $H(Y)$.
          > $H(X) = - 8\times \frac{1}{8}\times \log_2\frac{1}{8} = 3 bits$  
          > $H(Y) = \sum\limits_{k=1}^k[k \times 2^{-k}] = 2 bits$  
    2. What is the joint entropy $H(X, Y)$ of these random variables, and what is their mutual information $I(X; Y )$?
          > $H(X, Y) = H(X) + H(Y) = 3 + 2 = 5 (bit)$  
          > $I(X; Y ) = 0bits$

4. What is the maximum possible entropy H of an alphabet consisting of N different letters? In such a maximum entropy alphabet, what is the probability of its most likely letter? What is the probability of its least likely letter? Why are fixed length codes inefficient for alphabets whose letters are not equiprobable? Discuss this in relation to Morse Cod
     > $H_{max} = - \sum \frac{1}{N} * \log _2\frac{1}{N} = \log _2(N)$  
     > This is only achieved if the probability of every letter is 1/N. Thus 1/N is the probability of both the “most likely” and the “least likely” letter.  
     > Fixed length codes are inefficient for alphabets whose letters are not equiprobable because the cost of coding improbable letters is the same as that of coding more probable ones. It is more efficient to allocate fewer bits to coding the more probable letters, and to make up for the reduced address space of such short strings of bits by making longer codes for the less probable letters. In other words, a variable-length code. An example is Morse Code, in which the most probable English letter, e, is coded by a single dot.

### Exercise 2

1. Suppose that women who live beyond the age of 80 outnumber men in the same age group by three to one. How much information, in bits, is gained by learning that a person who lives beyond 80 is male?
     > $p(female|old) = 3p(male|old)$  
     > $p(female|old)+p(male|old)= 1$  
     > $p(male|old)= 1/4$  
     > Information gained $-\log _2 \frac{1}{4}=2 bits$

2. Consider n discrete random variables, named $X_1, X_2, ..., X_n$, of which $X_i$ has entropy $H(X_i)$, the largest being $H(X_L)$. What is the upper bound on the joint entropy $H(X_1, X_2, ..., X_n)$ of all these random variables, and under what condition will this upper bound be reached? What is the lower bound on the joint entropy $H(X_1, X_2, ..., X_n)$?
     > $H(X_L) \leq H(X_1, X_2, ..., X_n) \leq \sum\limits_{i=1}^nH(X_i)$  

3. Suppose that X is a random variable whose entropy $H(X)$ is 8 bits. Suppose that $Y(X)$ is a deterministic function that takes on a different value for each value of X.
     1. What then is H(Y), the entropy of Y ?
          > The entropy of Y : H(Y ) = 8 bits also.
     2. What is H(Y|X), the conditional entropy of Y given X?
          > The conditional entropy of Y given X: H(Y |X) = 0 because of determinism.
     3. What is H(X|Y), the conditional entropy of X given Y ?
          > The conditional entropy of X given Y : H(X|Y ) = 0 also.
     4. What is H(X,Y), the joint entropy of X and Y ?
          > The joint entropy H(X, Y) = H(X) + H(Y|X) = 8 bits.
     5. Suppose now that the deterministic function Y (X) is not invertible; in other words, different values of X may correspond to the same value of Y (X). In that case, what could you say about H(Y)?
          > Since now different values of X may correspond to the same value of Y (X), the new distribution of Y has lost entropy and so H(Y ) < 8 bits.
     6. In that case, what could you say about H(X|Y) ?
          > Now knowledge of Y no longer determines X, and so the conditional entropy H(X|Y) is no longer zero: H(X|Y ) > 0

4. Let the random variable X be five possible symbols {α, β, γ, δ, ε}. Consider two probability distributions p(x) and q(x) over these symbols, and two possible coding schemes C1(x) and C2(x) for this random variable:  
     | Symbol | p(x) | q(x) | C1(x) | C2(x) |
     |--------|------|------|-------|-------|
     | α      | 1/2  | 1/2  | 0     | 0     |
     | β      | 1/4  | 1/8  | 10    | 100   |
     | γ      | 1/8  | 1/8  | 110   | 101   |
     | δ      | 1/16 | 1/8  | 1110  | 110   |
     | ε      | 1/16 | 1/8  | 1111  | 111   |
     1. Calculate H(p), H(q), and relative entropies (Kullback-Leibler distances) D(p||q) and D(q||p).
          > $H(p) = -\sum\limits_ip_i\log _2 p_i=1\frac{7}{8}bits$  
          > $H(q) = -\sum\limits_iq_i\log _2 q_i=2bits$


