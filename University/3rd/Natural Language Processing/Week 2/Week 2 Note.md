# Week 2 Note

## Introduction to N-grams

### Probabilistic Language Modeling

- Goal: compute the probability of a sentence or sequence of words:  
  $$
    P(W) = P(w_1, w_2, w_3, w_4, w_5,... w_n)
  $$

- Related task: probability of an upcoming word:
  $$
    P(w_5|w_1, w_2, w_3, w_4)
  $$

- A model that computes either of these:
  $$
    P(W) or P(w_n|w_1, w_2,... w_{n-1}) \text{ \ is called a language model}
  $$
  Better: `the grammar`. But `language model` or `LM` is standard

### Reminder: The Chain Rule

- Recall the definition of conditional probabilities
  $$
    p(B|A) = P(A, B)/P(A) \text{ Rewriting: } P(A,B) = P(A)P(B|A)
  $$

- More variables:
  $$
    P(A,B,C,D) = P(A)P(B|A)P(C|A,B)P(D|A,B,C)
  $$

- The Chain Rule in General
  $$
    P(x_1, x_2, x_3,...,x_n) = P(x_1)P(x_2|x_1)P(x_3|x_1,x_2)...P(x_n|x_1,...,x_{n-1})
  $$

- The Chain Rule applied to compute joint probability of words in sentence

  $$
      P(w_1w_2..w_n) = \prod_iP(w_i|w_1w_2...w_{i-1})
  $$

  P(“its water is so transparent”) = P(its) × P(water|its) × P(is|its water) × P(so|its water is) × P(transparent|its water is so)

- Markove Assumption

- Simplifying assumption:   
  In other words, we approximate each component in the product
  $$
    P(w_i|w_1w_2...w_{i-1}) \approx P(w_i|w_{i-k}\dots w_{i-1})
  $$

- Bigram model  
  Condition on the previous word
  $$
    P(w_i|w_1w_2\dots w_{i-1}) \approx P(w_i|w_{i-1})
  $$

### N-gram models

- We can extend to trigrams, 4-grams, 5-grams
- In general this is an insufficient model of language
  - because language has long-distance dependencies:  
  ```“The computer which I had just put into the machine room on the fifth floor crashed.```
- But we can often get away with N-gram models

## Estimating N-gram Probabilities

### Estimating bigram probabilities

- The Maximum Likelihood Estimate
  $$
    P(w_i|w_{i-1}) = \frac{count(w_{i-1}, w_i)}{count(w_{i-1})}
  $$

- Practical Issues
  - We do everything in log space
    - Avoid underflow
    - (also adding is faster than multiplying)
  $\log(p_1 × p_2 × p_3 × p_4 ) = \log p_1 + \log p_2 + \log p_3 + \log p_4$

## Evaluation and Perplexity

### Extrinsic evaluation of N-gram models

- Best evaluation for comparing models A and B
  - Put each model in a task
    - spelling corrector, speech recognizer, MT system
  - Run the task, get an accuracy for A and for B
    - How many misspelled words corrected properly
    - How many words translated correctly
  - Compare accuracy for A and B

#### Difficulty of extrinsic (in-vivo) evaluation of N-gram models

- Extrinsic evaluation
  - Time-consuming; can take days or weeks
- So
  - Sometimes use intrinsic evaluation: perplexity
  - Bad approximation
    - unless the test data looks just like the training data
    -  So generally only useful in pilot experiments
  - But is helpful to think about.

#### Intuition of Perplexity

- A better model of a text: 
  - is one which assigns a higher probability to the word that actually occurs

- Perplexity
  - The best language model is one that best predicts an unseen test set
    - Gives the highest P(sentence)
  - Perplexity is the inverse probability of the test set, normalized by the number of words

$$
\text{PP}(W) = \sqrt[N]{\frac{1}{P(w_1w_2...w_N)}}
$$

$$
\text{PP}(W) = \sqrt[N]{\prod_{i=1}^{N} \frac{1}{P(w_i|w_1...w_{i-1})}}
$$

$$
\text{PP}(W) = \sqrt[N]{\prod_{i=1}^{N} \frac{1}{P(w_i|w_{i-1})}}
$$

- Minimizing perplexity is the same as maximizing probability

- The Shannon Game intuition for perplexity
- From Josh Goodman
- Perplexity is weighted equivalent branching factor
- How hard is the task of recognizing digits ‘0,1,2,3,4,5,6,7,8,9’
  - Perplexity 10
- How hard is recognizing (30,000) names at Microsoft.
  - Perplexity = 30,000
- Let's imagine a call-routing phone system gets 120K calls and has to recognize
  - "Operator" (let's say this occurs 1 in 4 calls)
  - "Sales" (1in 4)
  - "Technical Support" (1 in 4)
  - 30,000 different names (each name occurring 1 time in the 120K calls)

- Lower perplexity = better model

## Generalization and zeros

### The Shannon Visualization Method

- Choose a random bigram 
- (\<s>, w) according to its probability
- Now choose a random bigram 
- (w, x) according to its probability
- And so on until we choose \</s>
- Then string the words together

#### The perils of overfitting

- N-grams only work well for word prediction if the test corpus looks like the training corpus
  - In real life, it often doesn’t
  - We need to train robust models that generalize!
  - One kind of generalization: Zeros!
    - Things that don’t ever occur in the training set
      - But occur in the test set

- Bigrams with zero probability
  - mean that we will assign 0 probability to the test set!
- And hence we cannot compute perplexity (can’t divide by 0)

## Smoothing: Add-one (Laplace) smoothing

- Add-one estimation
  - Also called Laplace smoothing 
  - Pretend we saw each word one more time than we did
  - Just add one to all the counts!

MLE估计：
$$
P_{MLE}(w_i | w_{i-1}) = \frac{c(w_{i-1}, w_i)}{c(w_{i-1})}
$$

加一平滑估计（Add-1 estimate）：
$$
P_{\text{Add-1}}(w_i | w_{i-1}) = \frac{c(w_{i-1}, w_i) + 1}{c(w_{i-1}) + V}
$$

### Maximum Likelihood Estimates

- The maximum likelihood estimate
  - of some parameter of a model M from a training set T
  - maximizes the likelihood of the training set T given the model M

### Add-1 estimation is a blunt instrument

- So add-1 isn’t used for N-grams: 
  - We’ll see better methods
- But add-1 is used to smooth other NLP models
  - For text classification 
  - In domains where the number of zeros isn’t so huge

## Interpolation, Backoff, and Web-Scale LMs

### Backoff and Interpolation
- Sometimes it helps to use less context
  - Condition on less context for contexts you haven’t learned much about 
- Backoff: 
  - use trigram if you have good evidence,
  - otherwise bigram, otherwise unigram
  - 退避是一种当更长的上下文（如trigram）没有足够数据支持时，选择使用较短上下文（如bigram或unigram）的技术
- Interpolation: 
  - mix unigram, bigram, trigram
  - 插值则是将unigram、bigram和trigram等多种模型的概率估计混合起来，即使在数据稀疏的情况下也可以使用所有可用的上下文信息

Interpolation works better

### Linear Interpolation

- Simple interpolation
  $$
    \hat{P}(w_n|w_{n-2}w_{n-1}) = \lambda_1 P(w_n|w_{n-2}w_{n-1}) + \lambda_2 P(w_n|w_{n-1}) + \lambda_3 P(w_n)
  $$
  其中，满足条件：
  $$
  \sum_{i} \lambda_i = 1
  $$

- Lambdas conditional on context:
  $$
    \hat{P}(w_n|w_{n-2}w_{n-1}) = \lambda_1(w_{n-2}^{n-1})P(w_n|w_{n-2}w_{n-1}) + \lambda_2(w_{n-2}^{n-1})P(w_n|w_{n-1}) + \lambda_3(w_{n-2}^{n-1})P(w_n)
  $$

### How to set the lambdas?

- Use a held-out corpus
  - Training Data
  - Held-out Data
  - Test Data

- Choose λs to maximize the probability of held-out data:
  - Fix the N-gram probabilities (on the training data)
  - Then search for λs that give largest probability to held-out set:
    $$
      \log P(w_1,...,w_n | M(\lambda_1,...,\lambda_k)) = \sum_{i} \log P_{M(\lambda_1,...,\lambda_k)}(w_i | w_{i-1})
    $$

### Unknown words: Open versus closed vocabulary tasks

> - If we know all the words in advanced
>   - Vocabulary V is fixed
>   - Closed vocabulary task
> - Often we don’t know this
>   - `Out Of Vocabulary` = OOV words
>   - Open vocabulary task
> - Instead: create an unknown word token \<UNK>
>   - Training of \<UNK> probabilities
>     - Create a fixed lexicon L of size V
>     - At text normalization phase, any training word not in L changed to \<UNK>
>     - Now we train its probabilities like a normal word
>   - At decoding time
>     - If text input: Use UNK probabilities for any word not in training

### Huge web-scale n-grams

How to deal with, e.g., Google N-gram corpus

Pruning
- Only store N-grams with count > threshold.
- Remove singletons of higher-order n-grams
- Entropy-based pruning

Efficiency
- Efficient data structures like tries
- Bloom filters: approximate language models
- Store words as indexes, not strings
  - Use Huffman coding to fit large numbers of words into two bytes
- Quantize probabilities (4-8 bits instead of 8-byte float)

### Smoothing for Web-scale N-grams

- “Stupid backoff” (Brants et al. 2007)
- No discounting, just use relative frequencies 

"Stupid backoff" 是一种简化的平滑技术，它不涉及概率的减价（discounting），而是直接使用相对频率来估计N-gram概率。当存在足够的数据来估计更长的N-gram时，它使用这些N-gram；如果没有，它会退回到更短的N-gram，并乘以一个固定的系数（这里是0.4）来减少概率。这种方法比标准的backoff简单，因为它不需要调整概率以保证它们加和为1。

公式如下：

当 $\text{count}(w_{i-k+1}^i) > 0$ 时:
$$
S(w_i | w_{i-k+1}^{i-1}) = \frac{\text{count}(w_{i-k+1}^i)}{\text{count}(w_{i-k+1}^{i-1})}
$$
否则：
$$
S(w_i | w_{i-k+1}^{i-1}) = 0.4 \times S(w_i | w_{i-k+2}^{i-1})
$$

对于单个词的平滑概率是：
$$
S(w_i) = \frac{\text{count}(w_i)}{N}
$$

其中 $S(w_i | w_{i-k+1}^{i-1})$ 是在给定前面词的条件下，词 $w_i$ 的平滑概率，$\text{count}(w_{i-k+1}^i)$ 是N-gram $w_{i-k+1}^i$ 在语料库中的计数，N 是语料库中所有词的总数。


### N-gram Smoothing Summary

Add-1 smoothing:
- OK for text categorization, not for language modeling

The most commonly used method:
- Extended Interpolated Kneser-Ney

For very large N-grams like the Web:
- Stupid backoff

### Advanced Language Modeling

Discriminative models:
- choose n-gram weights to improve a task, not to fit the training set

Parsing-based models

Caching Models
- Recently used words are more likely to appear
  $$
    P_{\text{CACHE}}(w|h) = \lambda P(w|w_{i-2}w_{i-1}) + (1 - \lambda) \frac{c(w \in \text{history})}{|\text{history}|}
  $$
- These turned out to perform very poorly for speech recognition

## Advanced: Kneser-Ney Smoothing

### Absolute Discounting Interpolation

- Save ourselves some time and just subtract 0.75 (or some d)
  $$
    P_{\text{AbsoluteDiscounting}}(w_i | w_{i-1}) = \frac{c(w_{i-1}, w_i) - d}{c(w_{i-1})} + \lambda(w_{i-1}) P(w)
  $$
  - (Maybe keeping a couple extra values of d for counts 1 and 2)

### Kneser-Ney Smoothing

How many times does w appear as a novel continuationL:
$$
  P_{CONTINUATION}(w) \propto \{w_{i-1} : c(w_{i-1},w) > 0\}
$$

Normalized by the total number of word bigram types:
$$
  P_{CONTINUATION}(w) = \frac{|\{w_{i-1} : c(w_{i-1},w) > 0\}|}{|\{(w_{j-1}, w_j) : c(w_{j-1}, w_j) > 0\}|}
$$

Alternative metaphor: The number of # of word types seen to precede w
$$
  |\{w_{i-1}:c(w_{i-1}, w) > 0\}|
$$

normalized by the # of words preceding all words:
$$
  P_{CONTINUATION}(w) = \frac{|\{w_{i-1} : c(w_{i-1}, w) > 0\}|}{\sum_{w'}|\{w'_{i-1} : c(w'_{i-1}, w') > 0\}|}
$$

A frequent word (Kong) occurring in only one context (Hong) will have a low continuation probability

$$
P_{KN}(w_i | w_{i-1}) = \frac{\max(c(w_{i-1}, w_i) - d, 0)}{c(w_{i-1})} + \lambda(w_{i-1})P_{CONTINUATION}(w_i)
$$

其中：

- $P_{KN}(w_i | w_{i-1})$是Kneser-Ney平滑后的条件概率。
- $c(w_{i-1}, w_i)$是bigram $(w_{i-1}, w_i)$在语料库中的计数。
- $d$是一个折扣常数。
- $\lambda(w_{i-1})$是一个归一化常数，用来确保折扣后的概率和为1。

这个公式的目标是为了保证即使在bigram数据稀疏的情况下，也能为每个词分配一个合理的概率。

$$
P_{KN}(w_i | w_{i-n+1}^{i-1}) = \frac{\max(c_{KN}(w_{i-n+1}^i) - d, 0)}{c_{KN}(w_{i-n+1}^{i-1})} + \lambda(w_{i-n+1}^{i-1})P_{KN}(w_i | w_{i-n+2}^{i-1})
$$

在Kneser-Ney平滑中，针对一个特定的N-gram的计数 $c_{KN}()$ 会根据它是最高阶的N-gram还是低阶的N-gram而有所不同。公式如下：

对于最高阶的N-gram：
$c_{KN}(•) = count(•)$

对于低阶的N-gram：
$c_{KN}(•) = continuation \ count(•)$

Continuation count = Number of unique single word contexts for ·