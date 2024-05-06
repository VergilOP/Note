# Week 3 Note

## The Task of Text Classification

- Basic Sentiment Classification
    - 基本情感分类
  - Sentiment analysis is the detection of attitudes
    - 情感分析是态度的检测

### Test Classification

- Sentiment analysis
    - 情感分析
- Spam detection
    - 垃圾邮件检测
- Authorship identification
    - 作者识别
- Language Identification
    - 语言识别
- Assigning subject categories, topics, or genres
    - 分配主题类别、主题或流派

### Classification Methods

#### Hand-coded rules
- Rules based on combinations of words or other features
    - 基于单词组合或其他特征的规则
  - spam: black-list-address OR (“dollars” AND “you have been selected”)
    - 垃圾邮件：黑名单地址或（“美元”和“您已被选中”）
- Accuracy can be high
    - 准确度可能很高
  - If rules carefully refined by expert
    - 如果规则由专家精心完善
- But building and maintaining these rules is expensive
    - 但建立和维护这些规则的成本很高

#### Supervised Machine Learning

- Input:
  - a document `d`
  - a fixed set of classes $C = {c_1, c_2,…, c_J}$
  - A training set of `m` hand-labeled documents $(d_1,c_1),....,(d_m,c_m)$
- Output:
  - a learned classifier $γ:d \rarr c$ 

- Any kind of classifier
  - Naïve Bayes
  - Logistic regression
  - Neural networks
  - k-Nearest Neighbors
  - …

### The Naive Bayes Classifier

- Naive Bayes Intuition
  - Simple ("naive") classification method based on Bayes rule
  - Relies on very simple representation of document
    - Bag of words(词袋模型)

- Bayes’ Rule Applied to Documents and Classes
  - For a document d and a class c
    $$
        P(c|d) = \frac{P(d|c)P(c)}{P(d)}
    $$

- Naive Bayes Classifier
  $$
    c_{MAP} = \argmax_{c\in C}P(c|d) = \argmax_{c\in C}P(d|c)P(c) = \argmax_{c\in C}P(x_1, x_2, ..., x_n|c)P(c)
  $$

- `Bag of Words assumption`: Assume position doesn’t matter
- `Conditional Independence`: Assume the feature probabilities $P(x_i|c_j)$ are independent given the class c

- Multinomial Naive Bayes Classifier
  $$
    C_{NB} = \argmax_{c\in C}P(c_j)\prod_{x\in X}P(x|c)
  $$

## Naive Bayes: Learning

- First attempt: maximum likelihood estimates
  - simply use the frequencies in the data
  $$
    \hat{P}(c_j) = \frac{N_{c_j}}{N_{total}}\\
    \hat{P}(w_i|c_j) = \frac{count(w_i, c_j)}{\sum_{w\in V}count(w, c_j)}
  $$

### Parameter estimation

Fraction of times word $w_i$ appears among all words in documents of topic $c_j$

Create mege-document for topic j by concatenating all docs in this topic 
- Use frequenc of w in mega-docuiment

### Problem with Maximum Likelihood

- Zero probabilities cannot be conditioned away, no matter the other evidence
  $$
    c_{MAP} = \argmax_cP^(c)\prod_i\hat{P}(x_i|c)
  $$

### Laplace(add-1) smoothing for Naive Bayes

$$
\hat{P}(w_i \mid c) = \frac{\text{count}(w_i, c) + 1}{\sum_{w \in V} (\text{count}(w, c) + 1)} = \frac{\text{count}(w_i, c) + 1}{\left(\sum_{w \in V} \text{count}(w, c)\right) + |V|}
$$

- Unknown words: Ingore
  - Reason: doesn't help
- Stop words: very frequent words like `the` and `a`
  - Remove doen't usually help

## Sentiment and Binary Naive Bayes

- Binary multinomial naive bayes, or binary NB
  - Clip our word counts at 1
  - Note: this is different than Bernoulli naive bayes; see the textbook at the end of the chapter(次数多≠情感强烈)

## More on Sentiment Classification

- `not_like` vs `like`
  - Add `NOT_` to every words between negation

didn't - but 

### Using Lexicons in Senti-Classifi

- Add a feature that gets a count wherever a word from the lexicon occurs

