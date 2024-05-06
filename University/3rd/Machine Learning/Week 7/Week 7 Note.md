# Week 7 Note

## Kernel Functions

### Different types of inputs

- Numeric Inputs
  - Decision boundary
- Text of Various Lengths
  - Classify documents into types
- Strings of Various Lengths
  - classify protein's amino acid sequences into protein family
- Trees of Various Sizes
  - classify computer program as defective
- Graphs of Various Sizes
  - classify chemical compounds into toxic or non-toxic

- How to Deal With These Different Kinds of Input?
  - Create numeric features to represent them
    - Manually or automatically
    - The number of features may be very high
  - Create kernels to use with the dual representation of our approaches
    - Dual representation makes predictions based on inner products with respect to existing training examples

- Choosing Features Embeddings or Kernels For a Given Data Type
  - Devise an appropriate feature embedding and compute its kernel  
    设计一个合适的特征嵌入并计算其核
    - Feature mapping can map data types to numeric spaces  
      特征映射可以将数据类型映射到数值空间
    - The kernel should ideally be a function that can be computed more efficiently than an explicit evaluation of the feature mapping $\phi(x)$  
      核函数应该是比显式评估特征映射 $\phi(x)$ 更有效率的函数
  - Devise an appropriate similarity function that can be computed efficiently for the data type we are interested in and use it as a kernel  
    设计一个适合我们感兴趣的数据类型的高效可计算的相似性函数，并将其用作核
    - It needs to correspond to an inner product in some embedding(Mercer's Condition)  
      它需要对应于某个嵌入空间中的内积（满足Mercer条件）
    - Kernels can be defined for inputs that are not in vectorial format. e.g.,strings, graphs, text documents and sets  
      核函数可以为非向量格式的输入定义，例如字符串、图形、文本文档和集合。
    - Despite representing inner products in some feature space, we don't necessarily need to reflect about what exactly the feature embedding would be  
      尽管表示某些特征空间中的内积，我们不必一定要了解特征嵌入的确切内容。

### Kernels as similarity functions

- Linear Kernel
  $$
    \phi(\vec{x}) = \vec{x} \\
    k(\vec{x}, \vec{z}) = \vec{x}^T\vec{z}
  $$

- Polynomial Kernel
  $$
    \phi(\vec{x}) = (\text{all monomials of degree up to $p$})\\
    k(\vec{x},\vec{z}) = (1 + \vec{x}^T\vec{z})^p
  $$
  - More generically:
    $$
        k(\vec{x},\vec{z}) = f(k_1(\vec{x},\vec{z}))
    $$
    where $f$ is any polynomial of degree $\vec{p}$ with positive coefficients and $k_1(\vec{x}, \vec{z})$ is a kernel

- Gaussian Kernel
  - Gaussian kernel, a.k.a. Radial Basis Function(RBF) kernel
    $$
        k(\vec{x},\vec{z}) = e^{-\frac{||\vec{x} - \vec{z}||^2}{2\sigma^2}}
    $$
  > The embedding $\phi$ is infinite dimensional
  - Taylor series with infinite terms gives a representation of the true Gaussian itself
    $$
        k(\vec{x},\vec{z}) = \sum^\infin_{j=0}\frac{(\vec{x}^T\vec{z})^j}{j!}e^{-\frac{1}{2}||\vec{x}||^2}e^{-\frac{1}{2}||\vec{z}||^2}
    $$
  - Gaussian Kernel With Other Distance Functions
    - other Gaussian kernels could be defined with other distance metrics than the Eucidean distance in the original input space
      $$
        k(\vec{x},\vec{z}) = e^{-\frac{||\vec{x} - \vec{z}||^2}{2\sigma^2}}\\
        \darr\\
        ||\phi_1(\vec{x}) - \phi_1(\vec{z})||^2 = k_1(\vec{x}, \vec{x}) - 2k_1(\vec{x}, \vec{z}) + k_1(\vec{z}, \vec{z}) \\
        \darr\\
        k(\vec{x},\vec{z}) = e^{-\frac{k_1(\vec{x},\vec{x}) - 2k_1(\vec{x},\vec{z}+k_1(\vec{z},\vec{z}))}{2\sigma^2}}
      $$

### Examples of kernel functions

- Kernel for Sets
  - Let $A_1$ and $A_2$ be two subsets of a larger set $A$
    $$
        k(A_1, A_2) = 2^{|A_1 \bigcap A_2|}
    $$
    where $|A_i|$ denotes the number of elements in $A_i$

- Kernel for Text
  - Let $\vec{x}$ and $\vec{z}$ be two text documents, and a collection of documents $\mathscr{D}$
    $$
        \phi_1(\vec{x}) = (\text{bag of words})
    $$
    词袋模型  
    (vector with the number of occurrences of each possible term)  
    Cosine similarity(cosine of angle between $\phi_1(\vec{x})$ and $\phi_1(\vec{z})$)  
    使用余弦相似度作为核函数来计算文档间的相似性
    $$
        k(\vec{x},\vec{z}) = \frac{\phi_1(\vec{x}^T)\phi_1(\vec{z})}{\sqrt{\phi_1(\vec{x})^T\phi_1(\vec{x}) \times \phi_1(\vec{z})^T\phi_1(\vec{z})}}\\
        k(\vec{x}, \vec{z}) = \frac{k_1(\vec{x},\vec{z})}{\sqrt{k_1(\vec{x},\vec{x})k_1(\vec{z},\vec{z})}}
    $$
  - Let $\vec{x}$ and $\vec{z}$ be two text documents, and a collection of documents $\mathscr{D}$ where there are $d$ possible terms
    $$
        \phi_2(\vec{x}) = (tf-idf(i,\vec{x}, \mathscr{D}))^d_{i=1}
    $$
    TF-IDF(词频-逆文档频率)  
    (term frequency inverse document frequency for each possible term)
    $$
        tf-idf(i, \vec{x}, \mathscr{D}) = \frac{tf(i,\vec{x})}{df(i, \mathscr{D})}\\
        tf-idf(i, \vec{x}, \mathscr{D}) = tf(i,\vec{x}) \times idf(i, \mathscr{D})\\
        tf(i, \vec{x}) = \frac{\phi_{1i}(\vec{x})}{\sum^d_{j=1}\phi_{1j}(\vec{x})}\\
        idf(i, \mathscr{D}) = \ln\frac{|\mathscr{D}|}{|\vec{z} \in \mathscr{D} : i \in \vec{z}|}
    $$
  - Remarks on Text Data
    - You may still wish to remove stop-words for efficiency  
      去除停用词
    - You may also wish to apply stemming to further reduce the dimensionality of the problem  
      应用词干提取
      - Stemming removes inflexions of words

- Strings of Varying Sizes
  - A string $s$ is a sequence of characters from a given alphabet $\mathscr{A}$.
    $$
        s = s_1s_2...s_{|s|} \space \forall i \in \{1 , ..., |s|\}, s_i \in \mathscr{A}
    $$
  - Denote the set of all finite strings of length $m$ as $\mathscr{A}^m$
  - The set of all pssible strings is:
    $$ 
        \mathscr{A}^* = \bigcup^\infin_{i=1}\mathscr{A}^i
    $$
    
  - Subsequence
    - A sequence $u$ is a subsequence of $s$ if there exist indices $\vec{i} = (i_1, ... , i_{|u|})$ with $1 \leq i_1 < ... < i_{|u|} \leq |s|$ such that
      $$
        \forall j \in \{1, ..., |u|\}, u_j = s_{i_j}
      $$

  - All-Subsequence Kernel for Strings
    - Embedding corresponding to one dimension for each possible string of the alphabet, telling how many times that string appears as a substring of $s$:
      $$
        \phi(s) = (|\vec{i}: u = s(\vec{i})|)_{u \in \mathscr{A}^*}\\
        k(s,t) = \phi(s)^T\phi(t) = \sum^\infin_{i=1} \phi_i(s)\phi_i(t)
      $$  
      计算一个字符串的所有可能子序列在另一个字符串中出现的次数
    - Dynamic programming enables us to compute this more efficiently in $O(|s||y|)$
      - A technique where the solution to a bigger problem can be computed based on the solution for a smaller problem
      - This enables us to compute for larger subsequences based on what we have already computed for smaller subsequences

- All-Subtree Kernel
  - Assume $T_1$ and $T_2$ are trees that can be constructed with a given set of possible nodes, and $\mathscr{J}$ is the set of all possible trees
    $$
      \phi_S(T) = 1(S \in T) \space (\text{S is a subtree of T})\\
      k(T_1, T_2) = \phi(T_1)^T\phi(T_2) = \sum_{S \in \mathscr{J}}\phi_S(T_1)\phi_S(T_2)
    $$
    Dynamic programming can be used to compute this kernel in $O(|T_1||T_2|d^2_{\max})$, where $d_{\max}$ is the maximum number of children a node can have in these trees  
    动态规划可以用来高效地计算这种核函数，从而解决可能出现的计算复杂性问题

- Kernels for Graphs and Other Structures
  - Kernels for graphs and other sturctures that can be decomposed into smaller sub-structures can be defined using similar ideas to the ones  described for kernels for trees  
  对于图数据，核函数可以定义为测量图的子结构相似性。这些核函数通常基于图的子图或其他可以分解的小结构

### Summary

- Kernels are powerful tools
  - Can potentially enable us to compute inner products efficiently
  - Can enable use to deal with different data types

## Is Learning Feasible?

### Is learning feasible?

- Can limited training data reveal something about unseen test examples?
  - we can infer something outside $\mathscr{J}$ using only $\mathscr{J}$ but in a `probabilistic` way

#### Hoeffding Inequality

- Let $\mathscr{z}_1, ... , \mathscr{z}_N$ be random independent, indentically distributed random variables, such that $0 \leq \mathscr{z}_i \leq 1$
  $$
    p(|\frac{1}{N}\sum^N_{i=1}z_i - E_{z~p(z)}[z]| > \epsilon) \leq 2e^{-2\epsilon^2N} \text{ for any } \epsilon > 0
  $$
- Let $\mathscr{z}_1, ... , \mathscr{z}_N$ be random independent, indentically distributed `Bernoulli` random variables
  $$
    P(|\nu - \mu| \geq \epsilon) \leq 2e^{-2\epsilon^2N} \text{ for any } \epsilon > 0
  $$
  - $\mu$ is probability of red marbles("actual value")   
    是这个均值的真实期望值，通常是模型在整个数据分布上的平均错误率
    - Generalisation error across all possible examples
      $$
        E_{\text{out}}(h) = P_{\vec{x}~p(\vec{x})}(h(\vec{x}) \neq f(\vec{x}))
      $$
  - $\nu$ is fraction of red marbles in the sample("estimated value")   
    代表的是样本均值，比如一个分类模型在一组样本上的平均错误率  
    - Error estimated on the training set
      $$
        E_{\text{in}}(h) = \frac{1}{N}\sum^N_{i=1}1(h(\vec{x}^{(n)}) \neq f(\vec{x}^{(n)}))
      $$
  - $\epsilon$是我们设置的阈值，代表我们关心的误差界限
  - $N$ 是样本的数量
  - Probability that actual and estimated values are different by more than $\epsilon$  
    模型在样本上观测到的平均错误率$\nu$与模型在整个数据分布上的真实平均错误率$\mu$之间的差距超过$\epsilon$的概率，是非常小的。
  - $h$ has to be fixed beforehand to calculate $E_{\text{in}}(h)$ based on training examples drawn i.i.d., so that $1(h(\vec{x}^{(n)}) \neq f(\vec{x}^{(n)}))$ is i.i.d.. We can't choose $g$ based on these training examples and apply this inequality


- 霍夫丁不等式的作用主要是在模型训练之后，用来估计模型在未知数据上的表现。它不直接参与训练过程。简而言之，霍夫丁不等式是一种衡量标准，帮助我们了解：
  - 在给定样本上训练的模型在未知数据上表现可能有多好。
  - 给定一个观测误差，我们可以多大程度上信任模型在整个数据分布上的真实误差与观测误差接近。

- This is a `bound` on the maximum value of the probability, for any underlying distribution
  - It is `not a tight bound`, but useful for understanding machine learning and its feasibility
  - Chances of the differences being larger are bounded by smaller probability values.  
    差异较大的可能性受到较小概率值的限制。
  - Larger sample sizes reflect smaller chances that the difference between actuall and estimated value is larger than $\epsilon$  
    样本量越大反映实际值与估计值之间的差异大于 $\epsilon$ 的可能性越小

- Considering the Hypothesis Set
  $$
    P(|E_{\text{in}}(h) - E_{\text{out}}(h)| > \epsilon) \leq 2e^{-2\epsilon^2N}
  $$
  Consider a finite hypothesis set $\mathscr{H} = \{h_1, h_2, ..., h_M\}$
  $$
    P(|E_{\text{in}}(h) - E_{\text{out}}(h)| > \epsilon) \rarr P(|E_{\text{in}}(h_1) - E_{\text{out}}(h_1)| > \epsilon \\
    or |E_{\text{in}}(h_2) - E_{\text{out}}(h_2)| > \epsilon \\
    ...\\
    or |E_{\text{in}}(h_M) - E_{\text{out}}(h_M)| > \epsilon)
  $$
  > - Rule of probabilities:  
  >   if $z_1 \rarr z_2$, then $P(z_1) \leq P(z_2)$
  > - Rule of probabilities(union bound):  
  >   $P(z_1\ or\ z_2\ or\ \dots\ z_M) \leq P(z_1) + P(z_1) + \dots + P(z_M)$
  $$
    P(|E_{\text{in}}(g) - E_{\text{out}}(g)| > \epsilon) \leq \sum_{i=1}^MP(|E_{\text{in}}(h_i) - E_{\text{out}}(h_i)| > \epsilon)\\
    P(|E_{\text{in}}(g) - E_{\text{out}}(g)| > \epsilon) \leq 2Me^{-2\epsilon^2N}
  $$
  The bound above can be used with a hypothesis $g$ chosen based on the training set

> - It makes sense to estimate the generalisation error based on the training error
> - Larger sample sizes will likely lead to better estimation of generalisation error
> - It makes sense to trying and minimise the training error,but a smaller training error may require a larger hypothesis set, potentially resulting in the training error being a less good approximation of the gneralisation error
> - There is a complex relationship between model complexity, training error and generalisation error

#### Generalisation Bound

- With probability at least $1 - \delta$
  $$
    E_{\text{in}}(g) - \sqrt{\frac{1}{2N}\ln\frac{2M}{\delta}} \leq E_{\text{out}}(g) \leq E_{\text{in}}(g) + \sqrt{\frac{1}{2N}\ln\frac{2M}{\delta}}
  $$
  Smaller training error is limiting the generalisation error to smaller values  
  以 1−δ 的置信度，真实误差将在训练误差的一个特定范围内

#### Summary

- The Hoeffding Inequality can be used to discuss the feasibility of learning
- It shows use that larger training sets will likely lead to training errors more similar to the generalisation error
  - Reducing the training error is likely to reduce the generalisation error
- However, to reduce the training error, we may need a larger hypothesis set
  - Larger hypothesis sets are likely to lead to more different training and generalisation errors
- There is a trade-off between model complexity and generalisation
- Caveat: our analyses were considering a finite number of hypothesis, but our machine learning algorithms usually have infinite hypothesis sets

### Examples of kernel functions

- Different Terminologies
  - The term kernel can also be used with other meanings
  - Some simply define a kernel as a real valued function of two arguments $k(\vec{x}, \vec{z}) \in \R$
  - Some simply define a kernel as a real valued function of two arguments $k(\vec{x}, \vec{z}) \geq 0$
  - The term kernel is also used to refer to a matrix which is slid across an image and multiplied with the input such that the output is enhanced in a desired manner(used in Convolutional Neural Networks)

### Kernel machines

- One can create basis expansions based on kernels, where $\mu_i$ are "centroids"
  $$ 
    \phi(\vec{x}) = (k(\vec{x},\mu_1), k(\vec{x},\mu_2), \dots, k(\vec{x},\mu_D))
  $$
  通过核函数，可以创建一个特征空间的基扩展，其中的每个特征对应于输入空间中的一个点与“中心”之间的相似度
- We call this a kernelised feature vector, where the kernel typically corresponds to a similarity metric but does not need to satisfy the Mercer's condition  
  通过将原始特征通过核函数映射到一个高维空间，我们得到了一个核化的特征向量。这个过程不需要显式地计算高维空间中的特征，而是通过核函数隐式地进行计算
- We can then use a linear model such as linear regression or logistic regression with this basis expansion, making it non-linear  
  使用核化的特征向量，可以将线性模型（如线性回归或逻辑回归）扩展为能够处理非线性关系的模型。
- If the kernel is the Gaussian kernel, this gives rise to the Radial Basis Function Network
  如果使用的是高斯核，这种方法会导致所谓的径向基函数网络（Radial Basis Function Network, RBFN）的创建。

#### Summary

- Kernels are powerful tools
  - May enable us to bettwer separate training example through the feature embedding that they represent
  - May enable us to use such feature embeddings without having to compute them
  - May enable us to deal with different types of input features