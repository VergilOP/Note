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
    (term frequency inverse document frequency for each possible term)
    $$
        tf-idf(i, \vec{x}, \mathscr{D}) = \frac{tf(i,\vec{x})}{df(i, \mathscr{D})}\\
        tf-idf(i, \vec{x}, \mathscr{D}) = tf(i,\vec{x}) \times idf(i, \mathscr{D})\\
        tf(i, \vec{x}) = \frac{\phi_{1i}(\vec{x})}{\sum^d_{j=1}\phi_{1j}(\vec{x})}\\
        idf(i, \mathscr{D}) = \ln\frac{|\mathscr{D}|}{|\vec{z} \in \mathscr{D} : i \in \vec{z}|}
    $$

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

- Kernels for Graphs and Other Structures
  - Kernels for graphs and other sturctures that can be decomposed into smaller sub-structures can be defined using similar ideas to the ones  described for kernels for trees

### Summary

- Kernels are powerful tools
  - Can potentially enable us to compute inner products efficiently
  - Can enable use to deal with different data types

### Kernel machines