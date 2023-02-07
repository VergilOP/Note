- [AI2 Note](#ai2-note)
  - [Maximum Likelihood](#maximum-likelihood)
    - [Introduction](#introduction)
    - [Maximum Likelihood](#maximum-likelihood-1)
    - [Solving Maximum Likelihood Estimator](#solving-maximum-likelihood-estimator)
      - [Gradient descent](#gradient-descent)


# AI2 Note

## Maximum Likelihood

### Introduction

- To describe or summarise the data, we can use **Descriptive Statistics**: a summary that quantitatively describe our data in hand, the data we currently have. The simplest method is called Univariate analysis
  - Central tendency: **expectation**(mean), median, mode, etc.
  - Dispersion: the range and quartiles of the dataset,
  - Spread: **variance** and standard deviation
  - Shape of the distribution: skewness and kurtosi

### Maximum Likelihood

- **Bernouli distribution**: P(B) = θ and P(W) = 1 - θ
$$L(θ|x) = \prod_{i=1}^{N} y_{j}\begin{cases}
  θ \qquad\quad if\ x_i = B\\
  1 - θ \quad if\ x_i= W\\
\end{cases}$$
- This probability is called **likelihood** function L, which is a function of θ, given the samples(observation) x.
- **Likelihood**(似然): The probability of observing your data given a particular model.

- Likelihood function: general definition
  - Let n Xn denoted compactly using a vector
  - **X** = (X<sub>1</sub>, X<sub>2</sub>...X<sub>n</sub>) be a random sample from a distribution with a parameter θ.  
    Suppose we have observed that X<sub>1</sub> = x<sub>1</sub> ..., denote a vector **x** = (x<sub>1</sub>,...), we can define the likelihood function as
    - If $X_i$'s are discrete:  
      $$ 
        L(θ|x)  = L(θ|x_1,x_2,...,x_n)\\
                = P_x(x;θ),
      $$  
      where $P_x(x;θ)$ is the PMF of X parametrised by θ
    - If $X_i$'s are continuous:  
      $$
        L(θ|x)  = L(θ|x_1,x_2,...,x_n)\\
                = f_x(x|θ),
      $$  
- Note: In general, θ can be a vector, $θ = (θ_1,θ_2,...,θ_k)$

  ![Likelihood curve](./images/Likelihood%20curve.png)  
  Figure: Likelihood function $L(θ|x),θ\in[0,1]$

- Note that the likelihood is not a probability function - the area under the likelihood curve **does not have to sum to one**

- Difference between the Probability and Likelihood
  - **Probability**: **a number** $p\in[0,1]$ between 0 to 1 to describe how likely and event is to occur, or how likely it is that a proposition is true, assuming we know the distribution of the data.
  - **Likelihood**: **a function** that measures the gooodness of fit of a statistical model to a sample of data for given values of the unknown parameters. It is a functin of the unknown parameters(e.g. θ)
  > So the fundamental difference between Probability and Likelihood is their aims.  
  > The aim of probability calculation is to find a number $p\in[0,1]$ between 0 to 1 to describe how likely an event is to occur, or how likely it is that a proposition is true, assuming we know the distribution of the data.   
  > The aim of Likelihood calculation is to find the best distribution of the data  

### Solving Maximum Likelihood Estimator

- **Maximum likelihood estimation**: Informally, based solely on the data, Maximum likelihood estimation searches the best parameters of a probability distribution that makes the data most likely
  - Changing the value of θ will change the value of the function L(θ|x)
  - The bigger the value, the better the model fit
  - 
#### Gradient descent