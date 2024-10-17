# Note - Machine Learning and Statistics

## Lecture 1

### Precise & accuracy

- **Precise** measurement: the spread of results is "**small**"
- **Accurate** measurement: result is in agreement with "**accepted**" value

### Mean, standard deviation, standard error

- **Mean**: where is the measurement centred
    $$
        \overline{x} = \frac{1}{N}(x_1 + x_2 + \dots + x_n) = \frac{1}{N}\sum\limits^N_{i=1}x_i
    $$
- **Standard deviation**: width of the distribution
    $$
        \sigma_{n-1} = \sqrt{\frac{(d^2_1 + d^2_2 + \dots + d^2_N)}{N - 1}} = \sqrt{\frac{1}{N - 1}\sum\limits^N_{i=1}d^2_i}
    $$
    where $d_i = x_i - \overline{x}$
- **Standard error**: uncertainty in the location of the centre, $\alpha$
    $$
        \alpha = \frac{\sigma_{N-1}}{\sqrt{N}}
    $$
    We should quote our finding as $\overline{x} \plusmn \alpha$

> Consider an experiment with N number of data points collected:
> - the standard deviation is independent of N  
>   标准差不受样本量 𝑁 的影响, 因为它只关心数据的分散程度
> - the standard error improves with N  
>   标准误差随样本量 𝑁 的增加而减小, 意味着随着收集更多的数据, 均值估计会更加精确

### Random errors, the normal distribution

- **Gaussian or Normal Distribution**
    $$
        f(x) = \frac{1}{\sigma\sqrt{2\pi}}\exp[-\frac{(x-\overline{x})^2}{2\sigma^2}]
    $$
    > Facts: peak centred around mean, symmetric about mean,area under curve equals 1(normalised)

- The error in the error
    $$
        \alpha^{\plusmn} = \text{error in the error} = \frac{1}{\sqrt{2N-2}}
    $$

> Bigger sample size:
> - lower error in the error
> - more confidence
> - can quote more significant figures
>
> Need $N=50$ for the error to be known to 10%
> Need $N > 10k$ for the error to be known below 1%

### Reporting results, Confidence limits and error bars

- The five golden rules for reporting results
  1. The best estimate for a parameter is the mean
  2. The error is the standard error in the mean
  3. Round up the error to the appropriate number of significant figures
  4. Match the number of decimal places in the mean to the standard error
  5. Include units

- What is the probability of the data to lie within some multiple of $\sigma$?”
    - We need to evaluate the **error function** of the Gaussian distribution(G)
        $$
            Erf(x_1;\overline{x}, \sigma) = \int^{x_1}_{-\infty}G(x;\overline{x},\sigma)
        $$

        $$
           f(x) = \frac{1}{\sqrt{2\pi}} e^{-x^2 / 2}
        $$
        $$
            \text{CDF}(x) = \frac{1}{2} \left[ 1 + \text{erf}\left( \frac{x}{\sqrt{2}} \right) \right]
        $$
        $$
             P(-\sigma \leq X \leq \sigma) = \text{erf}\left( \frac{\sigma}{\sqrt{2}} \right)
        $$

- The standard deviation is thus used to define a **confidence level** on the data
- If your result and the accepted value differ by:
  - Up to 1 standard error it is in **excellent agreement**
  - Between 1 and 2: **reasonable agreement**
  - More than 3 standard errors: **disagreement**

### Poisson distribution

- The conditions under which a Poisson distribution holds are:
  - Events are rare
  - Events are independent
  - Tha average rate does not change with time
  $$
    P(N;\overline{N}) = \frac{\exp(-\overline{N})\overline{N}^N}{N!}
  $$
  > Mean = $\overline{N}$
  > Standard deviation = $\sqrt{\overline{N}}$

### Chauvenet's Criterion 

**Chauvenet's Criterion** 是一种统计学方法, 用于检测和判断数据集中是否存在离群值(outliers)。离群值是指与其他数据点偏差较大的数据点。Chauvenet 的准则基于标准正态分布, 提供了一种方法来确定一个数据点是否与数据集的其他部分显著不同。

### 使用 Chauvenet's Criterion 的步骤：

1. **计算数据的均值(mean, $\mu$)和标准差(standard deviation, $\sigma$)**。

2. **计算每个数据点与均值的偏差**：使用以下公式计算每个数据点与均值的标准化差值(即 Z 值)：
   $$
   Z = \frac{|x_i - \mu|}{\sigma}
   $$
   其中 $x_i$ 是第 $i$ 个数据点, $\mu$ 是均值, $\sigma$ 是标准差。
   
3. **计算离群值的概率**：利用正态分布, Z 值代表的是数据点与均值的偏差程度。对于正态分布, 计算该数据点的累计概率。这个概率表示数据点在多大程度上可以被视为异常值。

4. **判断数据点是否为离群值**：根据数据点的数量 $N$, Chauvenet's Criterion 提供了一个门槛。如果一个数据点的概率低于：
   $$
   P = \frac{1}{2N}
   $$
   则该数据点可以被视为离群值并被拒绝(REJECT), 否则接受(ACCEPT)。

## Lecture 2

### Error propagation

**Objective**:
1. Understanding how to propagate the error is **a vital part of data analysis and reduction**.  
    了解如何传播错误是**数据分析和减少错误的重要部分**。
2. Understanding which factors contribute to the limiting error is **a vital part of experimental design**.  
    了解哪些因素导致了限制误差是**实验设计的重要部分**。

$$
    \alpha_\text{speed} \neq \alpha_\text{distance} + \alpha_{time}
$$
$$
    \alpha_\text{speed} \approx \text{speed}\sqrt{(\frac{\alpha_d}{d})^2 + (\frac{\alpha_t}{t})^2}
$$

#### Single-variable functions 单变量误差传播

##### Functional approach
$$
    \overline{Z} \plusmn \alpha_Z = f(\overline{A} + \alpha_A) \\
    \overline{Z} = f(\overline{A}) \\
    \overline{Z} \mp \alpha_Z = f(\overline{A} - \alpha_A)
$$

- Valid for every single-varible function
    $$
        \alpha_Z = |f(\overline{A} + \alpha_A) - f(\overline{A})|
    $$

##### Calculus-based approach

For $Z = f(A)$:

$$
    P = (\overline{A}, f(A))\\
    Q = (\overline{A} + \alpha_A, f(\overline{A}))\\
    R = (\overline{A} + \alpha_A, f(\overline{A}) + \frac{df}{dA} \times \alpha_A)\\
    S = (\overline{A} + \alpha_A, f(\overline{A} + \alpha_A))\\
    f(\overline{A}) + \frac{df(A)}{dA}\alpha_A = f(\overline{A} + \alpha_A)
$$

![](./images/Single-variable%20functions.png)

$$
    \alpha_Z = |\frac{dZ}{dA}|\alpha_A
$$

#### Multi-variable functions 多变量函数的误差传播

##### Functional approach

Consider a function of two variables, $Z = f(A, B)$
The error of Z has 2 components:
- Change in Z when A is varied and B is constant  
  $$
    \alpha^A_Z = f(\overline{A} + \alpha_A, \overline{B}) - f(\overline{A}, \overline{B})
  $$
- Change in Z when B is varied and A is constant 
  $$
    \alpha^B_Z = f(\overline{A}, \overline{B} + \alpha_B) - f(\overline{A}, \overline{B})
  $$

The total error on Z is obtained via Pythagoras, adding in quadrature:
$$
    (\alpha_Z)^2 = (\alpha^A_Z)^2 + (\alpha^B_Z)^2 + (\alpha^C_Z)^2 + ...
$$

> $$
>     (\alpha Z)^2 = 
>   \left[ f(\bar{A} + \alpha_A, \bar{B}, \bar{C}, \dots) - f(\bar{A}, \bar{B}, \bar{C}, \dots) \right]^2 \\
>   + \left[ f(\bar{A}, \bar{\bar{B}} + \alpha_B, \bar{C}, \dots) - f(\bar{A}, \bar{B}, \bar{C}, \dots) \right]^2 \\
>   + \left[ f(\bar{A}, \bar{B}, \bar{C} + \alpha_C, \dots) - f(\bar{A}, \bar{B}, \bar{C}, \dots) \right]^2 \\
>   + \dots
> $$

- Calculus approximation
$$
    (\alpha_Z)^2 = (\frac{\partial Z}{\partial A})^2(\alpha_A)^2 + (\frac{\partial Z}{\partial B})^2(\alpha_B)^2 + (\frac{\partial Z}{\partial C})^2(\alpha_C)^2 + ...
$$

### Least Squares Method 最小二乘法

#### The importance of residuals 残差的重要性

残差=实际值−模型预测值
$$
    R_i = y_i - y(x_i)
$$
> 最佳拟合直线应该使所有残差尽可能小。

#### The goodness-of-fit parameter 拟合优度参数

Determining the optimal values of parameters for a function is called **regression analysis**.  
确定函数参数的最优值称为回归分析。

The **best-fit straight line** is the one that is close to as many data points as possible -> residuals will be small  
**最佳拟合直线**是尽可能接近更多数据点的直线 -> 残差会很小

This is quantified by the **goodness-of-fit parameter**, $X^2$:
- When $X^2$ is minimised, the probability that we obtain our original set of measurements from the best-fit straight line, is maximised.  
    当 $X^2$ 最小化时，我们从最佳拟合直线获得原始测量值集合的概率最大化。
$$
    X^2 = \sum\limits_i\frac{(y_i - y(x_i))^2}{\alpha^2_i}
$$

> What we have just done is called **method of least squares** (= minimising the sum of the squares of the residuals)  
> 我们刚刚做的叫做最小二乘法（=最小化残差平方和）  
> In the case of the straight line, the best values of slope and intercept are those that minimise the summed differences squared  
> 对于直线，斜率和截距的最佳值是最小化平方和的值  
> This is derived from what is called **maximum likelihood** together with the **central limit theorem** (assumption: the parent distribution from which we draw the yi values is Gaussian width width given by the standard error, ⍺i)  
> 这是从所谓的最大似然和中心极限定理中得出的（假设：我们从中得出 yi 值的父分布是高斯宽度，由标准误差 ⍺i 给出）
> The y-coordinate we get from the best-fit line equation is the **most probable** value of the parent distribution  
> 我们从最佳拟合线方程中得到的 y 坐标是父分布的最可能值  
> The probability of obtaining our measurement values from the best-fit line is maximised when χ2 is minimised  
> 当 χ2 最小化时，从最佳拟合线获得测量值的概率最大化

- $X^2$ for data with Poisson errors
$$
    X^2 = \sum\limits_i\frac{(O_i-E_i)^2}{E_i}
$$

> i: number of counts(bins)
> $O_i$: observed number of occurrences(in the i-th bin)
> $E_j$: expected number of occurrences(in the i-th bin)

> If we have a goot fit, $\alpha_i = \sqrt{E_i} \approx \sqrt{O_i}$

#### Minimisation 最小化

The best values for **slope** and **intercept** are those that minimise the squares of the differences summed for all data points  
斜率和截距的最佳值是最小化所有数据点之差的平方和

$$
    S = \sum\limits_i(y_i - y(x_i))^2 = \sum\limits_i R^2_i\\
    S = \sum\limits_i(y_i - mx_i - c)^2 
$$

$$
    \frac{\partial S}{\partial m} = -2\sum_i(x_i[y_i - mx_i - c]) = 0\\
    \frac{\partial S}{\partial c} = -2\sum_i(y_i - mx_i - c) = 0
$$

$S$ is a minimum when $\frac{\partial S}{\partial m} = \frac{\partial S}{\partial c} = 0$

The required values of the slope (m) and the intercept (c) are obtained from the two simultaneous equations  
斜率 (m) 和截距 (c) 的所需值可通过两个联立方程得出

$$
    m\sum_i x^2_i + c\sum_i x_i = \sum_i x_i y_i
$$
$$
    m\sum_i x_i + cN = \sum_i y_i
$$

- The solutions for gradient, intercept, and their uncertainties reduce to simple analytic expressions  
截距 𝑐 的公式
$$
    c = \frac{\sum_ix^2_i\sum_iy_i - \sum_ix_i\sum_ix_iy_i}{\Delta}, \alpha_c = \alpha_{CU}\sqrt{\frac{\sum_ix^2_i}{\Delta}}
$$

斜率 𝑚 的公式：
$$
    m = \frac{N\sum_ix_iy_i - \sum_ix_i\sum_iy_i}{\Delta}, \alpha_m = \alpha_{CU}\sqrt{\frac{N}{\Delta}}
$$

> 共同不确定性:    
> Common uncertainty: $\alpha_{CU} = \sqrt{\frac{1}{N - 2}\sum_i(y_i - mx_i - c)^2}$

总不确定性参数 Δ 
$$
\Delta = N\sum_ix^2_i - (\sum_i x_i)^2
$$

#### non-uniform error bars

Need to perform a **weighted** least-squares fit to take them into account
$$
    R_i = \frac{y_i - y(x_i)}{\alpha_i}
$$
The sum of the squares of the normalised residuals is called $X^2$

This is now a weighted fit, and we need to take this into account in the analytic expressions for $m, c, \alpha_c$, with $w_i = \alpha^{-2}_i$

> Points with small errors are more important!

