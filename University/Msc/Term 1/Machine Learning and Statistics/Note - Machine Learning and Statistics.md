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