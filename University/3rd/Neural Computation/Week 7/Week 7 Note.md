# Week 7 Note

# Generative Modeling and Variational Auto-Encoders (VAEs)

## Generative Models

### What is Gnerative Models?

- A generative model describes `how a data set is generated`, in terms of a `probabilistic model`. By `sampling` from this model, we are `able to generate new data`
  $$
    g: Z \rarr X
  $$
  - Generated new data point  
    ![](../images/Week%207/Screenshot%202024-04-13%20100726.png)

- Recognition(discriminative) models:
  $$
    f: X \rarr Z(\text{or } Y)
  $$
  ![](../images/Week%207/Screenshot%202024-04-13%20100903.png)

- For Basic AEs
  - Given only input samples x, how to learn a meaningful encoding
    $$
      f_\theta: X \rarr Z
    $$
  - This was the original motivation for the creation of AEs
  - They are `not trained/designed for generation`(sampling) of new data points

### Probability Density Estimation

- One of the main aims of unsupervised approaches and Generative Modelling
- Goal of Density EstimationL
  - We could try to fit a probabilistic model $p_\theta(x)$ to the data to learn their underlying distribution $p_\text{data}(x)$

## VAE

### From basic deterministic AE to Variational AE

- VAE(informally):
  - Make encoder that maps each input to an 'area'
  - Forces decoder to learn to decode z from whole coverd area

> - Training: Enforce codes z to cover a specified area
> - Generation: We can then sample from anywhere in the area. Decoding should then work

![](../images/Week%207/Screenshot%202024-04-13%20101837.png)

- Basic AE:
  - During training, learn to map each input to a point
  - Encoder leaves space between mapped points, to enable reconstruction whithout confusion

![](../images/Week%207/Screenshot%202024-04-13%20101946.png)

- VAE(more accurately)
  - Learn encoder that predicts codes z that follow distribution p(z)
  - Forces decoder to learn to decode z from whole p(z)

> - Training: Enforce codes z to follow a prior distribution p(z)
> - Generation: We can then sample from prior p(z) and decode!

![](../images/Week%207/Screenshot%202024-04-13%20102240.png)

### Probabilistic Encoder

- Each x is mapped to a multi-variate Normal/Gaussian distribution of z, $p_\phi(z|x) = N(\mu_\phi(x), \sigma_\phi(x)^2)$ by predicting a mean and standard deviation for each dimension of z  
  ![](../images/Week%207/Screenshot%202024-04-13%20102603.png)

> - The final layer of VAE's encoder has 2 times the number of neurons than the encoder of a standard AE
> - For z of dimension v, $z \in \R^v$, encoder of VAE has $2 \times v$ neurons
> - For each dimension of z, the standard AE's encoder predicts on value, the code. The AE's encoder is deterministic
> - For each dimension of z, the VAE's encoder predicts the mean and standard deviation of a Gaussian distribution
> - This distribution describes "which values of z are likely the correct code" for input x. VAE's encoder is probabilistic/stochastic

![](../images/Week%207/Screenshot%202024-04-13%20103045.png)

> Q: What would be the appearance of an image that would be mapped with a mean in the overlapping area of $x_1$ and $x_2$?  
> ![](../images/Week%207/Screenshot%202024-04-13%20103250.png)  
> A: Appearance that would be in-between those of $x_1$ and $x_2$

- Common solution in implementations:
  - Treat Encoder as if it predicts $\log(\sigma_\phi(x))$
    $$
      log\_stddev = matrix\_mult(act\_layer1, w\_layer2)
    $$
  - Compute sigma as: $\sigma_\phi(x) = \exp(\log(\sigma_\phi(x)))$
    $$
      stddev = exp(log\_stddev)
    $$

### Stochastic Decoder(during training)

1. Sample a code $\widetilde{Z}$ from the predicted Gaussian $p_\phi(z|x)$
2. Decode $\widetilde{Z}$ with a standard decoder, similar to a basic AE

- At each training iteration of SGD, for each input x in the batch, we draw 1 sample
- However, at different SGD iterations, different samples may be drawn for the same x.
- For any input x and predicted $p_\phi(z|x)$ it is more likely that we will sample value $\widetilde{Z}$ that is close to the predicted mean $\mu_\phi(x)$, rather than further
- At different SGD iterations, the decoder should try to reconstruct the input for any drawn sample, even the unlikely ones

![](../images/Week%207/Screenshot%202024-04-13%20104139.png)

### Training Objective

$$
L_{VAE} = L_{rec} + \lambda L_{reg}
$$

$$
\{\theta', \phi'\} = \arg \min_{\theta,\phi} \mathbb{E}_{x\sim p_{data}} [L_{VAE}] = \arg \min_{\theta,\phi} \mathbb{E}_{x\sim p_{data}} [L_{rec}] + \mathbb{E}_{x\sim p_{data}} [L_{reg}]
$$

"Minimize the Expected Value of the VAE loss, when $x$ is sampled from the data distribution"

In other words: "Minimize the VAE loss on average over the training data $x$"

#### Reconstruction loss

$$
L_{rec} = \frac{1}{d} \sum_{j=1}^{d} (x^{(i)}_j - g_{\theta}(z^{(i)}_j))^2
$$

for $\tilde{z} \sim p_{\theta}(z|x)$ — Reconstruct input $x$, for any $\tilde{z}$ sampled from $p_{\theta}(z|x)$.

d - Dimensionality of x  
$g_\phi$ - Function of $\phi$(decoder params)  
$\widetilde{Z}$ - Function of $\phi$(encoder params)

> The Reconstruction loss is a function of all parameters  
> Therefore it trains BOTH the encoder AND the decoder

> Q: How can the encoder reduce overlapping areas by changing the predicted means?  
> A: Learn parameters that predict means that are away from each other.  
> 
> Q: How can the encoder reduce overlapping areas by changing the predicted standard deviations?  
> A: Learn parameters that predict small standard deviations.  
> ![](../images/Week%207/Screenshot%202024-04-13%20103250.png)  
> ![](../images/Week%207/Screenshot%202024-04-13%20105311.png)  

If we would train a VAE `only with the Reconstruction loss`, its learned parameters would make it behave `exactly like` the basic deterministic AE!

So, the “probabilistic encoder” architecture is not sufficient! We need the Regularizer!

#### Regularizer

What this regularizer does:
Learn encoder parameters $\phi$ such that on average over the training data, we minimize the “Kullback Leibler (KL) Divergence” between the predicted posterior istribution of z codes, $p_\phi(z|x)$, and the Gaussian distribution $N(0, I)$

$$
  L_\text{reg} = D_{KL} \left[ p_{\phi}(z|x) || \mathcal{N}(0, I) \right] = \frac{1}{2} \sum_{j=1}^{v} \left[ (\mu_{\phi}^{(j)}(x))^2 + (\sigma_{\phi}^{(j)}(x))^2 - 2\log \sigma_{\phi}^{(j)}(x) - 1 \right]
$$

- Effect of Regularizer:
  - If we would train the VAE to only minimize the Regularizer:
    - For each x, predicted `mean` tends to 0
    - For each x, predicted `std. deviation` tends to 1
  - For every sample, the predicted posterior $p(z|x)$ takes the shape of the prior $p(z)$

- $L_{rec}$
  - Means that "cluster" the samples
  - Samll std.Devs. that reduce overlap
- $L_{reg}$
  - Means around 0
  - Std.Devs that cover the prior

### Re-parameterization trick

Q: 在传统的设置中，直接从分布中抽样是不可微的，这意味着无法直接通过反向传播来计算梯度（因为抽样操作没有梯度）。  
A: 重参数化技巧通过将随机部分移出梯度路径，使得损失函数相对于网络参数变得可微。

$$
\tilde{z} \sim \mathcal{N} (\mu_{\phi}(x), \sigma_{\phi}(x)^2) \Leftrightarrow \tilde{z} = \mu_{\phi}(x) + \sigma_{\phi}(x) \odot \epsilon, \text{ with } \epsilon \sim \mathcal{N}(0, I)
$$

### Training

#### Generating (synthesizing) new data with VAE

Even for VAE, some “gaps” can still be left in space of Z. Why?
1. Reconstruction loss encourages separation of dissimilar data "opposing" the regularizer
2. SGD optimization does not find global optimum

![](../images/Week%207/Screenshot%202024-04-13%20111341.png)

#### Interpolating between different inputs with VAE

- AlgorithmL
  1. Encode inputs x and get $\mu_\phi(x)$ as z
  2. Create new z code by interpolation
  3. Decode z with decoder

#### Altering specific features of data with VAE

Algorithm:

1. **Encode** original input $x$ and use predicted $\mu_{\phi}(x)$ as its code:
   E.g. $z_1 = \mu_{\phi}(x_1)$
2. Identify all training samples that have the desired "target" characteristic. E.g. blondes. Assume these are $x_{t1}, x_{t2}, \ldots$
3. **Encode** all training samples with the target characteristic.
   Use mean of the Gaussian predicted by encoder as the code.
   E.g. $z_{t1} = \mu_{\phi}(x_{t1}), z_{t2} = \mu_{\phi}(x_{t2}), \ldots$
4. Compute average value of codes of all samples with target characteristic: $z_{t,avg} = \text{average}(z_{t1}, z_{t2}, \ldots)$
5. Create new $z$ code by interpolation:
   E.g. $z = z_1 + \alpha(z_{t,avg} - z_1)$
6. **Decode** $z$ with decoder.

#### Reconstruction with VAE vs Basic AE

VAE’s capacity is used to optimize both reconstruction and regularization losses.  
These two losses have competing goals!  
Therefore its reconstructions may not be as good as those from a basic AE of similar capacity.  
It is not what VAE is made for  (VAE的设计目的并不是仅仅为了重构精度，而是为了能够生成新的数据样本，其方式是学习输入数据的潜在分布)