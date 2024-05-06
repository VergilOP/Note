# Week 8 Note

## Generative Adversarial Network

- What is a Generative Model?
  - A generative model describes how a dataset is generated, in terms of a probabilistic model. By sampling from this model, we are able to generate new data

- How can we learn $p_\theta(x) \approx p_{data}(x)$?
  - Generated data are samples from the *model's* probability density funtion of x
  - Training data are samples from the *real* probability density function of x

![](./images/Screenshot%202024-04-15%20055237.png)

### The Discriminator

![](./images/Screenshot%202024-04-15%20060550.png)

- Discriminator learns a decision boundary between real and fake samples. For each input, it predicts the probability that the input is real or fake

$$
\phi' = \arg\max_\phi \mathbb{E}_{x\sim p_{data}(x)} [\log D_\phi(x)] + \mathbb{E}_{x\sim p_{g}(x)} [\log (1 - D_\phi(G_{\theta}(z)))]
$$

$$
\phi' = \arg\max_\phi \mathbb{E}_{x\sim p_{data}(x)} [\log D_\phi(x)] + \mathbb{E}_{z\sim p_{z}(z)} [\log (1 - D_\phi(G_{\theta}(z)))]
$$

$$
\phi' = \arg\min_\phi \mathbb{E}_{x\sim p_{data}(x)} [-\log D_\phi(x)] + \mathbb{E}_{z\sim p_{z}(z)} [-\log (1 - D_\phi(G_{\theta}(z)))]
$$

- Can be understood as if D learns by minimizing 2 losses:
  - One for real samples (make D(x) go to 1)
  - One for fake samples (make D(G(z)) go to 0)

- Both losses are functions of D's parameters $\phi$
  - By backpropagation, we can learn optimal D.
  - Note: All this for "fixed" G parameters

In practice, SGD with 2 batches: batch of \(N_{D_r}\) real data and batch of \(N_{D_f}\) fake samples:

$$
\phi' = \arg\min_\phi \left[ \frac{1}{N_{D_r}} \sum_{i} \log D_\phi(x_i) + \frac{1}{N_{D_f}} \sum_{j} \log(1 - D_\phi(G_{\theta}(z_j))) \right]
$$

> - Note-1: The term over fake samples is **also a function of the Generator** parameters $\theta$
> - Note-2: The generated output G(z) **is “just” another activation map**, on top of which we connect the Discriminator.
> 
> - Therefore, we **can backpropagate through the generated output G(z).**
> - We can use this to alter parameters of G to change this term in the opposite direction and **confuse** the Discriminator

### Adversarial Training

$$
\{\theta', \phi'\} = \arg\min_\theta \arg\max_\phi \mathbb{E}_{x\sim p_{data}(x)} [\log D_\phi(x)] + \mathbb{E}_{x\sim p_{g}(x)} [\log(1 - D_\phi(G_{\theta}(z)))]
$$

$$
\{\theta', \phi'\} = \arg\max_\theta \arg\min_\phi \mathbb{E}_{x\sim p_{data}(x)} [\log D_\phi(x)] + \mathbb{E}_{z\sim p_{z}(z)} [\log(1 - D_\phi(G_{\theta}(z)))]
$$

where $J_{GAN}(\theta, \phi)$ is the Objective function of the Min-Max game.

- Two-player Min-Max game for optimization(from Game Theory):
  - For given(fixed)G, D tries to maximum D's accuracy of separating Reals from Fakes
  - For given(fixed)D, G tried to minimum D's accuracy of separating Reals from Fakes
  - Therefore called "adversarial"

#### Loss of G

For fixed Discriminator parameters $\phi$, we train Generator parameters $\theta$:

$$
\theta' = \arg\min_\theta \mathbb{E}_{x\sim p_{data}(x)} [\log D_\phi(x)] + \mathbb{E}_{z\sim p_{z}(z)} [\log(1 - D_\phi(G_\theta(z)))]
$$

$$
\theta' = \arg\min_\theta \mathbb{E}_{z\sim p_{z}(z)} [\log(1 - D_\phi(G_\theta(z)))]
$$

where $L_{G}(z)$ is the Loss (theoretical) of Generator which is a function of $\theta$.

In practice, SGD with a batch of fake samples (size $N_G$):

$$
\theta' = \arg\min_\theta \frac{1}{N_G} \sum_{i} \log(1 - D_\phi(G_\theta(z_i)))
$$

#### Training GANs Algorithm(Theoretical $L_G$)

Training GANs Algorithm (Theoretical $L_G$)

- $\theta$ — random initialization
- $\phi$ — random initialization

For $t = 1$ to $T$ do:

- For $k = 1$ to $K$ do: # Train D for K iterations of SGD (often $K=1$)
  - $\{x_i\}^{N_{D,r}}$, sampling $x_i \sim X_{train}$
  - $\{z_i\}^{N_{D,f}}$, sampling $z_i \sim N(0, I)$
  - $L_D(\phi, \theta) = L_{D,r}(\phi) + L_{D,f}(\phi, \theta) = -\frac{1}{N_{D,r}} \sum_i \log D_\phi(x_i) -\frac{1}{N_{D,f}} \sum_i \log(1 - D_\phi(G_\theta(z_i)))$
  - $\phi \leftarrow \phi - \alpha \frac{\partial L_D(\phi, \theta)}{\partial \phi}$

    \# Train G for 1 iteration of SGD
  - $\{z_i\}^{N_G}$, where $z_i \sim N(0, I)$
  - $L_G(\phi, \theta) = \frac{1}{N_G} \sum_i \log(1 - D_\phi(G_\theta(z_i)))$
  - $\theta \leftarrow \theta - \beta \frac{\partial L_G(\phi, \theta)}{\partial \theta}$

Return $\theta, \phi$

#### How does adversarial training achieve $p_{\theta}(x) \approx p_{data}(x)$?

The previous theoretical results means:

*If* D was perfectly optimized at every SGD iteration before updating G, then minimizing $J_{GAN}$ with $G_{\theta}^*$ *would* lead to minimization of JS divergence. This in turn means the generator *would* replicate learn the density of real data "perfectly", i.e. $p_{\theta}(x) \approx p_{data}(x)$.

But, in practice:  
a. we don't optimize D perfectly in each SGD iteration (only K updates, and SGD local minima),  
b. we often use another loss for G, rather than what we showed before (which was aligned with theory)...

Regardless, this theoretical result gives a nice intuition about how GANs work.

## Limitations of basic GAN and Advanced Models

### Problem: Generator's Loss is not decreasing

- Why?
  - Because D also improves during training. When D changes, if D reduces its loss, it simultaneously increases G’s loss.

- Solution?
  - Open research on alternative GAN losses

Theoretical loss:
$$
L_G(\mathbf{z}) = \log(1 - D_\phi(G_\theta(\mathbf{z})))
$$

Practical loss:
$$
L_G(\mathbf{z}) = -\log D_\phi(G_\theta(\mathbf{z}))
$$

**Generator's loss is a function of the Discriminator** and its parameters.

G's loss decreases when G improves during training and pushes $D(G(\mathbf{z}))$ to 1,   
but also increases when D improves during training and pushes $D(G(\mathbf{z}))$ to 0.

Therefore, even when results by G improve, we may see G's loss go up!

### Advanced models for unsupervised learning
- VAE-GAN
- Cycle GAN

