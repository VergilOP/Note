# Week 11 Note

## Introduction to diffusion models

- Text to image generation
  - Way of learning to generate new stuff given many examples
  - Way to compress images(for speed in training and generation)
  - Way to link text and images
  - Way to add in good image-related inductive bias*

## Denoising diffusion probabilistic models

DDPM:
- Denoising(D):  gradually remove the noise to synthesise realistic image (reverse diffusion process)
- Diffusion (D): gradually diffuse/add noise to input image (forward diffusion process)
- Probabilistic Models (PM): model that learns probability distribution

### Diffusion model overview
- A DDPM consists of two processes:
  - Forward diffusion process that gradually adds noise to input
  - Reverse denoising process that learns to generate data by denoising

![](./images/Screenshot%202024-04-20%20160801.png)

- **Forward diffusion (noising)**
  - $x_0 \rightarrow x_1 \rightarrow \ldots \rightarrow x_{T-1} \rightarrow x_T$
  - Take a data distribution $x_0 \sim p(x)$, turn it into noise by diffusion $x_T \sim \mathcal{N}(0, I)$

- **Reverse diffusion (denoising)**
  - $x_T \rightarrow x_{T-1} \rightarrow \ldots \rightarrow x_1 \rightarrow x_0$
  - Sample from the noise distribution $x_T \sim \mathcal{N}(0, I)$, learn to reverse the diffusion process to generate data $x_0 \sim p(x)$

- **A diffusion model learns to produce a slightly more "denoised" image $x_{t-1}$ from a "noisy" image $x_t$**

### Notation

- $p(x)$ – data distribution
- $q(x_0)$ – data distribution

- $\mathcal{N}(0, I)$ – Gaussian distribution with 0 mean and identity $(I)$ variance
- $x_T \sim \mathcal{N}(0, I)$ – drawing a random sample from this Gaussian distribution

- $\mathcal{N}(x;\mu, \sigma I)$ – drawing a random sample from Gaussian distribution
  - $x$: random sample (e.g., image)
  - $\mu$: mean
  - $\sigma$: variance
  - $I$: identity matrix

- $x = \mu + \sqrt{\sigma} \varepsilon$ where $\varepsilon \sim \mathcal{N}(0, I)$
  - $\mathcal{N}(x;\mu, \sigma I)$ or $x \sim \mathcal{N}(\mu, \sigma I)$

![](./images/Screenshot%202024-04-20%20161257.png)

![](./images/Screenshot%202024-04-20%20161306.png)

### Aside: UNet

![](./images/Screenshot%202024-04-20%20161732.png)

![](./images/Screenshot%202024-04-20%20161728.png)

### Model training

#### Network architectures

![](./images/Screenshot%202024-04-20%20161856.png)

#### Diffusion parameters

![](./images/Screenshot%202024-04-20%20161914.png)

### Algorithm

- model traning  
![](./images/Screenshot%202024-04-20%20162005.png)

- Sampling from a trained model  
![](./images/Screenshot%202024-04-20%20162242.png)

## Extensions

1. Conditional diffusion model
2. Cascaded diffusion model
3. Super-resolution diffusion model
4. Image-to-image diffusion model
5. Semantic segmentation diffusion model
6. Latent diffusion model
