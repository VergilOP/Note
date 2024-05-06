# Week 6 Note

## From classical to deep learning-based methods in Computer Vision - Part 3

### Summary — Classical CV vs Deep Learning

#### Edge/Contour Detection — First-principle CV
- **Design Principles**: Mathematical models
- **Design Choices**: Edge models, noise models, first and second order derivatives, etc.; parameter settings
- **Advantages**: Compact, robust, stable, interpretable and quick to both train and evaluate; low on data, compute requirements
- **Disadvantages**: Performance is lacking
- **Conditions under which can be applied**: Clear understanding of the problem

#### Edge/Contour Detection — DL
- **Design Principles**: Data, network model
- **Design Choices**: Network meta parameters: number of layers, filters, pooling, etc., loss functions
- **Advantages**: Performance
- **Disadvantages**: Not compact, not interpretable and high on data, compute requirements
- **Conditions under which can be applied**: Data, compute, seek answers within the distribution of the training set

## From classical to deep learning-based methods in Computer Vision - Part 4

### Case II: PCA and Autoencoders

- PCA and Autoencoders
  - Both methods build models in an unsupervised way
  - Both methods minimise the reconstruction error
  - Both methods build a latent space than can be used for downstream tasks
  - Both methods implement encoder-decoder architecture
  - Orthogonal / non-orthogonal
  - Linear / non-linear
  - Expressiveness
  - Some potential applications

### Subspace estimation

- Find a low-dimensional linear subspace (i.e., a different coordinate system) such that projection of the data onto the subspace does not result in information loss.

![](./images/Screenshot%202024-05-02%20232611.png)

- We are given 𝑁 𝑀-dimensional points (images): $x_1, \dots, x_N;x_i \in \R^M$
- A toy example of finding 1D subspace in 2D data:
  - find a unit vector $𝒖 ∈ 𝑅^M$ such that projection to this vector will minimize the reconstruction error.  
    ![](./images/Screenshot%202024-05-02%20232843.png)

- Formally: Minimize the sum of reconstruction errors
  $$
    u_{opt} = \argmin_uE(u) = \argmax_uvar(a)
  $$
  ![](./images/Screenshot%202024-05-02%20233104.png)
- Reconstruction error minimization is equivalent to maximization of variance of projected points
  $$
    var(a) = \frac{1}{N}\sum^N_{i=1}a^2_i
  $$
  ![](./images/Screenshot%202024-05-02%20233004.png)

### PCA-geometric interpretation

- Calculate eigenvectors and eigenvalues of covariance matrix $\sum$
- Eigenvectors: main directions of variance, perpendicular to each other.
  $$
    U = [u_1, u_2]
  $$
- Eigenvalues: variance of data in direction of eigenvectors  
  ![](./images/Screenshot%202024-05-02%20234148.png)
- PCA is actually: change of coordinate system that captures major directions of variance in the data.

### Projection and reconstruction
- We know the covariance matrix 𝚺 and the mean value 𝝁
- Concatenate 𝑀 first (this case all) eigenvectors into a rotation matrix U:
  $$
    U = [u_1, ..., u_M]
  $$
  ![](./images/Screenshot%202024-05-02%20234814.png)
- Projection of data $x_i$ into the new coordinate system:
  $$
    y_i = U^T(x_i - \mu)
  $$
- Projection of $y_i$ back into the original coordinate system:
  $$
    x_i = Uy_i + \mu
  $$

- Similar holds also for K < M !
- Create U from just the first K eigen vectors:  
  $$
    \tilde{U} = [u_1, ..., u_K]
  $$
  ![](./images/Screenshot%202024-05-02%20235810.png)
- Projection to subspace:
  $$
    y_i = \tilde{U}^T(x_i - \mu)
  $$
- Reconstruction:
  $$
    \tilde{x}_i = \tilde{U}y_i + \mu
  $$
- Reconstruction error:
  $$
    \epsilon_i = ||x_i - \tilde{x}_i||
  $$

### Build your own subspace!

- Reshape all training images into column vectors: $[X_1, X_2, \dots, X_N]$
- Calculate the average image: $\mu = \frac{1}{N} \sum_{i=1}^N X_i$
- Center data: $X_d = [X_1 - \mu, X_2 - \mu, \dots, X_N - \mu]$
- Calculate the covariance matrix: 
  $$
  C = \frac{1}{N} \sum_{i=1}^N (X_i - \mu)(X_i - \mu)^T = \frac{1}{N} X_dX_d^T
  $$
- Calculate eigenvector matrix $U$ and eigenvalue matrix $S$ (using, e.g., SVD): $C = U S V^T$
- Construct a matrix using only first $K$ eigen vectors: $\tilde{U} = [u_1, \dots, u_K]$
- For each test image $x$:
  - Project to subspace: $y = \tilde{U}^T (x - \mu)$
  - Reconstruct:
    $$
    \hat{x} = \tilde{U}y + \mu
    $$

### Detection by distance from subspace

- Use a subspace learned on faces to detect a face.
- Approach: slide a window over each image position and calculate the reconstruction error.
- Repeat for all scales. Makes sense to normalize the window intensity |W|=1.
- Low reconstruction error indicates a face. (i.e., apply a threshold)
  $$
    ||\tilde{x}_i - x_i||^2 < \theta
  $$

### PCA is a linear autoencoder

![](./images/Screenshot%202024-05-03%20003219.png)
- Modern Autoencoders apply (e.g., convolutional) neural networks to map into a nonlinear subspace (latent space)

### Summary

From classical to machine (deep) learning-based computer vision approaches
![](./images/Screenshot%202024-05-03%20003338.png)

#### Traditional Computer Vision vs. Deep Learning

- Data availability
  - One of the main advantages of deep learning models is that they can learn from large amounts of data without requiring much feature engineering or domain knowledge. However, this also means that they need more data to train and generalize well. If you have a small or medium-sized dataset, you may not benefit much from deep learning models, as they may overfit or underperform. In such cases, traditional machine learning models, such as logistic regression, decision trees, or support vector machines, may be more suitable, as they can work well with less data and simpler features.
- Computational resources
  - Another factor to consider is the computational resources that you have available for your project. Deep learning models are often more complex and require more parameters than traditional machine learning models. This means that they need more processing power and memory to train and run. You may also need to use specialized hardware, such as GPUs or TPUs, to speed up the training and inference of deep learning models. If you have limited computational resources, you may want to opt for traditional machine learning models, as they are usually faster and cheaper to train and deploy.
- Interpretability and explainability
  - Another factor to consider is the interpretability and explainability of your model. Interpretability refers to how well you can understand the logic and reasoning behind your model's predictions, while explainability refers to how well you can communicate and justify your model's predictions to others. Deep learning models are often considered as black boxes, as they have many hidden layers and nonlinear transformations that make it hard to trace and explain their decisions. Traditional machine learning models, on the other hand, are often more transparent and easier to interpret and explain, as they have fewer and simpler components and rules. If you need to provide clear and understandable explanations for your model's predictions, especially for sensitive or regulated domains, such as healthcare, finance, or law, you may prefer traditional machine learning models over deep learning models.


