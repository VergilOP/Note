# Week 10 Note

## Introduction

![](./images/Screenshot%202024-04-19%20233641.png)

![](./images/Screenshot%202024-04-19%20233718.png)

## Attention

### Self-Attention

- Self-attention
  - Every element in sequence can influence every other element
  - learn weighting(attention) for each pair of elements

- Attention: learnable pairwise weighting that depends on other sequence
- Self-Attention: use input sequence for attention

![](./images/Screenshot%202024-04-20%20000738.png)

![](./images/Screenshot%202024-04-20%20000958.png)

Application to sequence: matrix multiplication

$$
Attention(Q,K,V) = softmax(\frac{QK^T}{\sqrt{d_k}})V
$$

Q, K, V are computed from X (embedding of input sequence) with learned weight matrix

![](./images/Screenshot%202024-04-20%20001204.png)

Perceptron/Feed-Forward:
- Fixed weight matrix

![](./images/Screenshot%202024-04-20%20004327.png)

Self-Attention:
- Dynamic weight matrix
- Computed form inputs

![](./images/Screenshot%202024-04-20%20004337.png)

### Multi-headed self-attention

- Multiple attention heads for increased model capacity
- Combine with additional feed-forward layer

![](./images/Screenshot%202024-04-20%20004618.png)

## Decoder

- Interpret data as sequence
$$
    P_\theta(X) = \prod^n_{i=1}p_\theta(x_i|x_1,...,x_{i-1})
$$

- Train neural network
  - Input: previous values(x_1, ..., x_{i-1})
  - Distribution of possible next values $p_\theta(x_i|x_1,...,x_{i-1})$
    - E.g. as histogram
    - Or Parametric dist
  - Ensure correct receptive field, e.g. special convolutions
- Sampling:
  - One sample at a time
  - Slow, involves repeated application of model

## Training

![](./images/Screenshot%202024-04-20%20005717.png)