# Problem 1 (Gradient for quadratic functions)

In this problem we develop gradients for quadratic function

1. We first consider the two-dimensional case. Let

    $$
        A = \begin{pmatrix} a_{1,1} & a_{1,2}\\  a_{2,1} & a_{2,2}\end{pmatrix} \space x = \vec{y} = \begin{pmatrix} x_1 \\  x_2 \end{pmatrix} \in \R^2
    $$

    and 

    $$
        f(\vec{x}) = \vec{x}^TA\vec{x} = a_{1,1}x_1^2 + (a_{1,2} + a_{2,1})x_1x_2 + a_{2,2}x^2_2
    $$

    Prove that

    $$
        \triangledown f(\vec{x}) = \begin{pmatrix} a_{1,1} & a_{1,2}\\  a_{2,1} & a_{2,2}\end{pmatrix}\begin{pmatrix} x_1 \\  x_2 \end{pmatrix} + \begin{pmatrix} a_{1,1} & a_{2,1}\\  a_{1,2} & a_{2,2}\end{pmatrix}\begin{pmatrix} x_1 \\  x_2 \end{pmatrix}
    $$

    > 首先，有:
    > 
    > $$f(\vec{x}) = \vec{x}^T A \vec{x} = x_1^2 a_{1,1} + x_1 x_2 (a_{1,2} + a_{2,1}) + x_2^2 a_{2,2}$$
    > 
    > 1. 对 $x_1$ 求偏导:
    >    $$
    >        \begin{align}
    >            \frac{\partial}{\partial x_1} f(\vec{x}) &= 2x_1 a_{1,1} + x_2(a_{1,2} + a_{2,1}) \\
    >            &= a_{1,1} x_1 + a_{1,2} x_2 + a_{2,1} x_1 \\
    >            &= \begin{pmatrix} a_{1,1} & a_{1,2}\\  a_{2,1} & a_{2,2}\end{pmatrix} \begin{pmatrix} x_1 \\  x_2 \end{pmatrix}
    >        \end{align}
    >    $$
    > 
    > 2. 对 $x_2$ 求偏导:
    >
    >    $$
    >        \begin{align}
    >            \frac{\partial}{\partial x_2} f(\vec{x}) &= x_1 (a_{1,2} + a_{2,1}) + 2x_2 a_{2,2} \\
                    &= a_{2,1} x_1 + a_{2,2} x_2 + a_{1,2} x_1\\
                    &= \begin{pmatrix} a_{1,1} & a_{2,1}\\  a_{1,2} & a_{2,2}\end{pmatrix} \begin{pmatrix} x_1 \\  x_2 \end{pmatrix}
    >        \end{align}
    >    $$
    > 
    > 将两个偏导组合成一个向量（梯度），得到:
    > 
    >    $$
    >        \triangledown f(\vec{x}) = \begin{pmatrix} a_{1,1} & a_{1,2}\\  a_{2,1} & a_{2,2}\end{pmatrix}\begin{pmatrix} x_1 \\  x_2 \end{pmatrix} + \begin{pmatrix} a_{1,1} & a_{2,1}\\  a_{1,2} & a_{2,2}\end{pmatrix}\begin{pmatrix} x_1 \\  x_2 \end{pmatrix}
    >    $$

2. We now turn to more general cases. Let

    $$
        A = \begin{pmatrix} a_{1,1} & a_{1,2} & ... &  a_{1,d} \\  a_{2,1} & a_{2,2} & ... &  a_{2,d} \\  ... & ... & ... & ... \\  a_{d,1} & a_{d,2} & ... &  a_{d,d}\end{pmatrix} \text{and} \vec{x} = \begin{pmatrix} x_1 \\  x_2 \\ ... \\ x_d \end{pmatrix}
    $$

    Define

    $$
        f(\vec{x}) = \vec{x}^TA\vec{x} = \sum\limits^d_{i,j=1}a_{i,j}x_ix_j
    $$

    Prove that
    
    $$
        \triangledown (\vec{x}^TA\vec{x}) = A\vec{x} + A^T\vec{x}
    $$

    > 首先，有:
    > 
    > $$f(\vec{x}) = \vec{x}^T A \vec{x} = \sum\limits^d_{i,j=1}a_{i,j}x_ix_j$$
    > 
    > 考虑 $f(\vec{x})$ 关于 $x_k$ 的偏导数：
    >    $$
    >        \frac{\partial}{\partial x_k} f(\vec{x}) = \frac{\partial}{\partial x_k}(\sum\limits^d_{i,j=1}a_{i,j}x_ix_j)
    >    $$
    > 
    >    $$
    >       \frac{\partial}{\partial x_k} f(\vec{x}) = \sum_{j=1}^d a_{k,j} x_j + \sum_{i=1}^d a_{i,k} x_i 
    >    $$
    >    这相当于 $A \vec{x}$ 的第 $k$ 项加上 $A^T \vec{x}$ 的第 $k$ 项。
    >
    >    可得
    >    $$
    >        \triangledown (\vec{x}^TA\vec{x}) = A\vec{x} + A^T\vec{x}
    >    $$