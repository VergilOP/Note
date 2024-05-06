# Week 5 Note

## Machine Learning Linear Regression

- Ordinary Least Squares(OLS)
  $$
    R(\omega) = \sum^N_{i=1}r^2_i = \sum^N_{i=1}(y_i - \hat{y}_i(x_i, \omega))^2
  $$
  - Setting derivates of R w.r.t $\omega$ to 0:
    $$
    \frac{\partial R(\omega_0, \omega_1)}{\partial \omega_0} = 0
    $$

    $$
    \frac{\partial R(\omega_0, \omega_1)}{\partial \omega_1} = 0
    $$

  - This results in a linear system of two equations with two unknown($\omega_0, \omega_1$)
    $$
    \sum_{i=1}^{N} x_i y_i = \omega_0 \sum_{i=1}^{N} x_i + \omega_1 \sum_{i=1}^{N} x_i^2
    $$

    $$
    \sum_{i=1}^{N} y_i = \omega_0 N + \omega_1 \sum_{i=1}^{N} x_i
    $$

  - Expressing the system of equations in matrix form:
    $$
    \begin{pmatrix}
    \sum_{i=1}^{N} x_i y_i \\
    \sum_{i=1}^{N} y_i
    \end{pmatrix} = 
    \begin{pmatrix}
    \sum_{i=1}^{N} x_i & \sum_{i=1}^{N} x_i^2 \\
    N & \sum_{i=1}^{N} x_i
    \end{pmatrix}
    \begin{pmatrix}
    \omega_0 \\
    \omega_1
    \end{pmatrix}
    $$

    $$
    \begin{pmatrix}
    \omega_0 \\
    \omega_1
    \end{pmatrix} = 
    \begin{pmatrix}
    \sum_{i=1}^{N} x_i & \sum_{i=1}^{N} x_i^2 \\
    N & \sum_{i=1}^{N} x_i
    \end{pmatrix}^{-1}
    \begin{pmatrix}
    \sum_{i=1}^{N} x_i y_i \\
    \sum_{i=1}^{N} y_i
    \end{pmatrix}
    $$
  
  - The equation is the OLS solution for $\omega_0, \omega_1$ for a degree = 1 polynomial regression function

- OLS: For arbitrary degree polynomial
  - Fitting an M-degree polynomial requires estimation of M + 1 parameters in $\omega$
  - Using $\omega, \phi$ we can define $R(\omega)$ as: $R(\omega) = \sum^N_{i=1}(y_i - \omega^T\phi(x_i))^2$
  - As before we can estimate $\omega_{OLS}$ by minimising $R(\omega)$
    $$
    \frac{\partial R(\omega)}{\partial \omega} = \sum_{i=1}^{N} (y_i - \omega^T \phi(x_i))\phi(x_i)^T = 0
    $$
    $$
    \sum_{i=1}^{N} y_i \phi^T (x_i) = \omega^T \left( \sum_{i=1}^{N} \phi(x_i) \phi^T (x_i) \right)
    $$
  - $\Phi \text{ is an } N \times (M+1) \text{ matrix where each } x_i \text{ has an associated basis function vector } \phi(x_i)$

  - By defining design matrix \( \Phi \) as:

    $$
    \Phi = \begin{pmatrix}
    \phi_0(x_1) & \phi_1(x_1) & \cdots & \phi_M(x_1) \\
    \phi_0(x_2) & \phi_1(x_2) & \cdots & \phi_M(x_2) \\
    \vdots & \vdots & \ddots & \vdots \\
    \phi_0(x_N) & \phi_1(x_N) & \cdots & \phi_M(x_N)
    \end{pmatrix}
    $$

  - $\omega_{OLS}$is given by solving: $\Phi^T Y = \Phi^T \Phi \omega$ -- the normal equation

    $$
    \omega_{OLS} = (\Phi^T \Phi)^{-1} \Phi^T Y
    $$

  - $(\Phi^T \Phi)^{-1} \Phi^T = \Phi^+$ is also known as the Moore-Penrose pseudoinverse of $\Phi$

> - The solution can be obtained by this set of linear equations (the normal equation).
> - However, numerical inversion of matrices can be troublesome, especially if the matrix is large.


- Evaluating Regression Models
  - Common metrics for evaluating regression models
    - Coefficient of determination or $R^2 = 1 - \frac{\sum_i(y_i-\hat{y}_i)^2}{\sum_i(y_i-\bar{y})^2}$; $\bar{y}$ is the mean of the observed targets
    - Mean absoute error(MAE) = $\frac{1}{N}\sum^N_i|y_i-\hat{y}_i|$
    - Mean squared error(MSE) = $\frac{1}{N}\sum^n_i(y_i-\hat{y}_i)^2$
    - Root mean squared error(RMSE) = $\sqrt{\frac{1}{N}\sum^N_i(y_i-\hat{y}_i)^2}$

- Formalization:
  - Input: $\vec{x}$
  - Output: $y$
  - Target function: $f : X \rarr Y$
  - Data: $(\vec{x}_1, y_1), (\vec{x}_2, y_2), ...,(\vec{x}_N, y_N)$
  - Hypothesis: $g : X \rarr Y$
- Cost function
  - in-sample error: $E_{in}(h) =  \frac{1}{N}\sum\limits^N_{n=1}(h(\vec{x}_n) - y_n)^2 = \frac{1}{N}||X\vec{w}-y||^2$

> - Linear regression with linear and non-linear basis function
>   - Polynomial basis functions:
>     $$
>       w_0+w_1x^2_1+w_2x_2^2+...+w_Dx^2_D
>     $$
>   - Gaussian basis functions/radial basis functions
>     $$
>       \phi_j(x) = e^{-\frac{1}{2\sigma^2}(x-\mu_j)^2}
>     $$
>   - Sigmoidal basis functions
>     $$
>       g(\alpha) = \frac{1}{1+e^{-\alpha}}
>     $$
>   - tanh basis functions
>     $$
>       h(\alpha) = \frac{e^{2\alpha}-1}{e^{2\alpha}+1}
>     $$

### Summary

- Linear and non-linear basis functions may be used to formulate a linear regression function
- OLS used to estimate linear regresssion weights by minimising sum of squared residuals
- OLS solution boild down to computing pseudoinverse of the Design Matrix
- Linear regression models can be fit to data using gradient descent

## Machine Learning SVM Regression

- Support Vector Regression
  - Find a function, $f(x)$ with at most $\epsilon$-deviation from the target $y$
  - We don't care about errors as long as they are less than $\epsilon$
  - Only the pint ouside the $\epsilon$-region contribute to the final cost
  $$
    \min\frac{1}{2}||w||^2\\
    s.t. y_i - w_1x_i - b \leq \epsilon;\\
    w_1x_i + b - y_i \leq \epsilon;
  $$

  $$
    J(w) = \underbrace{\frac{1}{2}w'w}_\text{正则化防过拟合} + C\sum^N_1(\xi+\xi*);\\
    y_i - (x_iw + b) \leq \epsilon + \xi_i\\
    (x_iw + b) - y_i \leq \epsilon + \xi_i^*\\
    \xi^* \leq 0\\
    \xi_i \leq 0
  $$

- Hyperparameter $C$
  - As $C$ increases, our tolerance for points outside of $\epsilon$ also increases
  - As $C$ approaches 0, the tolerance approaches 0 and the quation collapes into the simplified(although sometimes infeasible) one

> 希望确保模型的预测值落在真实值的一个ε区域内，或者至少尽可能地靠近这个区域。slack variables $\xi$ 和 $\xi^*$ 允许我们有一些灵活性，即当预测值与真实值之间的差异大于ε时，它们会吸收这种差异。

- Optimizing the Lagrangian
  $$
    L := \frac{1}{2}||w||^2 + C\sum^l_{i=1}(\xi_i+\xi_i^*) - \sum^l_{i=1}(\mu_i\xi_i+\mu_i^*\xi^*_i)\\
    - \sum^l_{i=1}\alpha_i(\epsilon + \xi_i - y_i + \langle w,x_i \rangle + b)\\
    - \sum^l_{i=1}\alpha_i(\epsilon + \xi_i^* + y_i - \langle w,x_i \rangle - b)\\
  $$
  Lagrange multipliers $\alpha_i^{(*)}, \mu_i^{(*)} \leq 0$
  > - Optimizing the Lagrangian
  >   - The partial derivatives of $L$ with respect to the variables
  >   $$
  >     \delta_bL = \sum^l_{i=1}(\alpha^*_i-\alpha_i) = 0\\
  >     \delta_wL = w - \sum^l_{i=1}(\alpha_i - \alpha_i^*)x_i = 0\\
  >     \delta_{\xi_i^{(*)}}L = C - \alpha_i^{(*)}- \mu_i^{(*)} = 0
  >   $$

  $$
    maximize\\
      -\frac{1}{2}\sum^l_{i,j=1}(\alpha_i - \alpha_i^*)(\alpha_j - \alpha_j^*)\langle x_i, x_j\rangle-\epsilon\sum^l_{i=1}(\alpha_i+\alpha_i^*)+\alpha^l_{i=1}y_i(\alpha_i-\alpha_i^*)

  $$
  > Subject to $\sum^l_{i=1}(\alpha_i-\alpha_i^*) = 0$ and $\alpha_i, \alpha_i^* \in [0, C]$

- SVM: Regression vs Classification  
  ![](./images/SVM.png)

### Summary

- Linear regression tries to minimize the error between the real and predicted value
- SVR tries to fit the best line within a threshold value
- The threshold value is the distance between the hyperplane and boundary line
- Observations within the threshold of epsilon produce no error, only the observation outside the epsilon range produce error - sparse kernel machines