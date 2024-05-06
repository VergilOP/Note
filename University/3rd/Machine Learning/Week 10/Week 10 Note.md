# Week 10 Note

- Two Curves for Overfitting
  - Regularisation: Putting the brakes
  - Cross-Validation: Checking the bottom line

## Regularisation

$$
    E_{out}(h) \leq E_{in}(h) + \Omega(H),\text{ for all }h \in H
$$
- Model complexity penalty $\Omega(H)$
- Smaller $E_{out}(h)$, samller $\Omega(H)$
  $$
    minimize_{h}\space E_{in}(h) + \Omega(h)
  $$
  ![](./images/Screenshot%202024-04-21%20174752.png)
- Regularisation with **Hard** Constraint  
  ![](./images/Screenshot%202024-04-21%20194057.png)
- Regularisation with **Losser** Constraint  
  ![](./images/Screenshot%202024-04-21%20194140.png)
- Regularisation with **Softer** Constraint  
  ![](./images/Screenshot%202024-04-21%20194228.png)

### Matrix Form of Regularized Regression Problem

![](./images/Screenshot%202024-04-21%20195100.png)

![](./images/Screenshot%202024-04-21%20195137.png)

![](./images/Screenshot%202024-04-21%20195210.png)

![](./images/Screenshot%202024-04-21%20215534.png)

![](./images/Screenshot%202024-04-21%20215727.png)

![](./images/Screenshot%202024-04-22%20223512.png)

![](./images/Screenshot%202024-04-22%20223608.png)

### L2 and L1 Regulariser

![](./images/Screenshot%202024-04-22%20223716.png)

- LASSO (Least Absolute Shrinkage and Selection Operator)
  - LASSO regularization promotes a sparse solution.
  - If the underlying model has a sparse solution, e.g., you choose a 50th-order polynomial, but the underlying model is a 3rd-order polynomial, then there should only be three non-zero regression coefficients in your 50th-order polynomial. LASSO will help in this case.
  - If the underlying model has a dense solution, then LASSO is of limited value. A Ridge Regression could be better.

![](./images/Screenshot%202024-04-22%20223905.png)

![](./images/Screenshot%202024-04-22%20223927.png)

### The Optimal $\lambda$

![](./images/Screenshot%202024-04-21%20215808.png)

![](./images/Screenshot%202024-04-22%20224609.png)

### Linear Regression: Bayesian perspective

![](./images/Screenshot%202024-04-22%20224900.png)

- MLE = OLS
- MAP = Regularised Regression
- MAP estimate for Bayesian linear regression results in quadratic regulariser of weights (Gaussian case)
- Quadratic regulariser (ridge regression) - shrinks weights, reduces overfitting (form of regularised least squares)

### Summary

- Whenever you train a model, try including regularization.
- Helps dramatically when there is noise in data, not enough data, complex target.
- Hand-waving argument: noise is high frequency. Complex target is also high frequency.
- So low-frequency regularization helps.
- As long as you have a good λ, the benefifit of regularization is often more than the harm.
- Modern deep learning can easily incorporate regularization.
- E.g., you can regularize the magnitude of the network weights, or number of non-zeros through sparsity.

## Validation

![](./images/Screenshot%202024-04-22%20225341.png)

### Model Selection Probelm

