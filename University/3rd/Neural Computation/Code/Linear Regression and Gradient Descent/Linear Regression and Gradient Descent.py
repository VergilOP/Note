import pandas as pd
import numpy as np
import random
import matplotlib.pyplot as plt
import os

# 更改当前工作目录
os.chdir('C:/_Study_Resource/Study_Note/University/3rd/Neural Computation/Code')
# print(os.getcwd())

data = pd.read_csv("student_scores.csv")

x_input = data['Hours'].values.reshape(-1,1)
y_target = data['Scores'].values

# axis=1 竖直合并
x_vectorized = np.concatenate([np.ones([x_input.shape[0], 1]), x_input], axis=1)

def linearmodel(w, b, x):
    return np.dot(w, x) + b

# bias不作为权重
def linearmat(w, b, X):
    n = X.shape[0]
    t = np.zeros(n)
    for i in range(n):
        t[i] = linearmodel(w, b, X[i, :])
    return t

# bias作为权重
def linearmat_vectorized(w, X):
    return np.dot(X, w)

def cost(w, X, y):
    residual = y - linearmat_vectorized(w, X)
    err = np.dot(residual, residual) / (2 * len(y))

    return err

def gradfn(w, X, y):
    y_pred = linearmat_vectorized(w, X)
    error = y_pred - y
    return np.dot(X.T, error) / len(y)

def solve_via_gradient_descent(X, y, print_every=100, niter = 5000, eta = 0.01):
    D = np.shape(X)[1]
    w = np.zeros([D])
    idx_res = []
    err_res = []
    for k in range(niter):
        dw = gradfn(w, X, y)
        w = w - eta * dw
        if k % print_every == print_every - 1:
            t_cost = cost(w, X, y)
            idx_res.append(k)
            err_res.append(t_cost)
    print('error after %d iteration: %s' % (k, t_cost))
    return w, idx_res, err_res

def solve_via_minibatch(X, y, print_every=100, niter = 5000, eta = 0.01, batch_size = 10):
    N, D = np.shape(X)
    w = np.zeros([D])
    idx_res = []
    err_res = []
    tset = list(range(N))
    for k in range(niter):
        idx = random.sample(tset, batch_size)
        sample_X = X[idx, :]
        sample_y = y[idx]
        dw = gradfn(w, sample_X, sample_y)
        w = w - eta * dw
        if k % print_every == print_every - 1:
            t_cost = cost(w, X, y)
            idx_res.append(k)
            err_res.append(t_cost)
    print('error after %d iteration: %s' % (k, t_cost))
    return w,idx_res,err_res

def solve_exactly(X, y):
    A = np.dot(X.T, X)
    c = np.dot(X.T, y)
    return np.dot(np.linalg.inv(A), c)

w_exact = solve_exactly(x_vectorized, y_target)
err = cost(w_exact, x_vectorized, y_target)

print(w_exact)
print(err)

w_gd, idx_gd, err_gd = solve_via_gradient_descent( X=x_vectorized, y=y_target)
w_batch, idx_batch, err_batch = solve_via_minibatch( X=x_vectorized, y=y_target)