import numpy as np

weights = np.array([0.352, 1.462])

# Given data
x_input = np.array([
    [3, 5],
    [2, 4],
    [5, 7],
    [4, 6],
    [6, 8],
    [8, 11],
    [1, 3],
    [7, 10],
    [9, 13],
    [10, 14]
])

y_target = np.array([
    9,
    6,
    12,
    10,
    14,
    19,
    5,
    17,
    22,
    24
])


def cost(w, X, y):
    predictions = X.dot(w)
    residuals = y - predictions
    mse = np.dot(residuals, residuals)/ (2*len(y))

    return mse

# Evaluate the cost
print(cost(weights, x_input, y_target))

