import numpy as np
import matplotlib.pyplot as plt

# Constants
EPS = 0.5
SENSITIVITY = 100000/11

# Laplace
draws = np.random.laplace(0, SENSITIVITY/EPS, size=10000)

# Plot
fig, ax = plt.subplots()
ax.hist(draws, bins=40, linewidth=0.5, edgecolor="white")
plt.show()

# Example from Practice Quiz
result = 54000 + np.random.laplace(0, SENSITIVITY/EPS)
