import numpy as np
import matplotlib.pyplot as plt

def estimate_true_yes(reported_yes, p):
    return (reported_yes + p - 1) / (2*p - 1)

def estimate_reported_yes(true_yes, p):
    return p * true_yes + (1-p) * (1-true_yes)

# Constants
TRUE_YES = 0.40  # = 40%

# Calculate for different p
x = np.linspace(0, 1, 40)
y = [estimate_reported_yes(TRUE_YES, p) for p in x]

# Plot
fig, ax = plt.subplots()
plt.xlabel('p')
plt.ylabel('reported yes')
ax.plot(x, y)
plt.show()

# Run Questionnaire
def run_questionnaire(p):
    n = 1000
    reported_yes = 0
    for _ in range(n):
        true_yes = np.random.random() <= TRUE_YES
        toss = np.random.random()
        if (toss <= p and true_yes) or (toss > p and not true_yes):
            reported_yes += 1
    return reported_yes / n

# Try to infer true yes
y2 = [estimate_true_yes(run_questionnaire(p), p) for p in x]

# Plot
fig, ax = plt.subplots()
plt.xlabel('p')
plt.ylabel('true yes')
ax.plot(x, y2)
plt.show()
