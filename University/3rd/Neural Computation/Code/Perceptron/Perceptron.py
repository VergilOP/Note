import numpy as np
import pandas as pd
import random
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score
import os

# 更改当前工作目录
os.chdir('C:/_Study_Resource/Study_Note/University/3rd/Neural Computation/Code')
# print(os.getcwd())

data = pd.read_csv("Single Layer Perceptron Dataset.csv")
data = data.dropna()

features = data[['Feature1', 'Feature2', 'Feature3']].values

features_vectorized = np.concatenate([np.ones([features.shape[0], 1]), features], axis=1)

labels = data['Class_Label'].values

class Perceptron():
    def __init__(self, b = 0, max_iter = 1000):
        self.max_iter = max_iter
        self.w = []
        self.b = b
        self.no_examples = 0
        self.no_features = 0

    def train(self, X, Y):
        self.no_examples, self.no_features = np.shape(X)
        self.w = np.zeros(self.no_features)

        for ii in range(0, self.no_examples):
            w_updated = False
            for jj in range(0, self.no_examples):
                a = self.b + np.dot(self.w, X[jj])
                if Y[jj] * a <= 0:
                    w_updated = True
                    self.w += Y[jj] * X[jj]
                    self.b += Y[jj]
            if not w_updated:
                print("Convergence reached in %i iterations." % ii)
                break
        if w_updated:
            print(
            """
            WARNING: convergence not reached in %i iterations.
            Either dataset is not linearly separable, 
            or max_iter should be increased
            """ % self.max_iter
                )
            
    def classify_element(self, x_elem):
        return np.sign(self.b + np.dot(self.w, x_elem))
    
    def classify(self, X):
        out = np.dot(X, self.w)
        predicted_Y = np.sign(out + self.b)
        return predicted_Y
    
p = Perceptron()
p.train(features_vectorized, labels)
predicted_Y = p.classify(features_vectorized)
acc_tr = accuracy_score(predicted_Y, labels)
print(acc_tr)

# def generate_data(no_points):
#     X = np.zeros(shape=(no_points, 2))
#     Y = np.zeros(shape=no_points)
#     for ii in range(no_points):
#         X[ii, 0] = random.randint(0,20)
#         X[ii, 1] = random.randint(0,20)
#         if X[ii, 0]+X[ii, 1] > 20:
#             Y[ii] = 1 
#         else:
#             Y[ii] = -1
#     return X, Y
# 
# X, Y = generate_data(100)
# 
# # TO Do: Insert your code to find the indices for positive examples
# idx_pos = [i for i in np.arange(100) if Y[i]==1]
# # TO Do: Insert your code to find the indices for negative examples
# idx_neg = [i for i in np.arange(100) if Y[i]==-1]
# # make a scatter plot
# plt.scatter(X[idx_pos, 0], X[idx_pos, 1], color='blue')
# plt.scatter(X[idx_neg, 0], X[idx_neg, 1], color='red')
# plt.show()
# 
# # Create an instance p
# p = Perceptron()
# # applies the train algorithm to (X,Y) and sets the weight vector and bias
# p.train(X, Y)
# # To Do: Insert your code to get the predicted output on the training set
# predicted_Y = p.classify(X)
# # TO Do: Insert your code to get the accuracy on training set
# acc_tr = accuracy_score(predicted_Y, Y)
# print(acc_tr)
# 
# # we first generate a new dataset
# X_test, Y_test = generate_data(100)
# # To Do: Insert your code to get the predicted output on the test set
# predicted_Y_test = p.classify(X_test)
# # TO Do: Insert your code to get the accuracy on the test set
# acc = accuracy_score(Y_test, predicted_Y_test)
# print(acc)
# 
# x1 = np.arange(0, 20, 0.1)
# # bias
# b = p.b
# # weight vector
# w = p.w
# # we now use list comprehension to generate the array of the second feature
# # To do: generate the second features for the hyperplane, i.e., (X1[i], X2[i]) is an point in the hyperplane
# x2 = [(-b-w[0]*x)/w[1] for x in x1]
# plt.scatter(X[idx_pos, 0], X[idx_pos, 1], color='blue')
# plt.scatter(X[idx_neg, 0], X[idx_neg, 1], color='red')
# # plot the hyperplane corresponding to the perceptron
# plt.plot(x1, x2, color="black", linewidth=2.5, linestyle="-")
# plt.show()