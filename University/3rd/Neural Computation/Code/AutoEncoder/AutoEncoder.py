import os

# 更改当前工作目录
os.chdir('C:/_Study_Resource/Study_Note/University/3rd/Neural Computation/Code')
# print(os.getcwd())

import numpy as np
from utils.data_utils import get_mnist

DATA_DIR = './data/mnist'
SEED = 111111

train_imgs, train_lbls = get_mnist(data_dir=DATA_DIR, train = True , download = True)
test_imgs , test_lbls = get_mnist(data_dir=DATA_DIR, train = False, download = True)

print("[train_imgs] Type: ", type(train_imgs), "|| Shape:", train_imgs.shape, "|| Data type: ", train_imgs.dtype)
print("[train_lbls] Type: ", type(train_lbls), "|| Shape:", train_lbls.shape, "|| Data type: ", train_lbls.dtype)
print('Class labels in train = ', np.unique(train_lbls))

print("[test_imgs] Type: ", type(test_imgs), "|| Shape:", test_imgs.shape, " || Data type: ", test_imgs.dtype)
print("[test_lbls] Type: ", type(test_lbls), "|| Shape:", test_lbls.shape, " || Date type: ", test_lbls.dtype)
print('Class labels in test = ', np.unique(test_lbls))

N_tr_imgs = train_imgs.shape[0]
H_height = train_imgs.shape[1]
W_width = train_imgs.shape[2]
C_classes = len(np.unique(train_lbls))