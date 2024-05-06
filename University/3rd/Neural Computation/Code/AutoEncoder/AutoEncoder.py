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

from utils.plotting import plot_grid_of_images
import matplotlib.pyplot as plt
plot_grid_of_images(train_imgs[0:100], n_imgs_per_row=10)
plt.show()

# Data pre-processing

train_lbls_onehot = np.zeros(shape=( train_lbls.shape[0] , C_classes ))
train_lbls_onehot[  np.arange(train_lbls_onehot.shape[0]), train_lbls] = 1
test_lbls_onehot =  np.zeros(shape=(  test_lbls.shape[0] , C_classes ))
test_lbls_onehot[   np.arange( test_lbls_onehot.shape[0]), test_lbls ] = 1
print("BEFORE: [train_lbls]        Type: ", type(train_lbls), "|| Shape:", train_lbls.shape, " || Data type: ", train_lbls.dtype )
print("AFTER : [train_lbls_onehot] Type: ", type(train_lbls_onehot), "|| Shape:", train_lbls_onehot.shape, " || Data type: ", train_lbls_onehot.dtype )

from utils.data_utils import normalize_int_whole_database
train_imgs = normalize_int_whole_database(train_imgs, norm_type="minus_1_to_1")
test_imgs = normalize_int_whole_database(test_imgs, norm_type="minus_1_to_1")

from utils.plotting import plot_image
index = 0
print("Plotting image of index: [", index, "]")
print("Class label for this image is: ", train_lbls[index])
print("One-hot label representation: [", train_lbls_onehot[index], "]")
plot_image(train_imgs[index])
plt.show()

train_imgs_flat = train_imgs.reshape([train_imgs.shape[0], -1])
test_imgs_flat = test_imgs.reshape([test_imgs.shape[0], -1])
print("Shape of numpy array holding the training database:")
print("Original : [N, H, W] = [", train_imgs.shape , "]")
print("Flattened: [N, H*W]  = [", train_imgs_flat.shape , "]")

# Unsupervised training with SGD for Auto-Encoders

from utils.plotting import plot_train_progress_1, plot_grids_of_images

def get_random_batch(train_imgs, train_lbls, batch_size, rng):
    indices = range(0, batch_size)
    indices = rng.randint(low = 0, high = train_imgs.shape[0], size = batch_size, dtype = 'int32')
    train_imgs_batch = train_imgs[indices]
    if train_lbls is not None:
        train_lbls_batch = train_lbls[indices]
    else:
        train_imgs_batch = None
    return [train_imgs_batch, train_lbls_batch]

def unsupervised_training_AE(net,
                             loss_func,
                             rng,
                             train_imgs_all,
                             batch_size,
                             learning_rate,
                             total_iters,
                             iters_per_recon_plot = -1):
    loss_values_to_plot = []
    optimizer = optim.Adam(net.params, lr=learning_rate)

    for t in range(total_iters):
        x_imgs, _ = get_random_batch(train_imgs_all, None, batch_size, rng)
        x_pred, z_codes = net.forward_pass(x_imgs)
        loss = loss_func(x_pred,x_imgs)
        optimizer.zero_grad()
        _ = net.backward_pass(loss)
        optimizer.step()

        loss_np = loss if type(loss) is type(float) else loss.item()
        print("[iter:", t, "]: Training Loss: {0:.2f}".format(loss))
        loss_values_to_plot.append(loss_np)

        if t==total_iters-1 or t%iters_per_recon_plot == 0:
            x_pred_all, z_codes_all = net.forward_pass(train_imgs_all)
            x_pred_all_np = x_pred_all if type(x_pred_all) is np.ndarray else x_pred_all.detach().numpy()

            train_imgs_resh = train_imgs_all.reshape([train_imgs_all.shape[0], H_height, W_width])
            x_pred_all_np_resh = x_pred_all_np.reshape([train_imgs_all.shape[0], H_height, W_width])

            plot_grids_of_images([train_imgs_resh[0:100], x_pred_all_np_resh[0:100]],
                                 titles=["Real", "Reconstructions"],
                                 n_imgs_per_row=10,
                                 dynamically=True)
            plt.show()
        
    plot_train_progress_1(loss_values_to_plot, iters_per_point=1)

import torch
import torch.optim as optim
import torch.nn as nn

class Network():
    def backward_pass(self, loss):
        loss.backward()
        grads = [param.grad for param in self.params]

class Autoencoder(Network):
    def __init__(self, rng, D_in, D_hid_enc, D_bottleneck, D_hid_dec):
        D_in = D_in
        D_hid_1 = D_hid_enc
        D_hid_2 = D_bottleneck
        D_hid_3 = D_hid_dec
        D_out = D_in

        self.D_bottleneck = D_bottleneck

        w1_init = rng.normal(loc = 0.0, scale = 0.01, size = (D_in + 1, D_hid_1))
        w2_init = rng.normal(loc = 0.0, scale = 0.01, size = (D_hid_1 + 1, D_hid_2))
        w3_init = rng.normal(loc=0.0, scale=0.01, size=(D_hid_2+1, D_hid_3))
        w4_init = rng.normal(loc=0.0, scale=0.01, size=(D_hid_3+1, D_out))

        w1 = torch.tensor(w1_init, dtype=torch.float, requires_grad=True)
        w2 = torch.tensor(w2_init, dtype=torch.float, requires_grad=True)
        w3 = torch.tensor(w3_init, dtype=torch.float, requires_grad=True)
        w4 = torch.tensor(w4_init, dtype=torch.float, requires_grad=True)

        self.params = [w1, w2, w3, w4]

    def forward_pass(self, batch_imgs):
        [w1, w2, w3, w4] = self.params

        batch_imgs_t = torch.tensor(batch_imgs, dtype=torch.float)

        unary_feature_for_bias = torch.ones(size=(batch_imgs.shape[0], 1))
        x = torch.cat((batch_imgs_t, unary_feature_for_bias), dim=1)

        h1_preact = x.mm(w1)
        h1_act = h1_preact.clamp(min=0)

        h1_ext = torch.cat((h1_act, unary_feature_for_bias), dim=1)
        h2_preact = h1_ext.mm(w2)
        h2_act = h2_preact.clamp(min=0)

        h2_ext = torch.cat((h2_act, unary_feature_for_bias), dim=1)
        h3_preact = h2_ext.mm(w3)
        h3_act = h3_preact.clamp(min=0)

        h3_ext = torch.cat((h3_act, unary_feature_for_bias), dim=1)
        h4_preact = h3_ext.mm(w4)
        h4_act = torch.tanh(h4_preact)

        x_pred = h4_act

        acts_bottleneck = h2_act

        return (x_pred, acts_bottleneck)
    
def reconstruction_loss(x_pred, x_real, eps=1e-7):
    x_pred = torch.tensor(x_pred, dtype=torch.float) if type(x_pred) is np.ndarray else x_pred
    x_real = torch.tensor(x_real, dtype=torch.float) if type(x_real) is np.ndarray else x_real
    loss_recon = torch.mean(torch.square(x_pred - x_real), dim=1)
    cost = torch.mean(loss_recon, dim=0) # Expectation of loss: Mean over samples (axis=0).
    return cost

rng = np.random.RandomState(seed=SEED)
autoencoder_thin = Autoencoder(rng=rng,
                               D_in=H_height*W_width,
                               D_hid_enc=256,
                               D_bottleneck=2,
                               D_hid_dec=256)
unsupervised_training_AE(autoencoder_thin,
                         reconstruction_loss,
                         rng,
                         train_imgs_flat,
                         batch_size=40,
                         learning_rate=3e-3,
                         total_iters=1000,
                         iters_per_recon_plot=50)
    
import matplotlib.pyplot as plt

def encode_and_get_min_max_z(net,
                             imgs_flat,
                             lbls,
                             batch_size,
                             total_iterations=None,
                             plot_2d_embedding=True):
    if total_iterations is None:
        total_iterations = (train_imgs_flat.shape[0] - 1) // batch_size + 1
    
    z_codes_all = []
    lbls_all = []
    for t in range(total_iterations):
        x_batch = imgs_flat[t*batch_size: (t+1)*batch_size]
        lbls_batch = lbls[t*batch_size: (t+1)*batch_size]
        
        x_pred, z_codes = net.forward_pass(x_batch)

        z_codes_np = z_codes if type(z_codes) is np.ndarray else z_codes.detach().numpy()
        
        z_codes_all.append(z_codes_np)
        lbls_all.append(lbls_batch)
    
    z_codes_all = np.concatenate(z_codes_all) 
    lbls_all = np.concatenate(lbls_all)
    
    if plot_2d_embedding:
        plt.scatter(z_codes_all[:,0], z_codes_all[:,1], c=lbls_all, alpha=0.5)
        plt.show()
    
    min_z = np.min(z_codes_all, axis=0)
    max_z = np.max(z_codes_all, axis=0)
    
    return min_z, max_z

min_z, max_z = encode_and_get_min_max_z(autoencoder_thin,
                                        train_imgs_flat,
                                        train_lbls,
                                        batch_size=100,
                                        total_iterations=100)
print("Min Z value per dimension of bottleneck:", min_z)
print("Max Z value per dimension of bottleneck:", max_z)