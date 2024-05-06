import os

# 更改当前工作目录
os.chdir('C:/_Study_Resource/Study_Note/University/3rd/Neural Computation/Code')
# print(os.getcwd())

import numpy as np
from utils.data_utils import get_mnist # Helper function. Use it out of the box.

# Constants
DATA_DIR = './data/mnist' # Location we will keep the data.
SEED = 111111

# If datasets are not at specified location, they will be downloaded.
train_imgs, train_lbls = get_mnist(data_dir=DATA_DIR, train=True, download=True)
test_imgs, test_lbls = get_mnist(data_dir=DATA_DIR, train=False, download=True)

print("[train_imgs] Type: ", type(train_imgs), "|| Shape:", train_imgs.shape, "|| Data type: ", train_imgs.dtype )
print("[train_lbls] Type: ", type(train_lbls), "|| Shape:", train_lbls.shape, "|| Data type: ", train_lbls.dtype )
print('Class labels in train = ', np.unique(train_lbls))

print("[test_imgs] Type: ", type(test_imgs), "|| Shape:", test_imgs.shape, " || Data type: ", test_imgs.dtype )
print("[test_lbls] Type: ", type(test_lbls), "|| Shape:", test_lbls.shape, " || Data type: ", test_lbls.dtype )
print('Class labels in test = ', np.unique(test_lbls))

N_tr_imgs = train_imgs.shape[0] # N hereafter. Number of training images in database.
H_height = train_imgs.shape[1] # H hereafter
W_width = train_imgs.shape[2] # W hereafter
C_classes = len(np.unique(train_lbls)) # C hereafter

# a) Change representation of labels to one-hot vectors of length C=10.
train_lbls_onehot = np.zeros(shape=(train_lbls.shape[0], C_classes ) )
train_lbls_onehot[ np.arange(train_lbls_onehot.shape[0]), train_lbls ] = 1
test_lbls_onehot = np.zeros(shape=(test_lbls.shape[0], C_classes ) )
test_lbls_onehot[ np.arange(test_lbls_onehot.shape[0]), test_lbls ] = 1
print("BEFORE: [train_lbls]        Type: ", type(train_lbls), "|| Shape:", train_lbls.shape, " || Data type: ", train_lbls.dtype )
print("AFTER : [train_lbls_onehot] Type: ", type(train_lbls_onehot), "|| Shape:", train_lbls_onehot.shape, " || Data type: ", train_lbls_onehot.dtype )

# b) Re-scale image intensities, from [0,255] to [-1, +1].
# This commonly facilitates learning:
# A zero-centered signal with small magnitude allows avoiding exploding/vanishing problems easier.
from utils.data_utils import normalize_int_whole_database # Helper function. Use out of the box.
train_imgs = normalize_int_whole_database(train_imgs, norm_type="minus_1_to_1")
test_imgs = normalize_int_whole_database(test_imgs, norm_type="minus_1_to_1")

# Lets plot one image.
from utils.plotting import plot_image, plot_images # Helper function, use out of the box.
index = 0  # Try any, up to 60000
print("Plotting image of index: [", index, "]")
print("Class label for this image is: ", train_lbls[index])
print("One-hot label representation: [", train_lbls_onehot[index], "]")
plot_image(train_imgs[index])
# Notice the magnitude of intensities. Black is now negative and white is positive float.
# Compare with intensities of figure further above.

# c) Flatten the images, from 2D matrices to 1D vectors. MLPs take feature-vectors as input, not 2D images.
train_imgs_flat = train_imgs.reshape([train_imgs.shape[0], -1]) # Preserve 1st dim (S = num Samples), flatten others.
test_imgs_flat = test_imgs.reshape([test_imgs.shape[0], -1])
print("Shape of numpy array holding the training database:")
print("Original : [N, H, W] = [", train_imgs.shape , "]")
print("Flattened: [N, H*W]  = [", train_imgs_flat.shape , "]")

import torch
import torch.optim as optim
import torch.nn as nn

class Network():
    def backward_pass(self, loss):
        loss.backward()
        grads = [param.grad for param in self.params]
        return grads
    
class VAE(Network):
    def __init__(self, rng, D_in, D_hid_enc, D_bottleneck, D_hid_dec):
        D_in = D_in
        D_hid_1 = D_hid_enc
        D_hid_2 = D_bottleneck
        D_hid_3 = D_hid_dec
        D_out = D_in

        self.D_bottleneck = D_bottleneck

        w1_init = rng.normal(loc=0.0, scale = 0.01, size = (D_in+1, D_hid_1))
        w2_mu_init = rng.normal(loc=0.0, scale=0.01, size=(D_hid_1+1, D_hid_2))
        w2_std_init = rng.normal(loc=0.0, scale=0.01, size=(D_hid_1+1, D_hid_2))
        w3_init = rng.normal(loc=0.0, scale=0.01, size=(D_hid_2+1, D_hid_3))
        w4_init = rng.normal(loc=0.0, scale=0.01, size=(D_hid_3+1, D_out))

        w1 = torch.tensor(w1_init, dtype=torch.float, requires_grad=True)
        w2_mu = torch.tensor(w2_mu_init, dtype=torch.float, requires_grad=True)
        w2_std = torch.tensor(w2_std_init, dtype=torch.float, requires_grad=True)
        w3 = torch.tensor(w3_init, dtype=torch.float, requires_grad=True)
        w4 = torch.tensor(w4_init, dtype=torch.float, requires_grad=True)
        self.params = [w1, w2_mu, w2_std, w3, w4]

    def encoder(self, batch_imgs):
        [w1, w2_mu, w2_std, w3, w4] = self.params

        batch_imgs_t = torch.tensor(batch_imgs, dtype=torch.float) if type(batch_imgs) is np.ndarray else batch_imgs

        unary_feature_for_bias = torch.ones(size = (batch_imgs_t.shape[0], 1))
        x = torch.cat((batch_imgs_t, unary_feature_for_bias), dim = 1)

        h1_preact = x.mm(w1)
        h1_act = h1_preact.clamp(min = 0)
        h1_ext = torch.cat((h1_act, unary_feature_for_bias), dim=1)

        h2_mu_preact = h1_ext.mm(w2_mu)
        h2_mu_act = h2_mu_preact
        h2_logstd_preact = h1_ext.mm(w2_std)
        h2_logstd_act = h2_logstd_preact

        z_coding = (h2_mu_act, h2_logstd_act)
        return z_coding
    
    def decode(self, z_codes):
        [w1, w2_mu, w2_std, w3, w4] = self.params

        z_codes_t = torch.tensor(z_codes, dtype=torch.float) if type(z_codes) is np.ndarray else z_codes

        unary_feature_for_bias = torch.ones(size=(z_codes_t.shape[0], 1))

        h2_ext = torch.cat((z_codes_t, unary_feature_for_bias), dim=1)
        h3_preact = h2_ext.mm(w3)
        h3_act = h3_preact.clamp(min=0)
        h3_ext = torch.cat((h3_act, unary_feature_for_bias), dim=1)
        h4_preact = h3_ext.mm(w4)
        h4_act = torch.tanh(h4_preact)

        x_pred = h4_act

        return x_pred
    
    def sample_with_reparameterization(self, z_mu, z_logstd):
        N_samples = z_mu.shape[0]
        Z_dims = z_mu.shape[1]

        z_std = torch.exp(z_logstd)
        eps = torch.randn(size=[N_samples,Z_dims])
        z_samples = z_mu + z_std * eps

        return z_samples
    
    def forward_pass(self,batch_imgs):
        batch_imgs_t = torch.tensor(batch_imgs, dtype=torch.float) if type(batch_imgs) is np.ndarray else batch_imgs

        z_mu, z_logstd = self.encoder(batch_imgs)
        z_samples = self.sample_with_reparameterization(z_mu, z_logstd)
        x_pred = self.decode(z_samples)

        return (x_pred, z_mu, z_logstd, z_samples)
    
def reconstruction_loss(x_pred, x_real, eps=1e-7):
    x_pred = torch.tensor(x_pred, dtype=torch.float) if type(x_pred) is np.ndarray else x_pred
    x_real = torch.tensor(x_real, dtype=torch.float) if type(x_real) is np.ndarray else x_real

    loss_recon = torch.mean(torch.square(x_pred - x_real), dim=1)

    cost = torch.mean(loss_recon, dim=0)

    return cost
    
def regularizer_loss(mu, log_std):
    std = torch.exp(log_std)
    reg_loss_per_sample = 0.5 * torch.sum(mu**2 + std**2 - 2 * log_std - 1, dim = 1)
    reg_loss = torch.mean(reg_loss_per_sample, dim = 0)

    return reg_loss
    
def vae_loss(x_real, x_pred, z_mu, z_logstd, lambda_rec = 1, lambda_reg=0.005, eps = 1e-7):
    rec_loss = reconstruction_loss(x_pred, x_real, eps=1e-7)
    reg_loss = regularizer_loss(z_mu, z_logstd)

    weighted_rec_loss = lambda_rec * rec_loss
    weighted_reg_loss = lambda_reg * reg_loss

    total_loss = weighted_rec_loss + weighted_reg_loss

    return total_loss, weighted_rec_loss, weighted_reg_loss
        
from utils.plotting import plot_train_progress_VAE, plot_grids_of_images

def get_random_batch(train_imgs, train_lbls, batch_size, rng):
    indices = range(0,batch_size)
    indices = rng.randint(low = 0, high = train_imgs.shape[0], size = batch_size, dtype = 'int32')

    train_imgs_batch = train_imgs[indices]
    if train_lbls is not None:
        train_lbls_batch = train_lbls[indices]
    else:
        train_lbls_batch = None
    return [train_imgs_batch, train_lbls_batch]

def unsupervised_training_VAE(net,
                             loss_func,
                             lambda_rec,
                             lambda_reg,
                             rng,
                             train_imgs_all,
                             batch_size,
                             learning_rate,
                             total_iters,
                             iters_per_recon_plot=-1):
    loss_total_to_plot = []
    loss_rec_to_plot = []
    loss_reg_to_plot = []

    optimizer = optim.Adam(net.params, lr=learning_rate)

    for t in range(total_iters):
        x_batch, _ = get_random_batch(train_imgs_all, None, batch_size, rng)

        x_pred, z_mu, z_logstd, z_codes = net.forward_pass(x_batch)

        total_loss, rec_loss, reg_loss = loss_func(x_batch, x_pred, z_mu, z_logstd, lambda_rec, lambda_reg)

        optimizer.zero_grad()
        _ = net.backward_pass(total_loss)
        optimizer.step()

        total_loss_np = total_loss if type(total_loss) is type(float) else total_loss.item()  # Pytorch returns tensor. Cast to float
        rec_loss_np = rec_loss if type(rec_loss) is type(float) else rec_loss.item()
        reg_loss_np = reg_loss if type(reg_loss) is type(float) else reg_loss.item()
        if t%10==0:  # Print every 10 iterations
            print("[iter:", t, "]: Total training Loss: {0:.2f}".format(total_loss_np))
        loss_total_to_plot.append(total_loss_np)
        loss_rec_to_plot.append(rec_loss_np)
        loss_reg_to_plot.append(reg_loss_np)

        if t==total_iters-1 or t%iters_per_recon_plot == 0:
            # Reconstruct all images, to plot reconstructions.
            x_pred_all, z_mu_all, z_logstd_all, z_codes_all = net.forward_pass(train_imgs_all)
            # Cast tensors to numpy arrays
            x_pred_all_np = x_pred_all if type(x_pred_all) is np.ndarray else x_pred_all.detach().numpy()
            
            # Predicted reconstructions have vector shape. Reshape them to original image shape.
            train_imgs_resh = train_imgs_all.reshape([train_imgs_all.shape[0], H_height, W_width])
            x_pred_all_np_resh = x_pred_all_np.reshape([train_imgs_all.shape[0], H_height, W_width])
            
            # Plot a few images, originals and predicted reconstructions.
            plot_grids_of_images([train_imgs_resh[0:100], x_pred_all_np_resh[0:100]],
                                  titles=["Real", "Reconstructions"],
                                  n_imgs_per_row=10,
                                  dynamically=True)

    plot_train_progress_VAE(loss_total_to_plot, loss_rec_to_plot, loss_reg_to_plot, iters_per_point=1, y_lims=[1., 1., None])

rng = np.random.RandomState(seed = SEED)
vae = VAE(rng=rng,
          D_in=H_height*W_width,
          D_hid_enc=256,
          D_bottleneck=2,
          D_hid_dec=256)

unsupervised_training_VAE(vae,
                          vae_loss,
                          lambda_rec=1.0,
                          lambda_reg=0.005,
                          rng=rng,
                          train_imgs_all=train_imgs_flat,
                          batch_size=40,
                          learning_rate=3e-3,
                          total_iters=1000,
                          iters_per_recon_plot=50)

import matplotlib.pyplot as plt

def encode_training_images(net,
                           imgs_flat,
                           lbls,
                           batch_size,
                           total_iterations=None,
                           plot_2d_embedding=True,
                           plot_hist_mu_std_for_dim=0):
    if total_iterations is None:
        total_iterations = (train_imgs_flat.shape[0] - 1) // batch_size + 1
    
    z_mu_all = []
    z_std_all = []
    lbls_all = []
    for t in range(total_iterations):
        x_batch = imgs_flat[t*batch_size: (t+1)*batch_size]
        lbls_batch = lbls[t*batch_size: (t+1)*batch_size]

        z_mu, z_logstd = net.encode(x_batch) 
        z_mu_np = z_mu if type(z_mu) is np.ndarray else z_mu.detach().numpy()
        z_logstd_np = z_logstd if type(z_logstd) is np.ndarray else z_logstd.detach().numpy()

        z_mu_all.append(z_mu_np)
        z_std_all.append(np.exp(z_logstd_np))
        lbls_all.append(lbls_batch)
    
    z_mu_all = np.concatenate(z_mu_all)
    z_std_all = np.concatenate(z_std_all)
    lbls_all = np.concatenate(lbls_all)

    if plot_2d_embedding:
        print("Z-Space and the MEAN of the predicted p(z|x) for each sample (std.devs not shown)")
        plt.scatter(z_mu_all[:,0], z_mu_all[:,1], c=lbls_all, alpha=0.5)
        plt.show()

    print("Histogram of values of the predicted MEANS")
    plt.hist(z_mu_all[:,plot_hist_mu_std_for_dim], bins=20)
    plt.show()
    print("Histogram of values of the predicted STANDARD DEVIATIONS")
    plt.hist(z_std_all[:,plot_hist_mu_std_for_dim], bins=20)
    plt.show()

encode_training_images(vae,
                       train_imgs_flat,
                       train_lbls,
                       batch_size=100,
                       total_iterations=200,
                       plot_2d_embedding=True,
                       plot_hist_mu_std_for_dim=0)

# Create the network
rng = np.random.RandomState(seed=SEED)
vae_2 = VAE(rng=rng,
            D_in=H_height*W_width,
            D_hid_enc=256,
            D_bottleneck=2,
            D_hid_dec=256)
# Start training
unsupervised_training_VAE(vae_2,
                          vae_loss,
                          lambda_rec=1.0,
                          lambda_reg=0.0,  # <------- No regularization loss. Just reconstruction.
                          rng=rng,
                          train_imgs_all=train_imgs_flat,
                          batch_size=40,
                          learning_rate=3e-3,
                          total_iters=1000,
                          iters_per_recon_plot=50)