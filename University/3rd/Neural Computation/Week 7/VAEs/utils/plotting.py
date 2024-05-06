# Copyright 2019, Imperial College London
# 
# Tutorial for CO416 - Machine Learning for Imaging
#
# This file: Functions to plot 2D images.

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from IPython import display
plt.rcParams['axes.labelsize'] = 14
plt.rcParams['xtick.labelsize'] = 12
plt.rcParams['ytick.labelsize'] = 12


def plot_image(image, interpol="nearest", cmap="gray", vminmax=[None,None], figsize=None, dynamically=False):
    # image: np.array of one of the following shapes:
    #       grayscale image:    (height, width)
    #       grayscale image:    (height, width, 1)
    #       rgb image:          (height, width, 3)
    
    #print("Plotting image of shape: ", image.shape)
    plt.figure(figsize=figsize) # (figsize=(n_imgs_per_row*0.5, n_rows*0.5)) # size (width, height), in inches.
    if len(image.shape) == 2:
        fig = plt.imshow(image, cmap=cmap, interpolation=interpol, vmin=vminmax[0], vmax=vminmax[1]) # imshow: (w,h) or (w,h,3)
        plt.colorbar(fig)
    elif len(image.shape) == 3 and image.shape[2] == 1:
        fig = plt.imshow(image[:,:,0], cmap=cmap, interpolation=interpol, vmin=v_minmax[0], vmax=vminmax[1]) # imshow: (w,h) or (w,h,3)
        plt.colorbar(fig)
    elif len(image.shape) == 3 and image.shape[2] == 3 :
        fig = plt.imshow(image, interpolation=interpol)
    else:
        raise Error("Wrong shape of given image for plotting.")
    
    if dynamically:
        #display.clear_output(wait=True)
        display.display(plt.gcf())
    else:
        plt.show()
    
    
def plot_images(images, titles=None, interpol="nearest", cmap="gray", vminmax=[None,None], figsize=None, dynamically=False):
    # images: list of images. Each image...
    # image: np.array of one of the following shapes:
    #       grayscale image:    (height, width)
    #       grayscale image:    (height, width, 1)
    #       rgb image:          (height, width, 3)
    
    # Check that all images have same number of dimensions:
    n_dims = len(images[0].shape)
    for image in images:
        assert len(image.shape) == n_dims
    
    n_images = len(images)
    fig, axes = plt.subplots(1, n_images, sharex=False, sharey=False)
    if titles is not None:
        for i in range(len(titles)):
            axes[i].set_title(titles[i])
    
    for i in range(len(images)):
        image = images[i]
        if n_dims == 2:
            # imshow: (w,h) or (w,h,3)
            axes[i].imshow(image, cmap=cmap, interpolation=interpol, vmin=vminmax[0], vmax=vminmax[1])
            #plt.colorbar(fig)
        elif n_dims == 3 and image.shape[2] == 1:
            axes[i].imshow(image[:,:,0], cmap=cmap, interpolation=interpol, vmin=v_minmax[0], vmax=vminmax[1])
            #plt.colorbar(fig)
        elif n_dims == 3 and image.shape[2] == 3 :
            axes[i].imshow(image, interpolation=interpol)
        else:
            raise Error("Wrong shape of given image for plotting.")
     
    if dynamically:
        #display.clear_output(wait=True)
        display.display(plt.gcf())
    else:
        plt.show()
        
        
def plot_grid_of_images(imgs, n_imgs_per_row=10, interpol="nearest", cmap="gray", vminmax=[None,None], dynamically=False):
    # imgs: numpy array of one of the following shapes:
    #       grayscales images:  (number-of-images, height, width)
    #       grayscales images:  (number-of-images, height, width, 1)
    #       color images:       (number-of-images, height, width, 3)
    n_rows = imgs.shape[0]//n_imgs_per_row + 1*int(imgs.shape[0]%n_imgs_per_row > 0)
    print("n_rows=",n_rows)
    # Append empty images if the last row is not complete
    n_empty_imgs = n_rows * n_imgs_per_row - imgs.shape[0]
    imgs_to_plot = np.concatenate( [imgs, np.zeros((n_empty_imgs, imgs.shape[1], imgs.shape[2]))], axis=0)
    
    # draw row by row
    row_images = [] # each element will be (image-height, image-width X n_imgs_per_row)
    for current_row in range(n_rows):
        tmp_row_images = imgs_to_plot[current_row * n_imgs_per_row : (current_row + 1) * n_imgs_per_row]
        row_images.append( np.concatenate(tmp_row_images, axis=1) )
    # draw all row-images in one image
    collage_of_images = np.concatenate(row_images, axis=0) # array.shape: (height X n_imgs_per_row, width X n_imgs_per_row)
    
    plot_image(collage_of_images, interpol=interpol, cmap=cmap, vminmax=[None,None], dynamically=dynamically)

    
def plot_grids_of_images(list_of_img_arrays,
                         titles=None,
                         n_imgs_per_row=10,
                         interpol="nearest",
                         cmap="gray",
                         vminmax=[None,None],
                         dynamically=False):
    # list_of_img_arrays: A list. Each element of the list is a 'imgs' numpy array (see below)
    # imgs: numpy array of one of the following shapes:
    #       grayscales images:  (number-of-images, height, width)
    #       grayscales images:  (number-of-images, height, width, 1)
    #       color images:       (number-of-images, height, width, 3)
    n_imgs_per_array = list_of_img_arrays[0].shape[0]
    assert all([imgs.shape[0] == n_imgs_per_array for imgs in list_of_img_arrays])
    
    imgs1 = list_of_img_arrays[0]
    n_rows = imgs1.shape[0]//n_imgs_per_row + 1*int(imgs1.shape[0]%n_imgs_per_row > 0)
    # Append empty images if the last row is not complete
    n_empty_imgs = n_rows * n_imgs_per_row - imgs1.shape[0]
    
    # Make the 2 collages
    collages_of_imgs = []
    for imgs in list_of_img_arrays:
        imgs_to_plot = np.concatenate( [imgs, np.zeros((n_empty_imgs, imgs.shape[1], imgs.shape[2]))], axis=0)

        # draw row by row
        row_images = [] # each element will be (image-height, image-width X n_imgs_per_row)
        for current_row in range(n_rows):
            tmp_row_images = imgs_to_plot[current_row * n_imgs_per_row : (current_row + 1) * n_imgs_per_row]
            row_images.append( np.concatenate(tmp_row_images, axis=1) )
        # draw all row-images in one image
        collage_of_imgs = np.concatenate(row_images, axis=0) # array.shape: (height X n_imgs_per_row, width X n_imgs_per_row)
        collages_of_imgs.append(collage_of_imgs)
        
    plot_images(collages_of_imgs, titles, interpol=interpol, cmap=cmap, vminmax=[None,None], dynamically=dynamically)
        
        
def plot_train_progress_1(loss_l, iters_per_point, total_iters=None, y_lim=None):

    fig, axes = plt.subplots(1, 1, sharex=False, sharey=False)
    x_points = range(0, len(loss_l)*iters_per_point, iters_per_point)
    
    axes.plot(x_points, loss_l, color="black", label="Training loss", linewidth=5)
    axes.set_title("Training loss", fontsize=10, y=1.022)
    axes.yaxis.grid(True, zorder=0)
    axes.set_xlabel('Iteration', fontsize=10)
    if total_iters is not None:
        axes.set_xlim([0,total_iters])
    axes.set_ylim([0,y_lim])
    axes.legend(loc='upper right')
    plt.show()
    
    
def plot_train_progress_2(loss_l, acc_train_l, acc_test_l, iters_per_point, total_iters=None):

    fig, axes = plt.subplots(1, 2, sharex=False, sharey=False)
    assert len(loss_l) == len(acc_train_l) == len(acc_test_l)
    x_points = range(0, len(loss_l)*iters_per_point, iters_per_point)
    
    axes[0].plot(x_points, loss_l, color="black", label="Training loss", linewidth=5)
    axes[0].set_title("Training loss", fontsize=10, y=1.022)
    axes[0].yaxis.grid(True, zorder=0)
    axes[0].set_xlabel('Iteration', fontsize=10)
    if total_iters is not None:
        axes[0].set_xlim([0,total_iters])
    axes[0].set_ylim([0,None])
    axes[0].legend(loc='upper right')
    
    axes[1].set_title("Accuracy", fontsize=10, y=1.022)
    axes[1].plot(x_points, acc_train_l, color="blue", label="Train", linewidth=5)
    axes[1].plot(x_points, acc_test_l, color="red", label="Test", linewidth=5)
    axes[1].yaxis.grid(True, zorder=0)
    axes[1].set_xlabel('Iteration', fontsize=10)
    if total_iters is not None:
        axes[1].set_xlim([0,total_iters])
    axes[1].set_ylim([0,100])
    axes[1].legend(loc='lower right')
    
    plt.show()
    
    
def plot_train_progress_VAE(loss_total, loss_rec, loss_reg, iters_per_point, total_iters=None, y_lims=[None, None, None]):

    fig, axes = plt.subplots(1, 3, sharex=False, sharey=False)
    assert len(loss_total) == len(loss_rec) == len(loss_reg)
    x_points = range(0, len(loss_total)*iters_per_point, iters_per_point)
    
    axes[0].set_title("Total loss", fontsize=10, y=1.022)
    axes[0].plot(x_points, loss_total, color="black", label="Total", linewidth=5)
    axes[0].yaxis.grid(True, zorder=0)
    axes[0].set_xlabel('Iteration', fontsize=10)
    if total_iters is not None:
        axes[0].set_xlim([0,total_iters])
    axes[0].set_ylim([0,y_lims[0]])
    axes[0].legend(loc='upper right')
    
    axes[1].set_title("Reconstruction", fontsize=10, y=1.022)
    axes[1].plot(x_points, loss_rec, color="blue", label="Recon", linewidth=5)
    axes[1].yaxis.grid(True, zorder=0)
    axes[1].set_xlabel('Iteration', fontsize=10)
    if total_iters is not None:
        axes[1].set_xlim([0,total_iters])
    axes[1].set_ylim([0,y_lims[1]])
    axes[1].legend(loc='lower right')
    
    axes[2].set_title("Regularizer", fontsize=10, y=1.022)
    axes[2].plot(x_points, loss_reg, color="blue", label="Regul", linewidth=5)
    axes[2].yaxis.grid(True, zorder=0)
    axes[2].set_xlabel('Iteration', fontsize=10)
    if total_iters is not None:
        axes[2].set_xlim([0,total_iters])
    axes[2].set_ylim([0,y_lims[2]])
    axes[2].legend(loc='lower right')
    
    plt.show()

    
    