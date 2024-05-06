# Imports
import skimage
from skimage.filters import threshold_otsu
from skimage import feature, img_as_float, io, color, filters, restoration, exposure
import scipy
from scipy import ndimage
import time
from matplotlib import pyplot as plt
import numpy as np
import math
from sklearn.metrics import roc_curve, auc
import scipy.signal

def show_binary_image(image, title=None):
    # Converts from one colour space to the other. this is needed as RGB
    # is not the default colour space for OpenCV

    # Show the image

    plt.imshow(image, cmap=plt.cm.gray)

    # remove the axis / ticks for a clean looking image
    plt.xticks([])
    plt.yticks([])

    # if a title is provided, show it
    if title is not None:
        plt.title(title)

    plt.show()

### Canny ###

def apply_canny_filter(image, std_dev = 1, size= 9, high_ratio=0.90, low_ratio=0.50):
    """
    Apply the Canny filter for edge detection.
    Parameters:
    - image: The input image to apply the filter on.
    - std_dev: The standard deviation of the Gaussian function.
    - size: The size of the Gaussian filter mask.
    - high_ratio: upper threshold
    - low_ratio: lower threshold
    Returns:
    - The edge image after applying the Canny filter.
    """
    n = size // 2
    y, x = np.ogrid[-n:n+1, -n:n+1]
    G = 1 / (2 * np.pi * std_dev**2) * np.exp(-(x**2 + y**2) / (2 * std_dev**2))
    Gx, Gy = (-x / std_dev**2) * G, (-y / std_dev**2) * G
    fx, fy = scipy.signal.convolve(image, Gx, mode='same'), scipy.signal.convolve(image, Gy, mode='same')
    magnitude, direction = np.abs(fx) + np.abs(fy), np.arctan2(fx, fy)
    
    canny_image = np.zeros_like(magnitude)
    
    angle_map = {0: (0, 1), 45: (1, 1), 90: (1, 0), 135: (1, -1)}
    for i in range(1, magnitude.shape[0]-1):
        for j in range(1, magnitude.shape[1]-1):
            angle = round(direction[i, j] / 45) * 45 % 180
            q, r = [magnitude[i + angle_map[angle][0]*k, j + angle_map[angle][1]*k] for k in (-1, 1)]
            canny_image[i, j] = magnitude[i, j] if magnitude[i, j] >= q and magnitude[i, j] >= r else 0
    
    high_threshold = np.percentile(magnitude, high_ratio * 100)
    low_threshold = high_threshold * low_ratio
    strong = magnitude >= high_threshold
    weak = (magnitude <= high_threshold) & (magnitude >= low_threshold)
    
    canny_image = np.where(strong, 0, np.where(weak, 128, 255))
    return canny_image

cells1      = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/9343 AM.bmp"))
cells2      = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/10905 JL.bmp"))
cells3      = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/43590 AM.bmp"))

cells_orginal = [cells1, cells2, cells3]

#cells = [filters.gaussian(cells1, sigma=1) > threshold_otsu(filters.gaussian(cells1, sigma=1)), filters.gaussian(cells2, sigma=1) > threshold_otsu(filters.gaussian(cells2, sigma=1)), filters.gaussian(cells3, sigma=1) > threshold_otsu(filters.gaussian(cells3, sigma=1))]

cells = [filters.gaussian(cells1, sigma=1), filters.gaussian(cells2, sigma=1), filters.gaussian(cells3, sigma=1)]

def show_canny_results(images):
    """
    Apply Canny edge detection to each image in the list and display the results side by side.
    
    Parameters:
    - images: List of images to apply Canny edge detection on.
    """
    num_images = len(images)
    
    plt.figure(figsize=(num_images * 5, 5))  # Adjust the figure size as needed
    
    for i, image in enumerate(images):
        # Apply Canny edge detection
        # Note: The sigma parameter controls the smoothness. Adjust it as needed for your images.
        edges = apply_canny_filter(image)
        
        plt.subplot(1, num_images, i + 1)  # Create a subplot for each image
        plt.imshow(edges, cmap='gray')  # Display the edges found by Canny
        plt.axis('off')  # Turn off axis ticks and labels
    
    plt.show()

show_canny_results(cells_orginal)