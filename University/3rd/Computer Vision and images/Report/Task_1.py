# Imports
import skimage
from skimage import filters
import scipy
from matplotlib import pyplot as plt
import numpy as np
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

def magnitude(x,y):
    return np.sqrt(np.square(x) + np.square(y))

### Laplacian of Guassian ###

def generate_log_mask(std_dev, size):
    """
    Generate a Laplacian of Gaussian (LoG) filter mask.
    
    Parameters:
    - std_dev: Standard deviation of the Gaussian component of the filter. This controls the amount of smoothing.
              The larger the std_dev, the more blurred the image will be before edge detection.
    - size: The size of the filter mask. It determines the dimensions of the output filter mask (size x size).
            It's recommended to be an odd number to have a central pixel.
    
    Returns:
    - log_filter: The calculated LoG filter mask.
    """
    # Define the grid for the filter mask.
    n = size // 2
    y, x = np.ogrid[-n:n+1, -n:n+1]
    
    # Calculate the LoG filter using its mathematical formula.
    # First, calculate the squared distance from the center, divided by twice the variance (std_dev squared).
    factor1 =  (x**2 + y**2) / (2 * std_dev**2)
    # Apply the exponential part of the Gaussian function.
    factor2 = np.exp(- factor1)
    # Combine all parts to form the LoG filter. Multiplying by -1 ensures the laplacian property of having a zero-crossing.
    log_filter = - 1 / (np.pi * std_dev**4) * (1 - factor1) * factor2
    
    # Normalize the filter to have zero sum. This step ensures that the filter conserves image brightness.
    log_filter -= log_filter.mean()
    
    return log_filter

def apply_log_filter(image, std_dev=1, size=9):
    """
    Apply the Laplacian of Gaussian (LoG) filter to an image.
    
    Parameters:
    - image: The input image to apply the LoG filter on.
    - std_dev: Standard deviation for the Gaussian part of the LoG filter.
    - size: The size of the LoG filter mask.
    
    Returns:
    - A binary image where the pixels below the threshold (0.09 in this case) are considered edges.
    """
    # Generate the LoG mask with the given standard deviation and size.
    log_mask = generate_log_mask(std_dev, size)
    # Convolve the input image with the LoG mask to detect edges.
    log_image = scipy.signal.convolve2d(image, log_mask, mode='same')

    return log_image

shakey = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/shakey.jpg"))

shakey_smoothed = filters.gaussian(shakey, sigma=1)

show_binary_image(shakey)

show_binary_image(shakey_smoothed)

show_binary_image(apply_log_filter(shakey_smoothed))