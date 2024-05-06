# Imports
import skimage
from skimage import filters
from skimage.filters import threshold_otsu
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
    """
    Calculate the magnitude of vectors from their x and y components.
    
    Parameters:
    - x: The x component of the vectors.
    - y: The y component of the vectors.
    
    Returns:
    - The magnitude of the vectors.
    """

    return np.sqrt(np.square(x) + np.square(y))

### Roberts ###

def apply_roberts_filter(image):
    """
    Apply the Roberts Cross operator for edge detection.
    
    Parameters:
    - image: The input image to detect edges on.
    
    Returns:
    - The edge magnitude image after applying the Roberts Cross operator.
    """

    # Define roberts operators
    roberts_x = np.array(
        [[1,0],
         [0,-1]])

    roberts_y = np.array(
        [[0,1],
         [-1,0]])

    # Convolve the image with Roberts kernels
    roberts_cross_x = scipy.signal.convolve2d(image, roberts_x, mode='same')
    roberts_cross_y = scipy.signal.convolve2d(image, roberts_y, mode='same')

    # Calculate the magnitude of the gradient
    roberts_image = magnitude(roberts_cross_x,roberts_cross_y)
    
    return roberts_image

### Sobel ###

def apply_sobels_filter(image):
    """
    Apply the Sobel operator for edge detection.
    
    Parameters:
    - image: The input image to detect edges on.
    
    Returns:
    - The edge magnitude image after applying the Sobel operator.
    """

    #Define sobel operators
    sobel_x = np.array(
        [[1,0,-1],
         [2,0,-2],
         [1,0,-1]])

    sobel_y = np.array(
        [[1,2,1],
         [0,0,0],
         [-1,-2,-1]])

    # Convolve the image with Sobel kernels
    sobel_cross_x = scipy.signal.convolve2d(image, sobel_x, mode='same')
    sobel_cross_y = scipy.signal.convolve2d(image, sobel_y, mode='same')

    # Calculate the magnitude of the gradient
    sobels_image = magnitude(sobel_cross_x,sobel_cross_y)

    return sobels_image

### First Order Gaussian ###

def first_order_gaussian(std_dev, mean, vec):
    """
    Calculate the first order Gaussian function.
    
    Parameters:
    - std_dev: The standard deviation of the Gaussian function.
    - mean: The mean of the Gaussian function.
    - vec: The vector of points to evaluate the Gaussian function at.
    
    Returns:
    - The first order Gaussian function evaluated at each point in vec.
    """
    zero_mean_gaussian = 1/np.sqrt(2 * np.pi * (std_dev ** 2)) * np.exp(- (vec - mean) ** 2 / (2 * std_dev ** 2))
    return - ((vec - mean) / std_dev ** 2) * zero_mean_gaussian

def generate_first_order_gaussian_mask(std_dev, mean, size):
    """
    Generate a first order Gaussian filter mask.
    
    Parameters:
    - std_dev: The standard deviation of the Gaussian function.
    - mean: The mean of the Gaussian function.
    - size: The size of the filter mask.
    
    Returns:
    - The first order Gaussian filter mask.
    """
    vec = np.arange(-size // 2, size // 2 + 1, 1, dtype=np.float32)
    one_d_first_order_gaussian = first_order_gaussian(std_dev, mean, vec)
    
    return one_d_first_order_gaussian

def apply_first_order_gaussian_filter(image, std_dev=1, mean = 0, size = 9):
    """
    Apply a first order Gaussian filter to an image for edge detection.
    
    Parameters:
    - image: The input image to apply the filter on.
    - std_dev: The standard deviation of the Gaussian function.
    - mean: The mean of the Gaussian function.
    - size: The size of the Gaussian filter mask.
    
    Returns:
    - The edge magnitude image after applying the first order Gaussian filter.
    """
    first_order_gaussian_mask = generate_first_order_gaussian_mask(std_dev, mean, size)

    edge_x = scipy.signal.convolve2d(image, first_order_gaussian_mask[None, :], mode='same')
    edge_y = scipy.signal.convolve2d(image, first_order_gaussian_mask[:, None], mode='same')

    first_order_gaussian_image = magnitude(edge_x, edge_y)

    return first_order_gaussian_image

### Laplacian ###

def apply_laplacian_filter(image):
    """
    Apply the Laplacian filter for edge detection.
    
    Parameters:
    - image: The input image to detect edges on.
    
    Returns:
    - The edge magnitude image after applying the Laplacian filter.
    """

    # Define laplacian operator
    laplacian_filter = np.array([[1, 1, 1],
                             [1, -8, 1],
                             [1, 1, 1]])
    
    laplacian_image = scipy.signal.convolve2d(image, laplacian_filter, mode='same')

    return laplacian_image

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

cells1 = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/9343 AM.bmp"))
cells2 = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/10905 JL.bmp"))
cells3 = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/43590 AM.bmp"))

cells_orginal = [cells1, cells2, cells3]

#cells = [filters.gaussian(cells1, sigma=1) > threshold_otsu(filters.gaussian(cells1, sigma=1)), filters.gaussian(cells2, sigma=1) > threshold_otsu(filters.gaussian(cells2, sigma=1)), filters.gaussian(cells3, sigma=1) > threshold_otsu(filters.gaussian(cells3, sigma=1))]

cells = [filters.gaussian(cells1, sigma=1), filters.gaussian(cells2, sigma=1), filters.gaussian(cells3, sigma=1)]

functions = [apply_roberts_filter, apply_sobels_filter, apply_first_order_gaussian_filter, apply_laplacian_filter, apply_log_filter]

function_names = ["Roberts", "Sobels", "First Order Gaussian", "Laplacian", "Laplacian of Gaussian"]

def show_filter_results(images_orginal, images, functions, function_names):
    """
    Display the results of applying each filter to each image in a grid of subplots.
    
    Parameters:
    - images: List of images to apply the filters on.
    - functions: List of filter functions to apply to the images.
    - function_names: List of names of the filters corresponding to the functions.
    """
    num_images = len(images)
    num_functions = len(functions)
    
    # Calculate the number of rows and columns needed for the subplots
    nrows = num_images
    ncols = num_functions + 2  # +1 to include the original image
    
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(ncols * 3, nrows * 3))
    
    for i, image in enumerate(images):
        axes[i, 0].imshow(images_orginal[i], cmap='gray')
        axes[i, 0].set_title('Original')
        axes[i, 0].axis('off')

        axes[i, 1].imshow(image, cmap='gray')
        axes[i, 1].set_title('Original')
        axes[i, 1].axis('off')
        
        for j, function in enumerate(functions):
            filtered_image = function(image)
            ax = axes[i, j + 2]  # j+1 because the first column is for the original image
            ax.imshow(filtered_image, cmap='gray')
            ax.set_title(function_names[j])
            ax.axis('off')
    
    plt.tight_layout()
    plt.show()

show_filter_results(cells_orginal, cells, functions, function_names)