# Imports
import skimage
from skimage.filters import threshold_otsu
from skimage import filters
import scipy
from matplotlib import pyplot as plt
import numpy as np
from sklearn.metrics import roc_curve, auc
import scipy.signal

def normalize_image(image):
    return (image - image.min()) / (image.max() - image.min())

### Task 1/3 ###

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

### Task 1/2: Laplacian of Guassian ###

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

    log_image_normalized = normalize_image(log_image)

    return log_image_normalized

### Task 2 ###

### Task2/4: Roberts ###

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
    
    return 1 - normalize_image(roberts_image)

### Task2/4: Sobel ###

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
        [[-1,0,1],
         [-2,0,2],
         [-1,0,1]])

    sobel_y = np.array(
        [[1,2,1],
         [0,0,0],
         [-1,-2,-1]])

    # Convolve the image with Sobel kernels
    sobel_cross_x = scipy.signal.convolve2d(image, sobel_x, mode='same')
    sobel_cross_y = scipy.signal.convolve2d(image, sobel_y, mode='same')

    # Calculate the magnitude of the gradient
    sobels_image = magnitude(sobel_cross_x,sobel_cross_y)

    return 1 - normalize_image(sobels_image)

### Task2/4: First Order Gaussian ###

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

    return 1 - normalize_image(first_order_gaussian_image)

### Task2/4: Laplacian ###

def apply_laplacian_filter(image):
    """
    Apply the Laplacian filter for edge detection.
    
    Parameters:
    - image: The input image to detect edges on.
    
    Returns:
    - The edge magnitude image after applying the Laplacian filter.
    """

    # Define laplacian operator
    laplacian_filter = np.array([[0, -1, 0], [-1, 4, -1], [0, -1, 0]])
    
    laplacian_image = scipy.signal.convolve2d(image, laplacian_filter, mode='same')

    return 1 - normalize_image(laplacian_image)

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

### Task3: Canny ###

def apply_canny_filter(image, std_dev = 1, size= 9, high_ratio=0.90, low_ratio=0.50):
    """Generate masks for the Canny filter."""
    n = size // 2
    y, x = np.ogrid[-n:n+1, -n:n+1]

    # Calculate the Gaussian function
    G = 1 / (2 * np.pi * std_dev**2) * np.exp(-(x**2 + y**2) / (2 * std_dev**2))

    # Calculate the derivatives of the Gaussian function
    Gx = (-x / std_dev**2) * G
    Gy = (-y / std_dev**2) * G

    fx = scipy.signal.convolve(image, Gx, mode='same')
    fy = scipy.signal.convolve(image, Gy, mode='same')

    magnitude = np.abs(fx) + np.abs(fy)
    direction = np.arctan2(fx, fy)

    M, N = magnitude.shape
    canny_image = np.zeros((M,N), dtype=np.float32)

    angle = direction * (180. / np.pi)
    angle[angle < 0] += 180

    for i in range(1, M-1):
        for j in range(1, N-1):
            try:
                q = 255
                r = 255
                
                if (0 <= angle[i,j] < 22.5) or (157.5 <= angle[i,j] <= 180):
                    q = magnitude[i, j+1]
                    r = magnitude[i, j-1]
                elif (22.5 <= angle[i,j] < 67.5):
                    q = magnitude[i+1, j-1]
                    r = magnitude[i-1, j+1]
                elif (67.5 <= angle[i,j] < 112.5):
                    q = magnitude[i+1, j]
                    r = magnitude[i-1, j]
                elif (112.5 <= angle[i,j] < 157.5):
                    q = magnitude[i-1, j-1]
                    r = magnitude[i+1, j+1]

                if (magnitude[i,j] >= q) and (magnitude[i,j] >= r):
                    canny_image[i,j] = magnitude[i,j]
                else:
                    canny_image[i,j] = 255

            except IndexError as e:
                pass

    high_threshold = np.percentile(magnitude, high_ratio * 100)
    low_threshold = high_threshold * low_ratio

    strong_i, strong_j = np.where(magnitude >= high_threshold)
    zeros_i, zeros_j = np.where(magnitude < low_threshold)
    weak_i, weak_j = np.where((magnitude <= high_threshold) & (magnitude >= low_threshold))
    
    canny_image[strong_i, strong_j] = 0
    canny_image[zeros_i, zeros_j] = 255
    
    for i, j in zip(weak_i, weak_j):
        if 0 in (canny_image[i-1:i+2, j-1:j+2]):
            canny_image[i, j] = 255
        else:
            canny_image[i, j] = 0

    return canny_image

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

### Task 4 ###

def apply_predicts(image):
    """
    Apply edge detection functions to an image and threshold the results.
    
    Parameters:
    - image: The input image on which to apply the edge detection functions.
    
    Returns:
    - predict_images: A list of binary images resulting from applying edge detection
                      and thresholding to the input image.
    """
    predict_images = []
    for index, function in enumerate(functions):
            predict_images.append(function(image))

    return predict_images

def show_ROC_curve(images, images_edge, method_names):
    """
    Display ROC curves to compare the performance of different edge detection methods.
    
    Parameters:
    - images: List of original images.
    - images_edge: List of binary images representing the ground truth for edges.
    - method_names: List of names corresponding to the edge detection methods.
    """
    plt.figure(figsize=(10, 8))

    for index, image in enumerate(images):
        ground_truth_image = images_edge[index]

        predict_images = apply_predicts(image)

        assert len(predict_images) == len(method_names)

        for predict_image, method_name in zip(predict_images, method_names):
            fpr, tpr, thresholds = roc_curve(ground_truth_image.ravel(), predict_image.ravel())
            roc_auc = auc(fpr, tpr)
            plt.plot(fpr, tpr, lw=2, label='ROC curve of %s (area = %0.2f)' % (method_name, roc_auc))
            #optimal_idx = np.argmax(tpr - fpr)
            #optimal_threshold = thresholds[optimal_idx]
            #binarized_edge = (predict_image > optimal_threshold).astype(int)
            #show_binary_image(binarized_edge)

        plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
        plt.xlim([0.0, 1.0])
        plt.ylim([0.0, 1.05])
        plt.xlabel('False Positive Rate')
        plt.ylabel('True Positive Rate')
        plt.title('Receiver Operating Characteristic to compare edge detection methods')
        plt.legend(loc="lower right")
        plt.show()

# #Gaussian smoothing
# def create_gaussian_kernel(size = 3, sigma = 1):
#     n = size // 2
#     y, x = np.ogrid[-n:n+1, -n:n+1]
#     gaussian_kernel = np.exp(-(x**2 + y**2) / (2 * sigma**2))
#     gaussian_kernel /= gaussian_kernel.sum()
#     return gaussian_kernel
# 
# def apply_gaussian_blur(image):
#     # """Apply Gaussian blur to an image."""
#     gaussian_kernel = create_gaussian_kernel()
#     return scipy.signal.convolve2d(image, gaussian_kernel, mode='same')

#--- Task 1 ---#
shakey = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/shakey.jpg"))

shakey_smoothed = filters.gaussian(shakey, sigma=1)

#--- Task 2/3/4 ---#
cells1 = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/9343 AM.bmp"))
cells2 = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/10905 JL.bmp"))
cells3 = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/43590 AM.bmp"))

cells1 = 1 - cells1
cells2 = 1 - cells2
cells3 = 1 - cells3

cells1_edge = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/9343 AM Edges.bmp"))
cells2_edge = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/10905 JL Edges.bmp"))
cells3_edge = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/43590 AM Edges.bmp"))

cells_orginal = [cells1, cells2, cells3]

cells = [filters.gaussian(cells1, sigma=1), filters.gaussian(cells2, sigma=1), filters.gaussian(cells3, sigma=1)]
#cells = [apply_gaussian_blur(cells1), apply_gaussian_blur(cells1), apply_gaussian_blur(cells1)]
#cells = [cells1 < threshold_otsu(cells1), cells2 < threshold_otsu(cells2), cells3 < threshold_otsu(cells3)]
# for cell in cells:
#     show_binary_image(cell)

cells_edge = [cells1_edge, cells2_edge, cells3_edge]

functions = [apply_roberts_filter, apply_sobels_filter, apply_first_order_gaussian_filter, apply_laplacian_filter, apply_log_filter]

function_names = ["Roberts", "Sobels", "First Order Gaussian", "Laplacian", "Laplacian of Gaussian"]

# Task1 #

#show_binary_image(shakey)
#
#show_binary_image(shakey_smoothed)
#
#show_binary_image(apply_log_filter(shakey_smoothed))

# Task2

show_filter_results(cells_orginal, cells, functions, function_names)

# Task3 #

#show_canny_results(cells)

# Task4 #

show_ROC_curve(cells, cells_edge, function_names)