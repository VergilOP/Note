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

def magnitude(x,y):
    return np.sqrt(np.square(x) + np.square(y))

### Roberts ###

roberts_x = np.array(
    [[1,0],
     [0,-1]])

roberts_y = np.array(
    [[0,1],
     [-1,0]])

def apply_roberts_filter(image):
    roberts_cross_x = scipy.signal.convolve2d(image, roberts_x, mode='same')
    roberts_cross_y = scipy.signal.convolve2d(image, roberts_y, mode='same')

    roberts_image = magnitude(roberts_cross_x,roberts_cross_y)
    
    thresh = threshold_otsu(roberts_image)
    return roberts_image < thresh

### Sobel ###

sobel_x = np.array(
    [[1,0,-1],
     [2,0,-2],
     [1,0,-1]])

sobel_y = np.array(
    [[1,2,1],
     [0,0,0],
     [-1,-2,-1]])

def apply_sobels_filter(image):
    sobel_cross_x = scipy.signal.convolve2d(image, sobel_x, mode='same')
    sobel_cross_y = scipy.signal.convolve2d(image, sobel_y, mode='same')

    sobels_image = magnitude(sobel_cross_x,sobel_cross_y)

    thresh = threshold_otsu(sobels_image)
    return sobels_image < thresh

### First Order Gaussian ###

def zero_mean_gaussian(std_dev, mean, vec):

    return 1/np.sqrt(2 * np.pi * (std_dev ** 2)) * np.exp(- (vec - mean) ** 2 / (2 * std_dev ** 2))

def first_order_gaussian(std_dev, mean, vec):

    return - ((vec - mean) / std_dev ** 2) * (zero_mean_gaussian(std_dev, mean, vec))

def generate_first_order_gaussian_mask(std_dev, mean, size):
    """Generate a Gaussian filter mask."""
    vec = np.arange(-size // 2, size // 2 + 1, 1, dtype=np.float32)
    one_d_first_order_gaussian = first_order_gaussian(std_dev, mean, vec)
    
    return one_d_first_order_gaussian

def apply_first_order_gaussian_filter(image, std_dev=1, mean = 0, size = 9):
    first_order_gaussian_mask = generate_first_order_gaussian_mask(std_dev, mean, size)

    edge_x = scipy.signal.convolve2d(image, first_order_gaussian_mask[None, :], mode='same')
    edge_y = scipy.signal.convolve2d(image, first_order_gaussian_mask[:, None], mode='same')

    edge_magnitude = magnitude(edge_x, edge_y)

    thresh = threshold_otsu(edge_magnitude)
    return edge_magnitude < thresh

### Laplacian ###

def apply_laplacian_filter(image):
    laplacian_filter = np.array([[1, 1, 1],
                             [1, -8, 1],
                             [1, 1, 1]])
    
    laplacian_image = scipy.signal.convolve2d(image, laplacian_filter, mode='same')

    return laplacian_image < 0.09

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

    # Threshold the filtered image to obtain a binary edge map.
    # Pixels with values below 0.09 are set to True (edges), others are set to False.
    return log_image < 0.09

### Canny ###

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

shakey = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/shakey.jpg"))
plt.imshow(apply_log_filter(shakey), cmap=plt.cm.gray)
plt.show()

cells1      = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/9343 AM.bmp"))
cells2      = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/10905 JL.bmp"))
cells3      = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/43590 AM.bmp"))

cells1_edge = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/9343 AM Edges.bmp"))
cells2_edge = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/10905 JL Edges.bmp"))
cells3_edge = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/Data/Cells/43590 AM Edges.bmp"))

cells = [filters.gaussian(cells1, sigma=1), filters.gaussian(cells2, sigma=1), filters.gaussian(cells3, sigma=1)]

cells_edge = [cells1_edge, cells2_edge, cells3_edge]

functions = [apply_roberts_filter, apply_sobels_filter, apply_first_order_gaussian_filter, apply_laplacian_filter, apply_log_filter, apply_canny_filter]

function_names = ["Roberts", "Sobels", "First Order Gaussian", "Laplacian", "Laplacian of Gaussian", "Canny"]

def apply_predicts(image):
    predict_images = []
    for function in functions:
        #show_binary_image(function(image))
        predict_images.append(function(image))

    return predict_images

def show_ROC_curve(images, images_edge, method_names):
    plt.figure(figsize=(10, 8))

    for index, image in enumerate(images):
        ground_truth_image = images_edge[index]

        predict_images = apply_predicts(image)

        assert len(predict_images) == len(method_names)

        for predict_image, method_name in zip(predict_images, method_names):
            fpr, tpr, thresholds = roc_curve(ground_truth_image.ravel(), predict_image.ravel())
            roc_auc = auc(fpr, tpr)
            plt.plot(fpr, tpr, lw=2, label='ROC curve of %s (area = %0.2f)' % (method_name, roc_auc))

        plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
        plt.xlim([0.0, 1.0])
        plt.ylim([0.0, 1.05])
        plt.xlabel('False Positive Rate')
        plt.ylabel('True Positive Rate')
        plt.title('Receiver Operating Characteristic to compare edge detection methods')
        plt.legend(loc="lower right")
        plt.show()

show_ROC_curve(cells, cells_edge, function_names)

#otsu_thresh = threshold_otsu(cells1)
#print(otsu_thresh)
#show_binary_image(cells1)
#show_binary_image(cells1 < otsu_thresh)
#show_binary_image(apply_roberts_filter(cells1 < otsu_thresh))
#show_binary_image(apply_sobels_filter(cells1 < otsu_thresh))
#show_binary_image(apply_first_order_gaussian_filter(cells1 < otsu_thresh))
#show_binary_image(apply_laplacian_filter(cells1 < otsu_thresh))
#show_binary_image(apply_log_filter(cells1 > otsu_thresh))

# for cell in cells:
#     fig, ax = plt.subplots(1, 6, figsize=(20, 4))
# 
#     ax[0].imshow(apply_roberts_filter(cell), cmap='gray')
#     ax[0].set_title('Roberts Filter')
#     ax[0].axis('off')
#     
#     ax[1].imshow(apply_sobels_filter(cell), cmap='gray')
#     ax[1].set_title('Sobel Filter')
#     ax[1].axis('off')
#     
#     ax[2].imshow(apply_1g_filter(cell), cmap='gray')
#     ax[2].set_title('Gaussian Filter')
#     ax[2].axis('off')
#     
#     ax[3].imshow(apply_laplacian_filter(cell), cmap='gray')
#     ax[3].set_title('Laplacian Filter')
#     ax[3].axis('off')
#     
#     ax[4].imshow(apply_log_filter(cell), cmap='gray')
#     ax[4].set_title('LoG Filter')
#     ax[4].axis('off')
# 
#     ax[5].imshow(generate_canny_filter_masks(cell), cmap=plt.cm.gray)
#     ax[5].set_title('Canny Filter')
#     ax[5].axis('off')
# 
#     plt.show()


#shakey = skimage.color.rgb2gray(skimage.io.imread("C:/_Study_Resource/Study_Note/University/3rd/Computer Vision and images/Report/shakey.jpg"))
#
#show_binary_image(apply_log_filter(shakey))



# # 应用每个过滤器，并存储结果
# predicted_roberts = [apply_roberts_filter(cell) for cell in cells]
# predicted_sobel = [apply_sobels_filter(cell) for cell in cells]
# predicted_gaussian = [apply_1g_filter(cell) for cell in cells]
# predicted_laplacian = [apply_laplacian_filter(cell) for cell in cells]
# predicted_log = [apply_log_filter(cell) for cell in cells]
# # Canny过滤器的结果可能需要根据你的实现进行调整
# predicted_canny = [generate_canny_filter_masks(cell) for cell in cells]
# 
# # 绘制每个过滤器的ROC曲线
# print("ROC Curves for Roberts Filter:")
# calculate_roc_curve(cells_edge, predicted_roberts)
# 
# print("ROC Curves for Sobel Filter:")
# calculate_roc_curve(cells_edge, predicted_sobel)
# 
# print("ROC Curves for Gaussian Filter:")
# calculate_roc_curve(cells_edge, predicted_gaussian)
# 
# print("ROC Curves for Laplacian Filter:")
# calculate_roc_curve(cells_edge, predicted_laplacian)
# 
# print("ROC Curves for LoG Filter:")
# calculate_roc_curve(cells_edge, predicted_log)
# 
# # 如果Canny过滤器应用的话
# print("ROC Curves for Canny Filter:")
# calculate_roc_curve(cells_edge, predicted_canny)



#
#plt.imshow(generate_canny_filter_masks(shakey, 1, 5), cmap=plt.cm.gray)
#plt.show()
