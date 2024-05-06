# #Task 4: Evaluation

from skimage.filters import threshold_otsu

def apply_otsu_threshold(image):
    thresh = threshold_otsu(image)
    print(f"Otsu's threshold: {thresh}")
    binary_image = image > thresh
    return binary_image

# 应用每个边缘检测算法并使用 Otsu 阈值
edge_roberts = apply_otsu_threshold(roberts_edge_detection(image))
edge_sobel = apply_otsu_threshold(sobel_edge_detection(image>23))
edge_gaussian = apply_otsu_threshold(gaussian_edge_detection(image, kernel_size=3 ,sigma=1))
edge_gaussian_sobel = apply_otsu_threshold(gaussian_edge_detection_sobel(image>23, kernel_size=3, sigma=2))
edge_gaussian_roberts = apply_otsu_threshold(gaussian_edge_detection_roberts(image>23, kernel_size=3, sigma=2))
edge_laplacian = apply_otsu_threshold(laplacian_edge_detection(image>25))
edge_log = apply_otsu_threshold(log_edge_detection(image>15, sigma=2))


images = [invert_image(edge_roberts), invert_image(edge_sobel), invert_image(edge_gaussian), invert_image(edge_gaussian_sobel), invert_image(edge_gaussian_roberts), invert_image(edge_laplacian), invert_image(edge_log)]

plt.figure(figsize=(20, 20))
for i, (img, title) in enumerate(zip(images, titles), 1):
    plt.subplot(3, 3, i)
    plt.imshow(img, cmap='gray')
    plt.title(title)
    plt.axis('off')
plt.show()


import numpy as np

def check_pixel_values(image):
    # 找到图像中的唯一值
    unique_values = np.unique(image)
    return unique_values

# 假设 grund_truth 是您的 ground truth 图像
unique_values_ground_truth = check_pixel_values(edge_sobel)

print("处理前的唯一像素值:", unique_values_ground_truth)

from sklearn.metrics import roc_curve, auc

def process_edge_detection_result(image):
    normalized = (image / image.max()) * 255
    uint8_image = normalized.astype(np.uint8)
    # print(f"Unique values in processed image: {np.unique(uint8_image)}")
    inverted = invert_image(uint8_image)
    print(f"Unique values in inverted image: {np.unique(inverted)}")
    return binarize_image(inverted)

# 二值化函数
def binarize_image(image, threshold=128):
    return (image > threshold).astype(int)

import numpy as np

binarized_images = [
    process_edge_detection_result(edge_roberts),
    process_edge_detection_result(edge_sobel),
    process_edge_detection_result(edge_gaussian),
    process_edge_detection_result(edge_gaussian_sobel),
    process_edge_detection_result(edge_gaussian_roberts),
    process_edge_detection_result(edge_laplacian),
    process_edge_detection_result(edge_log),
    process_edge_detection_result(image),
]
unique_values_ground_truth = check_pixel_values(process_edge_detection_result(edge_sobel))

print("处理后的唯一像素值:", unique_values_ground_truth)


titles = ['Roberts', 'Sobel', 'gaussian','gaussian_sobel','gaussian_roberts','laplacian','log','image']  # 请根据实际情况填写


# print(grund_truth)
# 二值化 ground truth
binarized_ground_truth = binarize_image(grund_truth)
print(binarized_ground_truth)

# 定义并绘制 ROC 曲线
from sklearn.metrics import confusion_matrix

def calculate_sensitivity_specificity(test_image, ground_truth):
    # 计算混淆矩阵
    tn, fp, fn, tp = confusion_matrix(ground_truth.flatten(), test_image.flatten(), labels=[0, 1]).ravel()

    # 计算敏感度和特异性
    sensitivity = tp / (tp + fn) if tp + fn != 0 else 0
    specificity = tn / (tn + fp) if tn + fp != 0 else 0

    return sensitivity, specificity

# 在绘制 ROC 曲线的同时计算敏感度和特异性
def plot_roc_curve_and_calculate_stats(binarized_images, ground_truth, titles):
    plt.figure(figsize=(10, 10))

    for img, title in zip(binarized_images, titles):
        fpr, tpr, _ = roc_curve(ground_truth.flatten(), img.flatten())
        roc_auc = auc(fpr, tpr)
        plt.plot(fpr, tpr, label=f'{title} (AUC = {roc_auc:.2f})')

        # 计算敏感度和特异性
        sensitivity, specificity = calculate_sensitivity_specificity(img, ground_truth)
        print(f'{title} - Sensitivity: {sensitivity:.2f}, Specificity: {specificity:.2f}')

    plt.plot([0, 1], [0, 1], 'k--')
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('ROC Curves')
    plt.legend(loc="lower right")
    plt.show()

# 执行分析
plot_roc_curve_and_calculate_stats(binarized_images, binarized_ground_truth.flatten(), titles)
