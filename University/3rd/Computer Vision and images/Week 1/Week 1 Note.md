# Week 1 Note

## Introduction

### Overview
- Introduction to the fundamentals of visual perception.
- Discussion on the concept and challenges of computational vision.
- Exploration of applications in various fields.

### Key Concepts
- Biological vision and the human eye.
- Light and image formation.
- Early visual processing in humans.

### Applications
- Automation in navigation and recognition tasks.
- Biomedical applications and human-computer interfaces.

## Human Vision & Edge Detection and Filtering

### Human Vision: Colour

- **Visible Light**: The spectrum of light that is visible to humans, ranging from 380 to 760 nanometers.
  - **可见光**：人类可见的光谱，范围从 380 到 760 纳米。
- **Color Perception**:
  - **Trichromatic Theory**: Proposes that the eye perceives color through three types of receptors sensitive to red, green, and blue.
    - **三色理论**：提出眼睛通过对红、绿、蓝三种敏感的受体来感知颜色。
  - **Opponent Process Coding**: Suggests that color is represented in the visual system as pairs of opposite colors, such as yellow-blue and red-green.
    - **对手过程编码**：表明颜色在视觉系统中表示为相反的颜色对，例如黄蓝和红绿。

### Intensity Images

- **Representation**: An intensity image is represented as a matrix where each element corresponds to a pixel's intensity.
  - **表示**：强度图像表示为一个矩阵，其中每个元素对应一个像素的强度。
- **Indexed Images**: Consist of a data matrix and a colormap, where the colormap defines the color of each pixel.
  - **索引图像**：由数据矩阵和颜色图组成，其中颜色图定义每个像素的颜色。

#### Intensity Gradients

- The image is a function mapping coordinates to intensity f(x,y)  
  ![](./images/Screenshot%202024-05-01%20233811.png)

- The gradient of the intensity is a vector
- **Function of Intensity**: The image is treated as a function mapping coordinates to intensity levels.
  $$
  \tilde{G}[f(x, y)] = \begin{bmatrix}
  G_x \\
  G_y
  \end{bmatrix} = \begin{bmatrix}
  \frac{\partial f}{\partial x} \\
  \frac{\partial f}{\partial y}
  \end{bmatrix}
  $$
  ![](./images/Screenshot%202024-05-01%20234130.png)
- We can think of the gradient as having an x and a y component
  $$
  M(\tilde{G}) = \sqrt{G_x^2 + G_y^2}
  $$

  $$
  \alpha(x, y) = \tan^{-1}\left(\frac{G_y}{G_x}\right)
  $$

- **Gradient Vector**: Defined as the vector of partial derivatives with respect to x and y, indicating the direction and rate of intensity change.
  $$
  G_x \approx f(i, j+1) - f(i, j)
  $$

  $$
  G_y \approx f(i, j) - f(i+1, j)
  $$

### Edge Detection

- **Concept**: Based on calculating the gradient of intensity across the image.
- **Edge Detectors**:
  - **Roberts Cross Operator**: A simple, early operator used for edge detection.
  The Roberts Cross operators for edge detection are used to approximate the gradient. They are defined as:

  - $G_x$ kernel:
    $$
    \begin{bmatrix}
    1 & 0 \\
    0 & -1
    \end{bmatrix}
    $$

  - $G_y$ kernel:
    $$
    \begin{bmatrix}
    0 & -1 \\
    1 & 0
    \end{bmatrix}
    $$

  - **Sobel Operator**: Uses convolution with a pair of 3x3 masks to estimate gradients.
  The Sobel operators are more robust edge detectors by using a larger area of the image for computing the gradient. They are defined as:

  - $G_x$ kernel:
    $$
    \begin{bmatrix}
    -1 & 0 & 1 \\
    -2 & 0 & 2 \\
    -1 & 0 & 1
    \end{bmatrix}
    $$

  - $G_y$ kernel:
    $$
    \begin{bmatrix}
    1 & 2 & 1 \\
    0 & 0 & 0 \\
    -1 & -2 & -1
    \end{bmatrix}
    $$

#### Convolution

- **Definition**: A process of applying a filter over an image, computed as the weighted sum of pixel values in the neighborhood.

### Noise Reduction

- **Noise Reduction**: Necessary for improving edge detection, achieved through filters like mean and Gaussian filters.

- **Linear Filtering**: A technique for noise reduction and smoothing, applied before edge detection to improve results.
  ```
  for i=2:image_height-1
      for j=2:image_width-1
          A_out(i, j) = ΣΣ A_in(i + y, j + x) * M(y + 2, x + 2)
  ```
  ![](./images/Screenshot%202024-05-01%20235838.png)

#### Laplacian Operator

![](./images/Screenshot%202024-05-02%20000137.png)

