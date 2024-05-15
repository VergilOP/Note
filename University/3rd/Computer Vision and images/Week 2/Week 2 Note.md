# Week 2 Note

## Advanced Edge Detection

### What Causes Intensity Changes?
- Geometric events like `surface orientation`, `depth`, `color and texture discontinuities`.  
  - 几何事件，如“表面方向”、“深度”、“颜色和纹理不连续性”
- Non-geometric events such as `illumination` changes, `specularities`, `shadows`, and `inter-reflections`.
  - 非几何事件，例如“照明”变化、“镜面反射”、“阴影”和“相互反射”。

### Goal of Edge Detection
- Produce a line “drawing” of a scene from an image of that scene.
  - 根据场景的图像制作场景的线条“画”。

### Why Edge Detection Is Useful
- Important features can be extracted from the edges of an image
  - 可以从图像的边缘提取重要特征
- These features are used by higher-level computer vision algorithms
  - 这些特征被更高级别的计算机视觉算法使用

### Edge Descriptors
- `Direction`, `strength`, and `position` of edges in the image.

- `Edge direction`: perpendicular to the direction of maximum intensity change (i.e., edge normal)
  - `边缘方向`：垂直于最大强度变化的方向（即边缘法线）
- `Edge strength`: related to the local image contrast along the normal.
  - `边缘强度`：与沿法线的局部图像对比度有关。 
- `Edge position`: the image position at which the edge is located
  - `边缘位置`：边缘所在的图像位置

### Main Steps in Edge Detection
1. **Smoothing**: To reduce noise without losing edge details.
     - **平滑**：在不丢失边缘细节的情况下降低噪声。 
2. **Enhancement**: Application of differentiation to sharpen edges.
     - **增强**：应用微分来锐化边缘。 
3. **Thresholding**: To discriminate between true edges and noise.
     - **阈值**：区分真实边缘和噪声。 
4. **Localization**: To pinpoint the exact location of edges, possibly requiring sub-pixel resolution.
     - **定位**：精确定位边缘的准确位置，可能需要亚像素分辨率。

### Edge Detection Using Derivatives
- Points on edges can be found by detecting local `maxima`/`minima` of the first derivative or `zero-crossings` of the second derivative.

- Often, points that lie on an edge are detected by:
  1. Detecting the local maxima or minima of the first dervative
     - 检测一阶导数的局部最大值或最小值
  2. Detecting the zero-crossings of the second derivative  
     - 检测二阶导数的零交叉点

1. **Sobel Operator** (approximates the gradient of the image intensity function):
   - Horizontal (Gx):
     $$
     G_x = \begin{bmatrix} -1 & 0 & 1 \\ -2 & 0 & 2 \\ -1 & 0 & 1 \end{bmatrix}
     $$
   - Vertical (Gy):
     $$
     G_y = \begin{bmatrix} -1 & -2 & -1 \\ 0 & 0 & 0 \\ 1 & 2 & 1 \end{bmatrix}
     $$

2. **Roberts Cross Operator** (captures edge information on a diagonal):
   $$
   G = \begin{bmatrix} 0 & 1 \\ -1 & 0 \end{bmatrix} \quad \text{or} \quad \begin{bmatrix} 1 & 0 \\ 0 & -1 \end{bmatrix}
   $$

3. **Prewitt Operator** (similar to Sobel, emphasizes horizontal or vertical edges):
   - Horizontal:
     $$
     G_x = \begin{bmatrix} -1 & 0 & 1 \\ -1 & 0 & 1 \\ -1 & 0 & 1 \end{bmatrix}
     $$
   - Vertical:
     $$
     G_y = \begin{bmatrix} -1 & -1 & -1 \\ 0 & 0 & 0 \\ 1 & 1 & 1 \end{bmatrix}
     $$

### Edge Detection Steps Using Gradient

1. Smooth the input image
   $$ \hat{f}(x, y) = f(x, y) * G(x, y) $$

2. Find the gradient of the smoothed image in the x direction
   $$ \hat{f}_x = \hat{f}(x, y) * M_x(x, y) \rightarrow \frac{\partial f}{\partial x} $$

3. Find the gradient of the smoothed image in the y direction
   $$ \hat{f}_y = \hat{f}(x, y) * M_y(x, y) \rightarrow \frac{\partial f}{\partial y} $$

4. Calculate the gradient magnitude (note: avoiding the square root operation for efficiency)
   $$ magn(x, y) = |\hat{f}_x| + |\hat{f}_y| $$

5. Determine the gradient direction
   $$ dir(x, y) = \tan^{-1}(\frac{\hat{f}_y}{\hat{f}_x}) $$

6. Identify potential edge points by comparing the magnitude to a threshold $T$
   - If $magn(x, y) > T$, then it is considered a possible edge point.

### Practical Issues
- Noise suppression-localization tradeoff.
  - Smoothing depends on mask size (e.g., depends on σ for Gaussian filters)  
    - 平滑取决于掩模尺寸（例如，取决于高斯滤波器的 σ）
  - Larger mask sizes reduce noise, but worsen localization (i.e., add uncertainty to the location of the edge) and vice versa
    - 较大的掩模尺寸会降低噪声，但会使定位恶化（即增加边缘位置的不确定性），反之亦然

- Criteria for Optimal Edge Detection
  - Good detection
    - Minimize the probability of false positives (i.e., spurious edges)
      - 尽量减少误报（即虚假边缘）的概率
    - Minimize the probability of false negatives (i.e., missing real edges)
      - 尽量减少误报（即遗漏真实边缘）的概率
  - Good localization
    - Detected edges must be as close as possible to the true edges
      - 检测到的边缘必须尽可能接近真实边缘
  - Single response
    - Minimize the number of local maxima around the true edge.
      - 尽量减少真实边缘周围的局部最大值的数量。

### Canny Edge Detector
- Canny has shown that the first derivative of the Gaussian closely approximates the operator that optimizes the product of signal-to-noise ratio and localization.
  - Canny 已证明，高斯的一阶导数非常接近优化信噪比和定位乘积的算子。

### Steps of the Canny Edge Detector

1. Compute $f_x$ and $f_y$
   - $f_x = \frac{\partial}{\partial x} (f * G) = f * \frac{\partial G}{\partial x} = f * G_x$
   - $f_y = \frac{\partial}{\partial y} (f * G) = f * \frac{\partial G}{\partial y} = f * G_y$
   - $G(x, y)$ is the Gaussian function.
   - $G_x(x, y)$ is the derivative of $G(x, y)$ with respect to x: $G_x(x, y) = -\frac{x}{\sigma^2} G(x, y)$
   - $G_y(x, y)$ is the derivative of $G(x, y)$ with respect to y: $G_y(x, y) = -\frac{y}{\sigma^2} G(x, y)$

2. Compute the gradient magnitude (and direction)
   - $magn(x, y) = |\hat{f}_x| + |\hat{f}_y|$
   - $dir(x, y) = \tan^{-1}(\frac{\hat{f}_y}{\hat{f}_x})$

3. Apply non-maxima suppression.

4. Apply **hysteresis** thresholding/edge linking.

### Hysteresis Thresholding

Hysteresis thresholding is a method used in edge detection algorithms to preserve edge continuity. Instead of using a single threshold, it employs two:
- A **high threshold** (`th`) to identify strong edges.
- A **low threshold** (`tl`) to continue edge curves.

The typical setting is `th = 2 * tl`, but this can vary based on the specific requirements of the image or application.

`Idea`: use a high threshold to start edge curves and a low threshold to continue them.

## Scale Invariant Feature Transform(SIFT)

### Why do we care about matching features?
- Object Recognition
- Wide baseline matching
  - Given any two images, estimate the fundamental matrix and a set of matched interest points
- Tracking

### Types of invariance
- Illumination
  - 照明
- Scale
  - 缩放
- Rotation
  - 旋转
- Affine
  - 仿射
- Full Perspective
  - 全透视

#### How to achieve illumination invariance(光照不变)

- The easy way(normalized)
- Differece based metrics(sift)

#### How to achieve scale invariance

- Pyramids
  - Divide width and height by 2
    - 将宽度和高度除以 2
  - Take average of 4 pixels for each pixel (or Gaussian blur)
    - 对每个像素取 4 个像素的平均值（或高斯模糊）
  - Repeat until image is tiny
    - 重复直到图像变小
  - Run filter over each size image and hope its robust
    - 对每个尺寸的图像运行过滤器并希望其稳健
- Scale Space (DOG method)
  - Pyramid but fill gaps with blurred images
    - 金字塔，但用模糊图像填补空白
  - Like having a nice linear scaling without the expense
    - 就像拥有一个没有代价的良好线性缩放
  - Take features from differences of these images
    - 从这些图像的差异中获取特征
  - If the feature is repeatably present in between Difference of Gaussians it is Scale Invariant and we should keep it.
    - 如果特征在高斯差异之间重复出现，则它是尺度不变的，我们应该保留它。

#### Rotation Invariance

- Rotate all features to go the same way in a determined manner
  - 旋转所有特征，以确定的方式朝同一方向移动
- Take histogram of Gradient directions
  - 取梯度方向的直方图
- Rotate to most dominant (maybe second if its good enough)
  - 旋转到最主要的（如果足够好的话，也许是第二个）

## Motion

- Boundary Detection
- Tracking
- Optical flow
  - 光流
- Video Mosaics
  - 视频马赛克
- Video Compression
- Geo Registration

### Dynamic Vision

- FOUR possibilities for dynamic nature of camera and world:
  - Stationary Camera, stationary Objects
    - 静止相机，静止物体
  - Stationary Camera, moving Objects
    - 静止相机，移动物体
  - Moving Camera, stationary Objects
    - 移动相机，静止物体
  - Moving Camera, moving Objects
    - 移动相机，移动物体


- Detecting a change
  - Where has an image changed?
  - F(x,y,i) is the intensity of the image at time i, at point x,y
  - Difference picture, DP
    $$
    DP_{12}(x, y) = 
    \begin{cases} 
    1 & \text{if } |F(x, y, 1) - F(x, y, 2)| > \tau \\
    0 & \text{otherwise}
    \end{cases}
    $$

- Connectedness
  - To filter out noise, we can use the idea of 8 or 4 connectedness:
    - 为了滤除噪音，我们可以使用 8 或 4 连通性的理念：
    - Two pixels are 4-neighbours if they share a common boundary
      - 如果两个像素共享一个共同边界，则它们为 4 邻域
    - Two pixels are 8-neighbours if they share at least a common corner
      - 如果两个像素至少共享一个共同角，则它们为 8 邻域
    - Two pixels are 8 connected if we can create a path of 8-neighbours from one to the other
      - 如果我们可以创建从一个像素到另一个像素的 8 邻域路径，则两个像素为 8 连通

- Removing Noise
  - Pixels not in a connected cluster of a certain size are removed from the difference image

- The aperture problem 光圈问题
  - The grating appears to be moving down and to the right
    - 光栅似乎正在向下和向右移动
  - But it could be moving in many other directions
       - 但它可能会朝许多其他方向发展
    - such as only down, or only to the right
           - 例如仅向下或仅向右

### Motion correspondence

- One way is to pick a bunch of interesting points in each image
  - 一种方法是在每幅图像中挑选一堆有趣的点
- And then match them (hoping they can be matched)
  - 然后匹配它们（希望它们能够匹配）
- e.g. pick corner points using a corner detector
  - 例如使用角点检测器挑选角点
- Or use a Moravec operator
  - 或者使用 Moravec 算子

### Moravec operator
- The Moravec operator is considered a corner detector
  - Moravec 算子被视为角点检测器
  - it defines interest points as points where there is a large intensity variation in every direction.
    - 它将兴趣点定义为各个方向上强度变化较大的点。
  - This is the case at corners.
    - 角点就是这种情况
  - It may be sensitive to detecting isolated pixels as corners.
    - 它可能对将孤立像素检测为角点很敏感。

- measure the intensity variation by placing a small square window (typically, 3x3, 5x5, or 7x7 pixels) centered at P
  - 通过放置一个小方形窗口（通常为 3x3、5x5 或 7x7 像素）来测量强度变化，该窗口以 P 为中心
  - then shifting this window by one pixel in each of the eight principle directions (horizontally, vertically, and four diagonals).
    - 然后在八个主要方向（水平、垂直和四个对角线）上分别将该窗口移动一个像素。

### Motion Correspondence

- Motion correspondence (matching) is guided by three principles
  - 运动对应（匹配）遵循三个原则
  1. Discreteness: a measure of the distinctiveness of individual points
       - 离散性：衡量单个点的独特性
  2. Similarity: a measure of how closely two points resemble one another
       - 相似性：衡量两个点彼此相似的程度
  3. Consistency: a measure of how well a match conforms with nearby matches
       - 一致性：衡量匹配与附近匹配的一致性

- A sketch of the algorithm
  - 算法概述
  - Get the interest points in each image
    - 获取每幅图像中的兴趣点
  - Pair each feature point in the first image with every feature point in the other image within some distance
    - 将第一幅图像中的每个特征点与另一幅图像中一定距离内的每个特征点配对
  - Calculate the degree of similarity between the images for each possible match
    - 计算每幅可能匹配的图像之间的相似度
  - Use this to calculate the likelihood of each match
    - 以此计算每幅匹配的可能性
  - Now revise the match likelihood for each point using the nearby matches
    - 现在使用附近的匹配修改每个点的匹配可能性

#### Calculating the degree of similarity

- To calculate the degree of similarity between two patches take the sum of the differences in the pixel values

#### Calculating the likelihood of a match

1. **计算权重**：
   - 首先计算每个潜在匹配（patch）的相似度得分（$S_{i1}, S_{i2}$ 等），例如得分24和30。
   - 这些得分转换为权重 $w_{il}$ 和 $w_{i2}$，计算方式为：
     $$
     w_{il} = \frac{1}{1 + \alpha S_{il}}
     $$
     其中 $\alpha$ 是一个调节参数，用于控制相似度得分对权重的影响。

2. **归一化权重**：
   - 将所有可能匹配的权重进行归一化，以形成匹配的概率分布。这是通过将每个权重除以所有权重的总和来实现的。这样，所有概率加起来等于1，表示完整的概率分布。
