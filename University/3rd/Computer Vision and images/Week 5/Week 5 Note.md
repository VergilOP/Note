# Week 5 Note

## From classical to deep learning-based methods in Computer Vision - Part 1

### CV: Edge detection-classical paradigm

- Edge detection was often the first step of a processing pipeline
- This step was both intuitive(intensity changes are where things happen) and pargmatic(significant data reduction for further processing)

### Convolution

for i=2: image_height-1  
  for j=2: image_width-1  
    $A_{out}(i,j) = \sum^1_{y=-1}$ 

Design choices
- Selecting a filter function
- Discretisation
- Underlying assumptions(e.g. noise in the image)
- How do we deal with multiple responses of the detectors that occur close to each other?

### Edge Detection - Different filters

- Different design choices(assumptions) resulted in a number of different filters

1. **高斯平滑**：首先，图像被高斯滤波器平滑处理，这有助于减少图像噪声。高斯滤波的程度由参数 $\sigma$（标准差）控制，$\sigma$ 值越大，图像越平滑。

2. **拉普拉斯边缘检测**：接着，对平滑后的图像应用拉普拉斯算子。拉普拉斯算子是一个二阶导数算子，非常敏感于灰度变化，因此可以识别边缘。

3. **零交叉**：LoG算子的边缘检测基于零交叉方法。零交叉是指拉普拉斯算子结果从正变负或从负变正的地方，这些地方通常对应于图像中的边缘。


### Combining filters

- Combining different convolution filters
    - 组合不同的卷积滤波器
- Associativity of the convolution enables us to exchange successive filtering with a single filter whose impulse response is a convolution of the initial filter's impulse responses
    - 卷积的关联性使我们能够用单个滤波器来交换连续滤波，该滤波器的脉冲响应是初始滤波器脉冲响应的卷积
- Example: a combined filter for Gaussian smoothing [5x5] and Laplace [3x3] kernel
    - 示例：高斯平滑 [5x5] 和拉普拉斯 [3x3] 核的组合滤波器

### Canny Edge Detector

- Bringing together many considerations:
  - Good detection
  - Good localisation
  - Single response
- Pipeline of modules  
    ![](./images/Screenshot%202024-05-02%20082613.png)

### Edge Detectors - Performance evaluation
- A well conveived and designed edge detector, however not without
  - 一个构思和设计良好的边缘检测器，但并非没有
  - Many practical issues(scale, thresholds)
    - 许多实际问题（规模、阈值）

- Datasets
    - 数据集
- Evaluation measures
    - 评估指标
  - Benchmarks - can be used to compare different edge detectors
    - 基准 - 可用于比较不同的边缘检测器
- One of the cornerstones towards more objective evaluation of the CV algorithms
    - 实现更客观评估 CV 算法的基石之一
- Large datasets and benchmarks accelerated development of CV methods and ML/DL methods(and progress in the field)
    - 大型数据集和基准加速了 CV 方法和 ML/DL 方法的发展（以及该领域的进展）

### Performance measures

- To use the dataset as benchmark, we need to define the performance evaluation measures
    - 要使用数据集作为基准，我们需要定义性能评估指标
- This benchmark operates by comparing machine generated contours to human ground-truth data
    - 该基准通过将机器生成的轮廓与人类地面真实数据进行比较来运行
- Edge detection can be considered as a binary classification model which classifies pixels as one of two values-for example, "edge" or "no-edge"
    - 边缘检测可以被视为二元分类模型，将像素分类为两个值之一 - 例如“边缘”或“无边缘”
- Precision-recall framework(it is basically comparing two edge maps)
    - 精确召回框架（它基本上是比较两个边缘图）


![](./images/Screenshot%202024-05-02%20092056.png)

- Precision is the fraction of a model's returned "edge" elements that are actual "edge" elements
    - 精度是模型返回的“边缘”元素中实际“边缘”元素的比例
- Recall is the fraction of image's actual "edge" elements that a model includes in its classified group of "edge" elements
    - 召回率是模型在其分类的“边缘”元素组中包括的图像实际“边缘”元素的比例
- F-score, a metric for evaluating the accuracy of binary classification model. It combines the precision and recall of an algorithm into one metric
    - F 分数，用于评估二分类模型准确率的指标。它将算法的精度和召回率合并为一个指标

- The evaluation methodology developed measures detector performance in terms of precision, the fraction of true positives, and recall, the fraction of ground-truth boundary pixels detected. The global F-measure, or harmonic mean of precision and recall at the optimal detector threshold, provides a summary score
  - 所开发的评估方法衡量了检测器在精度（真阳性比例）和召回率（检测到的地面真实边界像素比例）方面的性能。全局 F 测量值（即最佳检测器阈值下的精度和召回率的调和平均值）提供了汇总分数
  - An algorithm's F-score is calculated using
    - 算法的 F 分数使用以下公式计算
    $$
        F-score = 2(precision\ \times\ recall)/(precision + recall)
    $$
  - The F-score describes an algorithm's performance on a scale of 0 to 1
    - F 分数以 0 到 1 的等级描述算法的性能
  - An F-score of 1 indicates a perfect algorithm and
    - F 分数为 1 表示算法完美
  - an F-score of 0 indicates an algorithm that has failed completely in either recall precision,or both
    - F 分数为 0 表示算法在召回率准确率或两者方面完全失败

### Successful Detectors

- Using local brightness, colour, texture cues
    - 使用局部亮度、颜色、纹理提示
  - Gradient based features
    - 基于梯度的特征
  - Oriented energy detectors
    - 定向能量检测器
  - Colour gradients
    - 颜色梯度
  - Texture gradients (a directional operator that measures the degree to which texture of scale $r$ varies at an image location $(x, y)$ in direction $\theta$)
    - 纹理梯度（一种方向算子，用于测量尺度为 $r$ 的纹理在图像位置 $(x, y)$ 上沿方向 $\theta$ 变化的程度）

- Using local brightness, colour, texture cues
    - 使用局部亮度、颜色、纹理线索
  - A large bank of different detectors tailored to detect a variety of visual phenomena (different scales, different orientations, color spaces)
    - 大量不同的检测器，专门用于检测各种视觉现象（不同尺度、不同方向、颜色空间）
- How can multiple cues be optimised and combined?
    - 如何优化和组合多个线索？ 
  - Answer: Supervised machine learning (however, using manually designed features)
    - 答案：监督机器学习（但是，使用手动设计的特征）

- Cue Combination
    - 提示组合
- Supervised machine learning using manually designed features
    - 使用手动设计的特征进行监督机器学习
- Supervision is provided by the labelled data
    - 标记数据提供监督
- Logistic regression: probability edge / no edge
    - 逻辑回归：概率边缘/无边缘
- Study of the importance of individual detectors
    - 研究单个检测器的重要性

### Computing Textons

- For computing textons, a bank of filters is utilised.
    - 为了计算纹理单元，使用了一组滤波器。
- Each pixel produces a 13-element response to the filter bank.
    - 每个像素对滤波器组产生 13 个元素的响应。
- The responses are clustered with k-means.
    - 响应使用 k 均值聚类。
- This results in a set of universal textons.
    - 这会产生一组通用纹理单元。
- They identify basic structures such as steps, bars, corners, etc.
    - 它们识别基本结构，例如台阶、条形、角等。
- (d) shows the nearest assigned texton (colour coded).
    - (d) 显示最近分配的纹理单元（颜色编码）。

### Classifier Considerations
- The plots show the results for one scale per cue (a) and three scales per cue (b).
    - 图表显示了每个提示一个量表 (a) 和每个提示三个量表 (b) 的结果。 
- Many classifiers:
    - 许多分类器：
  - Classification tree
    - 分类树
  - Logistic regression
    - 逻辑回归
  - SVM
    - SVM
- Question: Is the choice of the classifier important?
    - 问题：分类器的选择重要吗？ 
  - Answer: Not really  
    - 答案：并不重要

![](./images/Screenshot%202024-05-02%20100644.png)

### Summary: Edge/Counter detection 
- Early research
    - 早期研究
    - Early approaches to contour detection aim at quantifying the presence of a boundary at a given image location through local measurements.
        - 早期的轮廓检测方法旨在通过局部测量来量化给定图像位置的边界的存在。 
    - **Roberts, Sobel, and Prewitt operators** detect edges by convolving a grayscale image with local derivative filters.
        - **Roberts、Sobel 和 Prewitt 算子**通过将灰度图像与局部导数滤波器进行卷积来检测边缘。 
    - **Marr and Hildreth** use zero-crossings of the Laplacian of Gaussian operator (scale).
        - **Marr 和 Hildreth** 使用高斯算子的拉普拉斯算子 (尺度) 的零交叉。 
    - **The Canny detector** also models edges as sharp discontinuities in the brightness channel, adding non-maximum suppression and hysteresis thresholding steps.
        - **Canny 检测器** 还将边缘建模为亮度通道中的尖锐不连续性，并添加非最大抑制和滞后阈值步骤。 
    - A richer description can be obtained by considering the response of the image to a family of filters of different scales and orientations. An example is the **Oriented Energy approach**. Also developed were filter-based methods with an automatic scale selection mechanism.
        - 通过考虑图像对不同尺度和方向的滤波器系列的响应，可以获得更丰富的描述。一个例子是**定向能量方法**。还开发了具有自动尺度选择机制的基于滤波器的方法。

### Summary: Edge/Contour detection 
- Beyond simple filters
    - 超越简单的过滤器
  - Later local approaches take into account colour and texture information and make use of learning techniques for cue combination.
    - 后期的局部方法考虑了颜色和纹理信息，并利用学习技术进行提示组合。 
  - A method that designed gradient operators for brightness, colour, and texture channels, and used them as input to a logistic regression classifier for predicting edge strength.
    - 一种为亮度、颜色和纹理通道设计梯度算子的方法，并将它们用作逻辑回归分类器的输入，以预测边缘强度。 
  - Further works combined generic and class-specific edge detectors by learning discriminative sparse representations of local image patches.
    - 进一步的研究通过学习局部图像块的判别稀疏表示，将通用和特定类的边缘检测器结合起来。

### Classical CV: Design Principles, Choices, Advantages, Disadvantages, Conditions

- **Edge/contour detection** – First-principle CV
    - **边缘/轮廓检测** – 第一性原理 CV
  - **Design principles:** sound mathematical models
    - **设计原则**：合理的数学模型
  - **Design choices:** edge models, noise models, first, second order derivatives, etc.; parameter settings
    - **设计选择**：边缘模型、噪声模型、一阶、二阶导数等；参数设置
  - **Advantages:** compact, robust and stable under assumed conditions, interpretable and quick to evaluate and train (if at all); low on data requirements, low on compute requirements
    - **优点**：在假设条件下紧凑、稳健且稳定，可解释且可快速评估和训练（如果有的话）；数据要求低，计算要求低
  - **Disadvantages:** performance is lacking
    - **缺点**：性能欠缺
  - **Conditions under which they can be applied:** clear understanding of the problem
    - **适用条件**：对问题的清晰理解

### Edge/Contour detection – Progression

- **Hand designed**
    - **手工设计**
- **Hand designed, but using ML to make some optimisation**
    - **手工设计，但使用 ML 进行一些优化**
- **Next step:** Learn detectors in an end-to-end fashion
    - **下一步：**以端到端的方式学习检测器

## From classical to deep learning-based methods in Computer Vision - Part 2

### Summary of convolutions(padding, stride)

- **n × n image**: 指的是输入图像的尺寸为n×n。
- **f × f filter**: 表示卷积核的尺寸为f×f。
- **padding p**: 指的是在图像的每一边添加p行/列像素，通常填充的是0值，用于控制输出尺寸和帮助卷积核覆盖图像边缘。
- **stride s**: 指卷积核移动的步长，即每次卷积核移动的像素数。

输出尺寸的计算公式是：

$$
\left\lfloor \frac{n + 2p - f}{s} \right\rfloor + 1 \times \left\lfloor \frac{n + 2p - f}{s} \right\rfloor + 1
$$

这个公式提供了输出特征图（feature map）的尺寸，其中：
- **n + 2p** 是添加了填充之后的图像尺寸。
- **f** 是滤波器的尺寸。
- **s** 是步长。

### Summary of Notation for Convolutional Layers

Let layer $l$ be a convolutional layer (superscript indicates the current layer):

- $f^{[l]}$ = filter size
- $p^{[l]}$ = padding (valid or same)
- $s^{[l]}$ = stride
- $n_c^{[l]}$ = number of filters

#### Input and Output Dimensions
- **Input**: $n_H^{[l-1]} \times n_W^{[l-1]} \times n_c^{[l-1]}$
- **Output**: $n_H^{[l]} \times n_W^{[l]} \times n_c^{[l]}$
  - Output dimensions are calculated as:
    $$
    n_H^{[l]} = \frac{n_H^{[l-1]} + 2p^{[l]} - f^{[l]}}{s^{[l]}} + 1
    $$
    $$
    n_W^{[l]} = \frac{n_W^{[l-1]} + 2p^{[l]} - f^{[l]}}{s^{[l]}} + 1
    $$

#### Filter and Activation Dimensions
- **Each filter**: $f^{[l]} \times f^{[l]} \times n_c^{[l-1]}$
- **Activations**: $a^{[l]}$ -> $n_H^{[l]} \times n_W^{[l]} \times n_c^{[l]}$
- **Weights**: $f^{[l]} \times f^{[l]} \times n_c^{[l-1]} \times n_c^{[l]}$
- **Bias**: $n_c^{[l]}$

#### Channel and Filter Size Considerations
- **Where does the number of channels come from?**
  - Number of channels in the output volume equals the number of filters $n_c^{[l]}$.
- **How about the size of each filter?**
  - Number of channels must be equal to the number of channels in the input layer.

### Summary of Pooling

**Hyperparameters:**
- $f$: filter size
- $s$: stride
- Max or average pooling

**Output dimensions calculation:**
$$
n_H = \left\lfloor \frac{n_H - f}{s} + 1 \right\rfloor \times \left\lfloor \frac{n_W - f}{s} + 1 \right\rfloor \times n_c
$$

**Note:**
- No parameters to learn during pooling.