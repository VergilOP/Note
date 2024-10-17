# Note - Robitcs - Planning and Motion

## Lecture 1 - Introduction to Robotic

### Learning Objectives

Objectives:
1. Definition of robotics & it's history  
    机器人定义以及历史
2. Current robotic sectors  
    当前的机器人领域
3. Industrial robotics & manipulators  
    工业机器人和操纵者

### Robot Definition

Two common definitions are:
- (old)An industrial robot is a reprogrammable, multifunctional manipulator designed to move parts, tools or special devices through variable programmed motions for the performance of a variety of tasks  
    （旧）工业机器人是一种可重新编程的多功能操纵器，旨在通过可变的编程动作移动零件、工具或特殊设备，以执行各种任务
- A robot is an artificial physical agent that perceives its environment through `sensors` and acts upon that environment through `actuators`.  
    机器人是一种人工物理代理，它通过传感器感知其环境并通过执行器对该环境采取行动。

### Current Robotic Application Sectors

- Manufacturing  制造业
- Surgical  外科
- Service  服务业
- Military  军事
- Healthcare  医疗保健
- Home  家居
- Space  太空
- Farming  农业
- Security/surveillance  安全/监视
- Rescue  救援
- Extreme Environments  极端环境

## Lecture 2 - Actuators & Sensors

### Learning Objectives

Objectives:
- Different Types of Actuators  不同类型的执行器
- Sensors  传感器

### Actuators

Three commonly used actuator types:
- Electromagnetic(The most common types of actuators)  电磁
- Hydraulic  液压
- Pneumatic  气动

#### Electromagnetic Actuators  电磁执行器

- Brushed DC Motor  有刷直流电动机
  - Current flowing through armature generates a magnetic field and permanent magnets torque the armature  
    通过电枢的电流产生磁场，永久磁铁扭转电枢 
    - Advantages: Provicdes variable speeds, low-cost  
        优点：提供可变速度，成本低
    - Disadvantages: Brush wear out, low precision  
        缺点：刷子磨损，精度低

- Brushless DC Motor  无刷直流电动机
  - Armature is fixed, and permanent magnets rotate    
    电枢固定，永久磁铁旋转
    - Advantages: Efficiency, Low noise, Cooling, Water-resistant  
        优点：效率高、噪音低、散热、耐水
    - Disadvantages: low percision, costly  
        缺点：精度低，成本高

- Stepper Motor  步进电动机
  - Brushless, synchronous motor that moves in discrete steps  
    无刷、同步电机，以离散步进运动
    - Advantage: Precise, quantized control without feedback  优点：精确、量化控制，无需反馈
    - Disadvantages: Slow and moves in discrete steps, expensive  缺点：速度慢，以离散步进移动，成本高

#### Hydraulic Actuators  液压执行器

- Cylinders(linear actuators):  气缸（线性执行器）
  - Advantages:
    - Very powerful that offer very large force capability, but expensive  非常强大，提供极大的力输出，但成本高
    - High power-to-weight ratio  功率与重量比高
  - Drawbacks:
    - Their power supplies are bulky and heavy  电源体积大且沉重
    - Oil leakage  漏油问题

- Motors(rotary actuators)  马达（旋转执行器）

- Integrated Smart Hydraulic Actuator  集成智能液压执行器
  - Usual hydraulic actuator-valve configuration  常见的液压执行器-阀门配置

#### Pneumatic Actuators  气动执行器

- Cylinders(linear actuators)  气缸（线性执行器）

- Motors(rotary actuators)  马达（旋转执行器）

### Sensors

#### Motivation

A robot would be easily controlled if a complete model of the environment was available for the robot, and if tis actuators could execute motion commands perfectly relative to this model  
如果机器人拥有完整的环境模型，并且其执行器能够相对于该模型完美执行运动命令，则机器人将更容易控制。

#### Robotic sensor classsification

- Proprioceptive  本体感知
  - Internal state of the robot  机器人的内部状态
  - Measures values (e.g. wheels position, joint angle, battery level, etc)
- Exteroceptive  外感知
  - External state of the system  系统的外部状态
  - Observing environment, detecting objects, etc

- Active  主动
  - Emits energy(e.g. radar)
- Passive  被动
  - Receives energy(e.g. camera)

- Real-world Characteristics of sensors
  - **Sensitivity**: Ratio of output change to input change  
    灵敏度：输出变化与输入变化的比率
  - **Error/Accuracy**: Difference between the sensor's output and the true value  
    误差/准确度：传感器输出与真实值之间的差异
    - **Systematic/Deterministic Error**: Caused by factors that can be modelled(in theory), e.g., calibration of a laser sensor  
        系统/确定性误差：由可建模的因素引起（理论上），如激光传感器的校准
    - **Random Error**: e.g., hue instability of camera, black level noise of camera  
        随机误差：如相机色调不稳定、相机的黑电平噪声
  - **Reproducibility**: Reproducibility of sensor results  
    再现性：传感器结果的可重复性

#### Various sensors overview

- A simple On/Off switch
- Titl sensor(mercury titl)  倾斜传感器（汞倾斜）
- Dual axis inclinometer  双轴倾斜仪
- Potentiometer  电位器
- Bumpers  缓冲器
  - Mechanical switches

- Light sensors
  - Photoresistors, light dependent resistors(LDR)
  - Phototransistors  光电晶体管

- Thermal sensor
  - Thermal resistor
  - Temperature sensors
    - Analogue
    - Digital

- Proximity sensors  接近传感器
  - Non-contact
  - Devices that can be used in areas that are near to an object to be sensed
  - Different types of Proximity Sensors
    - Infrared
    - Ultrasonic
    - Inductive  电感
    - Capacitive  电容

- Position Sensors
  - Potentiometer  电位器
  - Resolver  解算器
  - Optical Encoders
    - Relative position
    - Absolue position

- Heading sensors:  方位传感器
  - Heading sensors can be proprioceptive(gyroscope, inclinometer) or exteroceptive(compass)  
    方位传感器可以是本体感知（陀螺仪、倾角仪）或外感知（指南针）
  - Used to determine the robots orientation and inclination  
    用于确定机器人的方位和倾斜角

- Accelerometer
  - be made to sense acceleration by simply measuring the force on a mass

- Gyroscope  陀螺仪
  - Heading seonsors for measuring and to keep the orientation to a fixed frame  
    用于测量和保持相对于固定框架的方向的方位传感器
  - Two methods:
    - Mechanical(flywheel)
    - Electronic

### Components used for Manipulators
- Components in a joint:
  - Moters(electric or hydraulic)
  - Moter Encoders
    - Angle(joint angle)
    - Displacement sensor  位移传感器
  - Gearbox  齿轮箱

## Lecture 3 - Manipulators

### Learning Objectives

Objectives:
1. Introduction to Manipulators
2. Manipulators and joints

### Robotic Manipulators 机械臂

Benefits in repetitive operation:
- Increase volume / capacity 增加容量
- Improve quality and consistency 改进质量
- Untouched by human hand 不能人手触碰
- Reduce wastage 减少浪费
- "Up skilling" of work force 技能提升

> A Return On Investment(ROI 回报率) study would be performed to quatify these factors and justify the investment in a bespoke robotics solution

### joints 关节

- Different types of joints
  - Revolute Joint 旋转关节
    - 绕固定轴旋转，自由度(DOF)为1
  - Prismatic Joint 伸缩关节
    - 可以沿直线滑动，自由度(DOF)为1
  - Cylindrical Joint 圆柱关节
  - Spherical Joint 球形关节
  - Universal Joint 万向关节

### Manipulators 机械臂

- Different types of manipulator:
  - Cartesian PPP 笛卡尔型
    - 三个线性关节，适合直线运动
  - Cylindrical RPP 圆柱型
    - 适合具有圆柱形工作空间的任务
  - Spherical RRP 球型
    - 适合球型工作空间
  - Articulated RRR 关节型
    - 更加灵活，常用于需要复杂运动的任务中
  - SCARA, RRP (Selective Compliance Assembly Robot Arm 选择顺应性装配机械手臂)

- Links
  - n moving link(s) n个活动连杆
  - 1 fixed link 固定基座
- joints
  - Revolute (1 DOF)
  - Prismatic (1 DOF)

- Position Parameters 位置参数
  - Position parameters describe the full configuration of the system

n links -> 9n parameters (3 vectors: Each vector has 3 parameters)

**Generalised coordinates**: A set of independent configuration parameters
**Degreee of Freedom**: Number of generalised coordinates

- We need 6 DOF to have access to all space
  - 3 DOF: Position 位置
  - 3 DOF: Orientation 姿态

> Revolute and prismatic joints have 1 DOF

- Generalised coordinates 广义坐标
  - A set of independent configuration parameters 独立参数
  - Each rigid body(刚体) needs 6 parameters to be described
    - 3 positions
    - 3 orientations
  - For n rigid body, we need 6n parameters
  - Constrains myst be applied:
    - Each joint has 1DOF, so 5 constrains will be introduced
    > n moving links -> 6n parameters  
    > n joints -> 5n constrains  
    > n DOF  
    > This is for manipulator with fixed base

- End effectors configuration 末端执行器配置
  - End effector is the last rigid-body and it has all the freedom from previous links
  - A set of parameters describing position and orientation of the end effector: $(x_1, x_2, x_3, ... , x_m)$ with respect to {0}
    > $O_{n+1}$: is operational coordinates(task coordinates)
  - A set of $x_1, x_2, x_3, ... , x_{m_o}$ of $m_o$ independent configuration parameters
  - $m_o$ is number of DOF of the end effector, max 6 DOF 末端执行器自由度最高为6

- End effector, Joint coordination 末端执行器，关节坐标
  - Joint space (configuration space) is the space that a manipulator is represented as a point.
  - (x,y) is a vector for position of end effector $\alpha$ defines orientation(angle) of end effector
  - Defines: operational coordinates -> operational space

- Redundancy 冗余
  - A manipulator is Redundant if 
    $$
      n>m 
    $$
    n number of DOF of the manipulator  
    m number of DOF of the end effector(operational space)  
    Degreee of Redundancy: n - m  

## Lecture 4 - Kinematics

### Learning Objectives

Objectives:
1. Spatial Description
2. Transformation
  - Rotation
  - Translation

### Spatial Description

- Position of a Point 点的位置
  - With respect to a fixed origin O, the position of a point P is described by the vector OP(p)  
    相对于固定原点 O，点 P 的位置由向量 OP(p) 描述

- Coordinate Frames:
  - Rotation
  - Translation

- Rigid body configuration:
  - Position: $^AP$
  - Orientation: {^AX_B, ^AY_B, ^AZ_B}

> These vectors describe rotation of {B} with respect to {A}

### Transformation

#### Rotation

- Rotation Matrix:
  $$
    ^A_BR =  \begin{bmatrix}
    r_{11} & r_{12} & r_{13} \\
    r_{21} & r_{22} & r_{23} \\
    r_{31} & r_{32} & r_{33}
    \end{bmatrix} 
    = \begin{bmatrix} ^A \hat{X}_B & ^A \hat{Y}_B & ^A \hat{Z}_B \end{bmatrix}
    = \begin{bmatrix} ^A \hat{X}_B & ^A \hat{Y}_B & ^A \hat{Z}_B \end{bmatrix} 
    = \begin{bmatrix} {^B \hat{X}_A}^T \\ {^B \hat{Y}_A}^T \\ {^B \hat{Z}_A}^T \end{bmatrix} = {^B_A R}^T
  $$
  > - Inverse of Rotation Matrix(Orthonormal Matrix)
  >   $$
  >     ^A_BR^{-1} =\ ^B_AR =\ ^A_BR^T
  >   $$

- State description: $^A\hat{X}_B = ^A_BR\ \ ^B\hat{X}_B$

$$
  ^A \hat{X}_B = {^A_B R} \begin{bmatrix} 1 \\ 0 \\ 0 \end{bmatrix}\\
  ^A \hat{Y}_B = {^A_B R} \begin{bmatrix} 0 \\ 1 \\ 0 \end{bmatrix}\\
  ^A \hat{Z}_B = {^A_B R} \begin{bmatrix} 0 \\ 0 \\ 1 \end{bmatrix}
$$

- Dot product:
  $$
    ^A \hat{X}_B 
    = \begin{bmatrix} \hat{X}_B \cdot \hat{X}_A \\ \hat{X}_B \cdot \hat{Y}_A \\ \hat{X}_B \cdot \hat{Z}_A \end{bmatrix}\\
    ^A \hat{Y}_B 
    = \begin{bmatrix} \hat{Y}_B \cdot \hat{X}_A \\ \hat{Y}_B \cdot \hat{Y}_A \\ \hat{Y}_B \cdot \hat{Z}_A \end{bmatrix}\\
    ^A \hat{Z}_B 
    = \begin{bmatrix} \hat{Z}_B \cdot \hat{X}_A \\ \hat{Z}_B \cdot \hat{Y}_A \\ \hat{Z}_B \cdot \hat{Z}_A \end{bmatrix}
  $$

