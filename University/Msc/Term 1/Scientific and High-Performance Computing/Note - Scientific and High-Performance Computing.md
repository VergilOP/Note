# Note - Scientific and High-Performance Computing

## Lecture 1 - Radioactive decay

### Mathematical model & analytical solution

- Constant fraction of atoms decays per unit time
    $$
    \frac{dN}{dt} = - \frac{N}{\tau} \equiv f(N, t)
    $$
    > $\tau$ is called `mean life-time`  

- Analytical solution: 
    $$
        N(t) = N_0 \exp(-\frac{t}{\tau})
    $$

    > $N_0$: number of radio-active atoms at t = 0  

    > $N(t) = \frac{N_0}{2}$ for $\exp(-\frac{t}{\tau}) = \frac{1}{2}$, so `half life-time` $T_{\frac{1}{2}} = \tau\ln(2)$

### Numerical solution: Euler's method

- Basic idea: Replace continuous time $t$ by discrete times $t_i$ ($i \in N$)
    $$
        \frac{N(t + \Delta t) - N(t)}{\Delta t} = - \frac{N(t)}{\tau}
    $$

$$
    \frac{dx}{dt} = f(x,t) \rarr \frac{x(t + \Delta t) - x(t)}{\Delta t} = f(x(t), t)
$$

### Euler's method

#### choosing the step size

- $N_{i+1} = (1 - \frac{\Delta t}{\tau})$
  1. Requires $\Delta t < \tau, otherwise $N_{i+1} < 0$ (method is not unconditionally stable)
  2. Accuracy improves with decreasing $\frac{\Delta t}{\tau}$
  3. Taking $\Delta t$ constant gives constant relative error per step
- In general: $f(x) \rarr f(x,t)$

#### error estimate

- Taylor expansion
    $$
        x(t + \Delta t) = x(t) + \frac{dx(t)}{dt}\frac{(\Delta t)^1}{1!} + \frac{d^2x(t)}{dt^2}\frac{(\Delta t)^2}{2!} + \dots
    $$

- Error Estimate:
  - Euler method uses `first two terms`, but `ignores all others starting at the third one.`
  - Error per step: $O[(\Delta t)^2]$
- Overall Error:
  - We got $\frac{1}{\Delta t}$ steps from $t_0$ to $t_{end}$
  - Overall error: $O[(\Delta t)]$
  - Take $\Delta t$ small enough: compare to time-scale in the problem($\Delta t \ll \tau$)

### A change of variables

- Original equation: $\frac{dN}{dt} = - \frac{N}{\tau}$
- Change of variables: $x = \ln(\frac{N}{N_0})$
  $$
    \frac{dx}{dt} = -\frac{1}{\tau}
  $$
- Even better: $t \rarr t' \equiv \frac{t}{\tau}$
  $$
    \frac{dx}{dt'} = -1
  $$

> Advantages:
> - Simplify differential equation
> - Stablility of the equation
> - No limit on time-step

## Lecture 2 - Projectile motion

### Mathematical model & analytical solution

- Newtonian dynamis:
  $$
    \frac{d^2x}{dt^2} = 0; \frac{d^2y}{dt^2} = -g
  $$
- Analytical solution:
  $$
    x = x_0 + \dot{x_0}t; t = y_0 + \dot{y_0}t - \frac{g}{2}t^2
  $$
  >  $(x_0, y_0)$ is initial position, $(\dot{x_0},\dot{y_0})$ is initial velocity in x and y direction
- In terms of launch angle, $\theta_0$, and launch speed, $v_0$
  $$
    \dot{x_0} = v_0\cos(\theta_0); \dot{y_0} = v_0\sin(\theta_0)
  $$

### Numerical solution: Euler's method

- Solution for differential equations of the type
  $$
    \frac{dx}{dt} = f(x, t)
  $$
- Discretise time t an coordinate x with time-step $\delta t$:
  $$
    x(t^{n+1}) \equiv x^{n+1} = x^n + f(x^n, t^n)\delta t
  $$

> Euler's method not directly applicable, because equations are second order  
> So **REWRITE** as two
> $$
>   \frac{dy}{dt} = v_y; \frac{dv_y}{dt} = f_y
> $$

- Initial conditions: Lanuch angle $\theta_0$, launch speed $v_0$
  $$
    (x_0, y_0) = (0,0), (v_{x,0}, v_{y,0}) = v_0(cos(\theta_0), sin(\theta_0))
  $$
- Euler's methods:
  $$
    x(t^{n+1}) \equiv x^{n+1} = x^n + v^n_x \Delta t; v^{n+1}_x = v_x^n + 0 \Delta t  \\
    y(t^{n+1}) \equiv y^{n+1} = y^n + v^n_y \Delta t; v^{n+1}_y = v_y^n - g \Delta t  \\
    t^{n+1} = t^n + \Delta t
  $$

### Air resistance: mathematical model

#### Drag force

- Drag force:
  $$
    F_{drag} = -B_{1,drag}v\frac{\bold{v}}{v} - -B_{2,drag}v^2\frac{\bold{v}}{v}
  $$
- drag froce is parallel to velocity, $F || v$  
  $\frac{\bold{v}}{v}$ is unit vector in the direction of motion
- drag coefficients $B_{1,drag} > 0$ and $B_{2,drag} > 0$ since drag slows projectile down

- Dimensional analysis: $|F_{drag}|$ depends on density of air $(\rho)$, speed $(v)$, and size of projectile $(r)$: 
  $$F_{drag} \propto \rho^\alpha v^\beta r^\gamma$$
  - As F is $kg\ m\ s^{-2}$, so $\alpha = 1;\beta = 2; \gamma = 2$
- Therefore take
  $$
    F_{drag} \approx -B_{2,drag}v^2\frac{\bold{v}}{v} = -B_{2,drag}v\left
    (\begin{array}{cc} 
    v_x\\ 
    v_y
    \end{array}\right)
  $$

#### Air resistance: Numerical solution

- Mathematical model:
  $$
    \frac{d^2x}{dt^2} = -\frac{B(y)vv_x}{m}, \frac{d^2y}{dt^2} = - g -\frac{B(y)vv_y}{m}
  $$
- Euler's method
  $$
    x^{n+1} = x^n + v^n_x\Delta t; v^{n+1}_x = v^n_x - \frac{B(y^n)v^nv^n_x}{m}\Delta t \\
    y^{n+1} = y^n + v^n_y\Delta t; v^{n+1}_y = v^n_y - g\Delta t - \frac{B(y^n)v^nv^n_y}{m}\Delta t \\
    t^{n+1} = t^n + \Delta t \\
    v^n = ((v^n_x)^2 + (v^n_y)^2)^{\frac{1}{2}}
  $$

### Higher-order methods

- Euler method simple to implement, but correct only to $O(\Delta t)$
- According to the `mean value theorem`:
  $$
    \exist t' \in [t, t + \Delta t] : x(t + \Delta t) \equiv x(t) + \frac{dx}{dt} \bigg|_{t = t'} \Delta t
  $$

> - Advantage: Here t' includes higher order effects(curvature etc)
> - Drawback: Not known generally, but maybe better choices than $t' = t$ employed in Euler method

#### $2^{nd}$ order Runge-Kutta

- Underlying idea: Estimate $t' = t + \frac{\Delta t}{2}$
- But: also need $\frac{dx}{dt}$ at $t = t'$
  $$
    x' = x + f(x,t)\frac{\Delta t}{2}
  $$
- Second-order scheme(precision $O[(\Delta t)^2]$):
  $$
    x' = x + f(x,t)\frac{\Delta t}{2} \\
    x(t + \Delta t) = x(t) + f(x', t') \Delta t\\ 
    x^{n+1} = x^n + f(x^n + \frac{\Delta t}{2}f(x^n,t^n), t^n + \frac{\Delta t}{2})\Delta t
  $$

#### $4^{th}$ order Runge-Kutta

- More sampling points
  $$
    x(t + \Delta t) = x(t) + \frac{\Delta t}{6}[f(x'_1,t'_1) + 2f(x'_2,t'_2) + 2f(x'_3,t'_3) + f(x'_4,t'_4)]
  $$
- Sampling points given by
    $$
    x'_1 = x \quad t'_1 = t
    $$
    $$
    x'_2 = x + f(x'_1, t'_1) \frac{\Delta t}{2} \quad t'_2 = t + \frac{\Delta t}{2}
    $$
    $$
    x'_3 = x + f(x'_2, t'_2) \frac{\Delta t}{2} \quad t'_3 = t + \frac{\Delta t}{2}
    $$
    $$
    x'_4 = x + f(x'_3, t'_3) \Delta t \quad t'_4 = t + \Delta t
    $$
- Fourth-order scheme(precision $O[(\Delta t)^4]$)

## Lecture 3 - Harmonic Motion

### Mathematical model & analytic solution

- Force is proportional to displacement
  $$
    m\frac{d^2x}{dt^2} = -kx
  $$
  > $k$ is a constant, m is mass of object  `k 是一个常数，表示恢复力的刚度系数（例如弹簧常数）`  
  > $k > 0$: minus sign result in a restoring force(osillations)   `恢复力总是指向平衡位置`

  $2^{nd}$ order DE: need to specify $x(t = 0) = x_0, \dot{x}(t = 0) = \dot{x}_0$

- Analytical solution
  $$
    x(t) = A\cos(\Omega t) + B\sin(\Omega t); \Omega^2 = \frac{k}{m}\\
    x_0 = A; \dot{x_0} = \Omega B
  $$
  Initial conditions determine A and B
  > $\Omega$是角频率，rad/s(弧度每秒)
  > $$
  >   T = \frac{2\pi}{\Omega}\\
  >   f = \frac{\Omega}{2\pi}
  > $$

#### Example of harmonic motion: pendulum 摆

- Pendulum bob
  - mass `m`
  - length `l`
  - deflection angle from vertical $\theta$

- $F_\theta = - mg\sin\theta \approx - mg\theta$
  > in the `small angle approximation` 小角近似

- Apply Newton's law:
  $$
    m\ddot{r} = ml\ddot{\theta} = -mg\theta; \ddot{\theta} = -\frac{g}{l}\theta\\
    m\ddot{x} = -kx; x = \theta \& \frac{k}{m} = \frac{g}{l} = \Omega^2
  $$
  
---

- Analytical solution(small angles): $\theta(t) = A\cos(\Omega t) + B\sin(\Omega t)$
- Angular eigen-frequency: $\Omega = \sqrt{\frac{g}{l}}$
- Choose initial conditions:
  - Maximal amplitude: $\theta = \theta_0$ when $t = 0 \rarr A = \theta_0$
  - Angular velocity: $\omega \equiv \dot{\theta} = 0$ when $t = 0 \rarr B = 0$
- Energy E of pendulum is conserved:
  $$
    E = \frac{1}{2}ml^2\omega^2 + mgl(1 - \cos\theta) \approx \frac{1}{2}ml^2\omega^2 + \frac{1}{2}mgl\theta^2\\
    \dot{E} = ml\omega(l\dot{\omega} + g\theta) = 0; \text{since } \dot{\omega} = \ddot{\theta} = -\frac{g}{l}\theta
  $$
  > $1 - \cos(\theta) \approx \frac{\theta^2}{2}$ in the small angle approximation

> 这里所有的导数皆是比$dt$

### Numerical solution: Euler's method

- Replace $2^{nd}$ order DE by two $1^{st}$ order DEs
  $$
    \frac{d^2\theta}{dt^2} = -\frac{g}{l}\theta \rarr \frac{d\theta}{dt} = \omega;\ \frac{d\omega}{dt} = -\frac{g}{l}\theta
  $$
- Discretise: $dt \rarr \Delta t$
  $$
    \theta^{n + 1} = \theta^n + \omega^n\Delta t\\
    \omega^{n + 1} = \omega^n - \frac{g}{l}\theta^n\Delta t\\
    t^{n + 1} = t^n + \Delta t
  $$
- Choose time-step to be small compared to period:
  $$
    \Delta \ll \frac{2\pi}{\Omega}
  $$

> - Problem: Amplitude increases with time(even for small $\Delta t$)
>
> Why does it fail?
> - Increasing amplitude implies energy of numerical solution increases
> - Evaluate numerical energy:
>   $$
>     E^{n + 1} = \frac{ml^2}{2}[(\omega^{n + 1})^2 + \frac{g}{l}(\theta^{n+1})^2]\\
>     = \frac{ml^2}{2}[(\omega^{n} - \frac{g}{l}\theta^n\Delta t)^2 + \frac{g}{l}(\theta^{n} + \omega^n \theta)^2]\\
>     = E^{n} + \frac{mgl}{2}(\frac{g}{l}(\theta^n)^2 + (\omega^n)^2)\Delta t^2\\
>     > E^n
>   $$
>   for any choice of time-step
> - Numerical scheme does not conserve energy!
>
> - Euler method not good for harmonic motion
> - Why was it good before? Was energy conserved applying Euler's method to ballistic motion?
>   - Remember the trajectory of the cannon ball: For larger step-size higher peak in trajectory than for smaller step-size(with roughly the same range)  
>     在以前处理抛体运动（如炮弹运动）时，欧拉法表现得还算可以。虽然在较大的步长下，弹道轨迹的峰值较高，但总的射程保持相对准确。因此在单次抛射的情况中，能量守恒的误差影响较小，但在多次周期振荡（如简谐运动）中，误差会随着时间的推移累积，导致能量越来越大。
> - In practise: only calculate parabolic trajectory(cannon ball) compared to many oscillations(harmonic motion)
>   - Euler's method OK for trajectories - but not for harmonic motion
> - There is no single method that is perfect for all problems

### Improving the Euler method: Euler-Cromer

- Obvious solution: use Runge-Kutta instead
- However, consider following small change to Euler's method:
  - Instead of
    $$
      \omega^{n + 1} = \omega^{n} - \frac{g}{l}\theta^n\Delta t \text{ and } \theta^{n + 1} = \theta^n + \omega^{n}\Delta t
    $$
  - use
    $$
      \omega^{n + 1} = \omega^{n} - \frac{g}{l}\theta^n\Delta t \text{ and } \theta^{n + 1} = \theta^n + \omega^{n + 1}\Delta t
    $$
  - That is: use new value of $\omega$ to update $\theta$

#### Results with Euler-Cromer

- Amplitude does not increase rapidly, even if $\Delta t$ is no very small!

### Damping: mathematical model 阻尼模型
- Damping slows down the pendulum bob: 阻尼使摆在运动时逐渐减速
  $$
    \ddot{\theta} = - \Omega^2\theta \rarr \ddot{\theta} = -\Omega^2\theta - q\dot{\theta}; q > 0
  $$
- Form of analytical solutoin depends on value of q
  1. **Under-damped regime**: amplitude decays exponentially 欠阻尼情况，振幅逐渐衰减
    $$
      \theta(t) = \theta_0\exp(-\frac{qt}{2})\sin(\sqrt{\Omega^2 - \frac{q^2}{4}} · t + \phi)
    $$
  2. **Over-damped regime**: no oscillations 过阻尼情况，不在振荡
    $$
      \theta(t) = \theta_0\exp[-(\frac{q}{2} + \sqrt{\frac{q^2}{4 - \omega}} · t)]
    $$
  3. **Critically damped regime**: Pendulum "crawls" to 0 临界阻尼情况，迅速衰减到平衡
    $$
      \theta(t) = (\theta_0 + C_t)\exp(-\frac{qt}{2})
    $$

- Amplitude decreases with time

- Add a time-varying force
  $$
    \ddot{\theta} = - \Omega^2\theta - q\dot{\theta} \text{ without driving force}
    \ddot{\theta} = - \Omega^2\theta - q\dot{\theta} + F_d\sin(\Omega_Dt) \text{ driving force}
  $$
  > strictly speaking, $F_D$ is an acceleration, not a force - we will still call it force  
  > driving force has amplitude $F_D > 0$ and varies sinusoidally with constant frequency $\Omega_D$
- Driving increases energy of the system.  
  After initial transient:
  - Frequency changes $\Omega \rarr \Omega_d$
  - amplitude changes  
  Analytical solution
  $$
    \theta(t) = \theta_{\max}\sin(\Omega_D t + \phi)\\
    \theta_{\max} = \frac{F_D}{\sqrt{(\Omega^2 - \Omega^2_D)^2 + (q\Omega_D)^2}}
  $$

### Real oscillator: adding non-linearity 增加非线性

- So far assumed amplitude is small: not always a good approximation
- For the description of a more realistic pendulum, we reinstate the non-linearity, and we will use $\sin\theta$
- This will have interesting consequences:
  - In the non-driven, non-dissipative pendulum, the eigen-frequency depends on the amplitude  
    在非驱动、非耗散钟摆中，特征频率取决于振幅
  - Driving force leads to chaotic motion  
    驱动力导致混沌运动

## Lecture 4 - Chaos

### Driven, non-linear pendulum, with dissipation

- Add drivin force and dissipation
  $$
    \ddot{\theta} = - \frac{g}{l}\sin\theta + F_D\sin(\Omega_D t) - q\dot{\theta}
  $$
- Numerical solution: **Euler-Cromer method**
- $\theta$ coordinate has 'periodic boundary' conditions
  > meaning $\theta = \pi$ is the same position of pendulum as $\theta = -\pi$ for example  
  > may lead to 'jumps' in a plot of $\theta(t)$ vs time $t$
- When $F_D = 0$ but $q > 0$: amplitude decreases with time
- When $F_D > 0: Different regimes$:
  - pendulum **in resonance** with driving force  可能处于共振
    > frequency is $\Omega_D$, amplitude may increase
  - $\theta(t)$ plot may appear **chaotic** 混沌

- $\theta(t)$ for $F_D = 0.5$ is periodic but $F_D = 1.2$ is **chaotic**
  > **no apparent periodicity**, even at much later times: we call this choas  无明显的周期性
- This 'chaotic' is in:
  - $\theta(t)$ appears to be 'unpredictable' 不可预测
    > no obvious pattern emerges 没有明显的模式
  - Yet solution is determined uniquely by the DE and its initial condition
- Example of **deterministic chaos**:  确定性混沌
  - Small differences in initial conditions get amplified
    $$
      |\theta_1(t = 0) - \theta_2(t = 0)| < \epsilon \rarr |\theta_1(t) - \theta_2(t)| \gg \epsilon
    $$
  > 即便两个系统的初始条件差异极小，随着时间推移，这种微小差异会被放大，最终导致两者的表现完全不同。

### Chaos: dependence on initial conditions

- Compare evolution for small change in initial value $\theta_0 = 0.2 \pm \Delta\theta_0$
  > $F_D = 0.5$: small differences in ICs stay small  
  > $F_D = 1.2$: small differences in ICs amplify rapidly


- Vary start condition(initial displacement $\theta_0 = 0.2$)
- Compute evolution of two identical pendulums, differing by $\Delta\theta_0 = O(0.0001)$
- Plot difference $\Delta\theta = |\theta^{(1)} - \theta^{(2)}|$ as function of time
- Findings:
  - For $F_D = 0.5$ dampening dominates and $|\Delta\theta|$ decreases
  - For $F_D = 1.2$ $\Delta\theta$ increases (up to max = $\pi$)  
  In both cases for t small: $|\Delta\theta| \sim e^{\lambda t}$
  > $\lambda < 0$ not chatic, $\lambda > 0$: chaotic
- $\lambda$ is called **Lyapunov exponent**  Lyapunov指数
  > Simple test: $\lambda > 0 \rArr \text{chaotic}$, $\lambda < 0 \rArr \text{not chaotic}$  
- Definition of **deterministic chaos**:  确定性混沌   
  System shows deterministic chaos, if its evolution depends sensitively on the initial conditions  
  演化轨迹高度依赖于初始条件

### Visualising chaos: Phase space

> Position-velocity space aka 'phase space'. For pendulum: $\theta, \omega = \dot{\theta}$ space  相空间(相图)

- Small driving force
  - Transient time at beginning: eigen-frequency decays  特征频率衰减
  - pendulum quickly settles into a regular orbit  经过暂态后，会迅速进入到一个**规律的轨道**
  - shape of $\omega(\theta)$ curve is independent of initial conditions(in agreement with $\lambda < 0$)  曲线形状稳定，与系统的初始条件无关
- Large driving force
  - Expectation of no structure in this panel is not true 预期相图中的轨迹会变得毫无规律
    > Notice that there is no maximum $\theta$  无最大摆角$\theta$的限制
  - Surprise: recognizable orbits, even though chaotic  仍能存在一些可识别的轨道
    > but a given orbit is traversed only a few times  但是这些轨道会被摆动几次后就改变
  - Examine phase space by plotting its **Poincare section**  Poincaré截面  
    - Plot $\omega$ vs $\theta$ but only when $\Omega t = 2n\pi$, with $n \in N$   
    - 只在$\Omega t = 2n\pi$时 记录$\omega$ vs $\theta$
    > meaning: plot position inphase space when the driving term is zero but increasing  
    含义：当驱动项为零但增加时，绘制相空间中的位置

### Chaos: Poincare section

- value of $\omega(\theta)$ when driving force is zero: Poincare section
- non-chaotic: just two points(shown as red dots)
- chaotic: a curve called strange attractor

#### Chaos: Strange attractors

- Poincare section is very different for chaotic versus non-chaotic motion  Poincare截面
  - non-chaotic: just a few points  只有少数几个点  
    original motion at eigen frequency, the driven motion with $\Omega = \Omega_d$  规律性的周期运动
  - chaotic: a fuzzy surface  模糊表面
    - 'Fuzziness' is not due to numerics 模糊表面不是由数值计算误差引起的。
    - Shape of surface is largely independent of initial conditions
      > important: implies that the Poincare section is a good way to examine deterministic chaos  表面的形状与初始条件无关
    - A fractal structure  分形结构(无论放大多少倍，局部结构和整体相似)

#### Chaos: Period Doubling

- What happens to solution when $F_D = 0.5 \rarr 1.2$?
  > non-chaotic $\rarr$ chaotic
- Answer: Not only one, but a chain of transitions: Hard to study  周期加倍
- Therefore: look for $F_D \in [1.3, 1.48]$ (fix $\Omega_D = 3\pi$)
  > nature of the transition is clearer for this choice of $F_D$
- In this region, the **period starts doubling**!
  > periodic motion with frequencies $\frac{\Omega_D}{2}$, $\frac{\Omega}{4}$ etc.
- Typically the opposite happens in a **harmonic oscillator** 谐波现象
  > when harmonics appear - oscilations with period $P / 2$, $P / 3$. etc
- In the chaotic pendulum, periodicities with period 2P, 4P etc appear - **sub-harmonics** 亚谐波
  > P is the period of the driving force

![](./imgs/Period%20Doubling.png)

> Amplitude of the second maximum is smaller - true(almost periodic) cycle has a period twice that of the driving force(for $F_D = 1.44$) - and 4P(for $F_D = 1.465$)

#### Chaos: Bifurcations 分岔

- Plot $\theta$ when $\Omega_D t = 2\pi$ n with $n \in N$

![](./imgs/Bifurcations.png)

- At low $F_D$
  > At a given phase of the driving force, pendulum is at a single value of $\theta$  只有单一的$\theta$值
- At $F_D \approx 1.43$: **First period doubling**
  > two possible values for $\theta$ at a given phase of drivin, small or large amplitude oscillation 首次周期加倍
- At $F_D \approx 1.46$: **Second period doubling**
  > four possible values for $\theta$ at a given phase of driving - 4 different possible amplitudes 第二次周期加倍
- At larger $F_D$ - more and more period doublings appear
- Introduce $F_n = F_D$ for $n$th period doubling and  (Feigenbaum常数)
  $$
    \delta_n = \frac{F_n - F_{n - 1}}{F_{n+1} - F_n}
  $$ 
  In limit of $n \rarr \infin$, $\delta_n \rarr \delta_\infin \approx 4.669$
- Universal feature: $\delta_\infin$ seemingly the same for all systems  
  (所有经历周期加倍转变为混沌系统的共有特性)
  > for which where period doubling leads to chaos, see Feigenbaum's original 1987 papaer 

## Lecture 5 - Root finding & numerical integration

### Mathematical problem

Common problems in computational physics include root finding and *numerical integration*

- **Root-finding**: Find a (the) value('s) x, for which求根  
  找到使函数等于0的点
  $$
    f(x) = 0
  $$
  > usually within a range $x \in [a, b]$
- **Numerical integration**: evaluate数值积分  
  计算函数在特定范围内的面积
  $$
    I = \int^b_af(x)dx
  $$
  > for a given function f(x), where a and b are given
  Both problems can occur in more than on dimension

### Root finding
- Cooling in cosmological simulations:$\rho\frac{dT}{dt} = - \Lambda(T)\rho^2$ solved numerically(implicity) as 
  > T is temperature, t is time, rho is density, $\Lambda$ is cooling rate
  $$
    \frac{T(t+\Delta t)- T(t)}{\Delta t} = - \Lambda(T + \Delta t)\rho
  $$
  ![](./imgs/root%20finding.png)

### Numerical integration

- The energy radiated by cosmic gas
  > during an interval $\Delta t$
  $$
    \Delta E = \int^{t + \Delta t}_t \Lambda(T)\rho^2 dt
  $$

#### Newton-Raphson

> Find value X for a given function f(x), so that f(x) = 0
- Assume $x_i$ starting point for root, develop f as a Taylor series expansion close to $x_i$  
  假设起始点$x_i$是我们求解根的初始猜测
  $$
    f(x_i) + f'(x_i)(X - x_i) + \dots \approx f(x) = 0
  $$
  and solve for X
  $$
    X = x_i - \frac{f(x_i)}{f'(x_i)} \equiv x_{i+1}
  $$
  $x_{i + 1}$ is improved estimate for root  
  这个$X$是下一个改进的根的估计

- Newton-Raphson is the iterative scheme
  $$
    x_{i+1} = x_i - \frac{f(x_i)}{f'(x_i)}
  $$

- $x_{i+1} = x_i - \frac{f(x_i)}{f'(x_i)}$
- Termination criterion
  > for the iteration: $|x_{i+1} - x_i|$ is 'small enough'
- Method requies that f' be calculable 可计算的
- Method needs a guess for start of iteration 猜测一个初始值
- Which root is found if there is more than one? 无法算出多根

##### Convergence 收敛性

- To estimate the error rewrite $x_{i+1} = X + \Delta x_{i+1}$ as
  $$
    X + \Delta{x_{i+1}} = X + \Delta x_i - \frac{f(X + \Delta x_i)}{f'(X + \Delta x_i)}
  $$
- Solve for $\Delta{x_{i+1}}$ & expand last term in a Taylor series:
  $$
    \Delta x_{i+1} = \Delta x_i - \frac{f(X) + \Delta x_i f'(X) + \frac{1}{2}(\Delta x_i)^2f''(X) + \dots}{f'(X) + \Delta x_if''(X) + \dots}\\
    = \frac{f''(X)}{2f'(X)}(\Delta x_i)^2 + O[(\Delta x_i)^3]
  $$
- Error term is quadratic in $\Delta x$ $\rarr$ decreases quickly convergence rate depends on $f'$ and $f''$  
  因为误差的二次项$(\Delta x_i)^2$ 主导, 所以误差会迅速减少
  > $f'$ small and/or $f''$ large $\rarr$ convergence is slow  
  如果$f'$太小, 或者$f''$很大, 算法的收敛速度会变慢。

#### Secant method 割线法

- If $f'$ is not known, we can't apply Newton-Raphson
- Instead use **Secant method: compute $f'$ numerically** from actual and previous guesses. 数值估计导数
  $$
    f'(x_i) \approx \frac{f(x_i) - f(x_{i-1})}{x_i - x_{i-1}} + O[(x_i - x_{i-1})^2]
  $$
- Use this estimate in Newton-Raphson method
  $$
    x_{i+1} = x_i - f(x_i)\frac{x_i - x_{i-1}}{f(x_i) - f(x_{i-1})}
  $$
  > requires that we ahve two guesses to start iteration, $x_1$ and $x_2$
- Depending on smoothess of f, secant method may converge faster than Newton-Raphson. Asymptotically,  
  割线法的收敛速度依赖于函数的光滑程度。在某些情况下，割线法可能比牛顿-拉弗森法收敛的更快
  $$
    \lim_{i \rarr \infin}|\Delta x_{i+1}| \approx |\Delta x_i|^{1.618}
  $$
  > $\Delta x_i = X - x_i$. Exponent is called the 'golden ratio'. $(\sqrt{5}+1)/2 \approx 1.618$  
  割线法的渐进收敛速度为 1.618

#### Bisection 二分法

- Robust method that relies on subdividing intervals
  - Use: $f(x_0) \cdot f(x_1) < 0 \rArr \exist X \in [x_0, x_1]: f(X) = 0$  
    Provided $f$ is continuous
  - Find interval $[x_i, x_{i+1}]$ with $f(x_i)f(x_{i+1}) < 0$  
    for example $f(x_i) < 0$ but $f(x_{i+1}) > 0$
  - **Bisection**: divide interval at $x_{i+2} = \frac{x_{i} + x_{i+1}}{2}$. Replace either $x_i$ or $x_{i+1}$ with $x_{i+2}$ such that for new interval limits still one function value above and one below zero.  
    利用二分法，逐渐接近函数的零点


#### Comparison of convergence

![](./imgs/Comparition.png)

Summary
- We discussed Iterative procedures - must provide guess, and stop iteration when accuracy goal is reached. Typical condition:  
  停止条件
  $$
    |\frac{x_{i+1} - x_{i}}{x_{i+1} + x_{i}}| \leq p
  $$
  > can be absolute criterion as well, $|x_{i+1} - x_i| < q$
- Complications cases with several roots, extrema and saddle points, etc.  
  当存在多个根，极值点，和鞍点时，根查找可能变得复杂。 需要特别注意这些情况
- Newton-Raphson: fastest convergence
  > requires calculation of $f'$ 要求可以计算出导数
- Secant method: fast
  > compute $f'$ numerically 通过数值方式计算
- Bisections: Slowest convergence but very robust  
  很慢，但是robust性很强，即在各种条件下都能很好地工作
- In more than one dimension: very tricky business, would use gradient. Estimating convergence also tricky  
  在多维情况下，根查找变得更加复杂，通常需要梯度信息来处理，收敛速度也变得很慢

---

- Evaluate
  $$
    I = \int^b_a f(x) dx
  $$
  > for given integration limits a and b, and given function f

- Example: period of non-linear pendulum  非线性摆的周期
  $$
    T = \sqrt{\frac{8l}{g}}\int^{\theta_{\max}}_0\frac{d\theta}{\sqrt{\cos\theta - \cos\theta_{\max}}}
  $$
  Elliptic integral(椭圆积分), closed form `analytic` solution not known

Newton-Cotes method
- $$
    I = \int^b_a f(x) dx
  $$
- Newton-Cotes: divide interval $[a, b]$ in N subintervals of size $\Delta x = (b - a)/N$ and approximae integral by a sum  
  Newton-Cotes方法是一种通过将函数近似为小区间上的分段常数来求解积分的数值方法
  $$
    \int^b_a f(x) dx \approx \sum^{N-1}_{i=0}f(x_i)\Delta x = \sum^{N-1}_{i=0}f(a + i\Delta x)\Delta x
  $$
- Replace integration by sum over rectanglar segments
  > approximate f as being piece-wise constant when segments are 'small enough'

Convergence of Newton-Cotes 收敛性
- The **Euler-Maclaurin** summation formula is:  
  sum over integers, requires that *all* derivatives of F exist
  $$
    \sum^{N - 1}_{i = 1} F(i) = \int^N_0F(u)du - \frac{1}{2}[F(0) + F(N)] + \sum^\infin_{k = 1}\{\frac{B_{2k}}{(2k)!}[F^{(2k - 1)}(N) = F^{(2k-1)}(0)]\}
  $$
  $F^{(n)}(u) = n^{th}$ derivative of F  
  $B_{2k}$ are the **Bernoulli numbers**

- Setting $u = \frac{x-a}{\Delta x}$, $F(u) = f(x)$
  > E-M summation formula becomes
  $$
    \sum^{N-1}_{i=1}f(x_i) = \frac{1}{\Delta x}\int^b_a f(x) dx - \frac{1}{2}[f(a) + f(b)] + \sum^\infin_{k = 1}\{\frac{B_{2k}}{(2k)!}[f^{(2k-1)}(b) - f^{(2k-1)}(a)](\Delta x)^{2k-1}\}
  $$

- Rearrange and adjust the summation index:
  $$
    \int^b_a f(x) dx = \sum^{N-1}_{i=0} f(x_i) \Delta x + \frac{\Delta x}{2}[f(b) - f(a)] - \frac{(\Delta x)^2}{12}[f'(b) - f'(a)] + O[(\Delta x)^4]
  $$

#### trapezoidal rule

- Imporve convergence of N-C by including the term $\frac{[f(b) - f(a)]}{2}$
  $$
    \int^b_a f(x) dx = \sum^{N-1}_{i=0} f(x_i) \Delta x + \frac{\Delta x}{2}[f(b) - f(a)] + \dots
  $$

- Trapezoidal rule accurate upt to second order in $\Delta x$ - Newton-Cotes accurate to first order in $\Delta x$

#### Simpson's rule

- Use higher-order interpolation
  > rather than linear interpolation of trapezoidal rule
- Simpson's rule: Fit parabolic segments through the top edges of two neighbouring segments. If $A_i$ is the area of the segment between $x_i$ and $x_{i+1}$ in the parabolic fit, then
  $$
    A_i + A_{i+1} = \frac{\Delta x}{3}[f(x_i) + 4f(x_{i+1}) + f(x_{i+2})]
  $$
- This can be seen by using
  $$
    A_i + A_{i+1} = \int^{x_{i+1}}_{x_i} (ax^2 + bx + c) dx
  $$
  and the parabolic fit
  $$
    f(x_{j = i, i+1, i+2}) = ax^2_j + bx_j + c
  $$

- From the area $A_i + A_{i + 1}$ of two neighbouring segments in the parabolic fit we have
  $$
    \int^b_a f(x) dx \approx \frac{\Delta x}{3} [f(a) + 4f(x_1) + 2f(x_2) + 4f(x_3) + ... + 2f(x_{N-2}) + 4f(x_{N-1}) + f(b)]
  $$
- Convergence of Simpson's rule: $\propto (\Delta x)^4$

- Trivial test: Elliptic integral
  $$
    I=\intop_0^{\pi/2}\left(1-k^2\sin^2\theta\right)^{1/2}\mathrm{~d}\theta\xrightarrow{k\to1}1
  $$
  ![](./imgs/Error%20for%20evaluation%20of%20elliptic%20integral.png)

- Harder test - function with diverging derivative at $x = 2$:
  $$
    I=\intop_0^2\left(4-x^2\right)^{1/2}\mathrm{~d}x=\pi 
  $$
  ![](./imgs/Error%20for%20evaluation%20of%20tricky%20square-root.png)

#### Monte Carlo integration

- Example: calculating $\pi$. Compare surface area of sphere $(S = \pi r^2)$ to that of a square with length $2r (S = 4r^2)$
- Use pseudo-random number generator  
  ![](./imgs/pseudo-random%20number%20generator.png)  
  Ratio of surface of quarter circle $(S = \pi r^2 / 4)$ over that of square $(S = r^2)$ is fraction of points that land inside the circle
  ![](./imgs/Monte%20Carlo%20integration.png)

- MC integration: Estimate integral by N probes
  $$
    I=\intop_a^bf(x)\mathrm{~d}x\longrightarrow\langle I\rangle=\frac{b-a}N\sum_{i=1}^Nf(x_i)=\langle f\rangle_{a,b} ,
  $$
  where $x_i$ are `random numbers` homogeneously distributed in [a, b]
- Basic idea for error estimate: statistical sample $\rArr$ use standard deviation as error estimate
  $$
    \langle E_f(N)\rangle=\sigma=\left[\frac{\langle f^2\rangle_{a,b}-\langle f\rangle_{a,b}^2}N\right]^{1/2}
  $$
  ![](./imgs/MC-integration%20convergence.png)

#### Comparing convergence rates in numerical integration
- Interesting question: How do error estimates scale with the number of function calls?
- May become crucial, if function calls "expensive"
- Trapezium: $\sim N^{-2 / d}$, Simpson: $\sim N^{-4 / d}$, MC: $\sim N^{-1 / 2}$ for d dimensions
- Therefore: For $d \leq 8$ dimensions MC wins!
- Method of choice for high-dimensional integration

#### Summary
- When to favour higher-order over lower-order and vice versa?
  - integral needed only once: knowing accuracy important convergence
  - Integral needs evaluating many times  
    In general: smooth function $\rarr$ use higher-order method non-smooth function $\rarr$ use low-order method
- Very smooth function: use Gaussian integration

#### Application - Hyperspheres

> Hypersphere is a sphere in $d > 3$ dimensions

- Volume in spherical coordinates:
  $$
    V_d = \int^R_0 r^{d-1} dr \int d \Omega_n = \frac{R^d}{d} \int d\Omega
  $$
  R is radius of the sphere, $\int d \Omega_n$ is the 'angular bit'

##### Analytical calculation

- Transform to d-dimensional polar coordinates
  $$
    \begin{aligned}
    &x_{1}=&& r\sin\theta_1\sin\theta_2\ldots\sin\theta_{d-3}\sin\theta_{d-2}\sin\theta_{d-1} \\
    &x_{2}=&& = r\sin\theta_1\sin\theta_2\ldots\sin\theta_{d-3}\sin\theta_{d-2}\cos\theta_{d-1} \\
    &x_{3}=&& r\sin\theta_1\sin\theta_2\ldots\sin\theta_{d-3}\cos\theta_{d-2} \\
    ... \\
    &x_{d-1}&& =-r\sin\theta_1\cos\theta_1 \\
    &x_{d}=&& =-r\cos\theta_1 \\
    \end{aligned}
  $$
- Volume elements:
  $$
    \mathrm{d}V_d=\intop_0^Rr^{d-1}\mathrm{d}r\left[\prod_{i=1}^{d-2}\intop_0^\pi\sin^{d-1-i}\theta_i \mathrm{d}\theta_i\right]\intop_0^{2\pi}\mathrm{d}\theta_{d-1}
  $$

- For integral above, use(with $\beta = -\frac{1}{2}$)
  $$
    \int^\pi_0 \sin^{2\alpha + 1}(x)\cos^{2\beta+1}(x)dx = \frac{\Gamma(\alpha+1)\Gamma(\beta+1)}{\Gamma(2+\alpha+\beta)}
  $$

- Therefore volume of d-dimension Hypersphere
  $$
    V_d=\frac{\pi^{d/2}R^d}{\Gamma\left(1+\frac d2\right)}
  $$

- Remember Gaussian integral: $\int_{-\infty}^\infty\exp(-x^2) \mathrm{d}x=\pi^{1/2}$
- Therefore:
  $$
    \begin{aligned}\left(\int_{-\infty}^\infty\exp(-x^2) \mathrm{d}x\right)^n&=\quad\pi^{n/2}\\&=\quad\int_0^\infty r^{n-1} \exp(-r^2)\mathrm{d}r \int d\Omega_n\end{aligned}
  $$

- But $\int_0^\infty r^{n-1}\exp(-r^2)\mathrm{d}r=\Gamma(n/2)/2$
- Therefore
  $$
    \int\mathrm{d}\Omega_n=\frac{2\pi^{n/2}}{\Gamma(n/2)}
  $$

##### The $\Gamma$-function

- Properties:
  - $\Gamma(x+1)=x\Gamma(x)$, for $n \in N$: $\Gamma(n+1)=n!$
  - $\Gamma(1/2)=\sqrt{\pi}$, $\Gamma(1+n/2) = \sqrt{\pi / 2^{n+1}}n!!$
- Integral representation:
  $$
    \Gamma(z) = \int^\infin_0 t^{z-1}e^{-t} dt = \int^1_0 (\ln\frac{1}{u})^{z-1} du
  $$
- First derivative:
  $$
    \Gamma^{\prime}(z)=\Gamma(z)\psi^{(0)}(z)=\Gamma(z)\left[\intop_{0}^{1}\mathrm{d}t\frac{1-t^{z-1}}{1-t}-\gamma_{E}\right]
  $$
  where Euler-Mascheroni number $\gamma_E = 0.577215665$  
  ![](./imgs/Volume%20of%20unit%20hyperspheres.png)


## Lecture 6 - Random Walks

### Random systems

- Motivation
  - Random system are described **probabilistically** rather than **deterministically**. Probabilistic means described by a **probability distribution**  
    随机系统是通过概率性而非确定性来描述的，概率性描述意味着用一个概率分布来表述系统。
  - Two generic cases of system that are described probabilistically
    - Quantum mechanical system
      > wave function descirbes probability of being in a given state
    - System with large number of degrees of freedom(dof)
      > deterministic describption impossible: equations cannot be solved and initial conditions cannot be determined anyway.  
      > Examples: Brownian motion, stirring of creeam in coffee or tea
  - 'Random' has well defined meaning: probability distribution is known result of computation is mean value and dispersion around mean, rather tahn detailed 'microscopic' state

- Pseudo-random numbers 伪随机数
  - Desired: generate a set of numbers that correctly sample a given probability distribution
    > Example: random numbers uniform in the interval $x = [0,1]; P(x) = 1$  
    > return a random set of choices from a given set
  - Extensive literature for generating 'pseudo' random numbers
    > set of numbers that samples a distribution function without **artificail correlations** or **periodicity**  
    > Pseudo random numbers because any random number generator does have artificial correlations
  - Seed: often it is useful to be able to generate the same random sequence multiple times. This can be done by starting the random sequence from a given seed

- Random walk
  - 1D: each step changes the location of the walker by $\pm 1$
  - nD: randomly choose dimension to step in
  > Pseudo-code
  > - Initialise: start m random walkers at $x = 0, i = 0, 1, ..., m - 1$
  > - Calculations:
  >   - For each walker: coose direction to step in
  >   - After each (time) step t compute
  >     - the mean displacement $\langle x(t)\rangle$
  >     - the mean squared displacement $\langle x^2(t)\rangle$
  > - Plot the results
  - Results
    - 'No' identical random walkers
    - Average(signed) displacement of all random walkers
      $$
        \langle x(t)\rangle = 0
      $$
      as expected, since $\Delta x = + 1$ equally likely as $\Delta x = -1$
    - Average mean squared displacement
      $$
        \langle x^2(t)\rangle=t ;\quad\langle x(t)^2\rangle^{1/2}\propto t^{1/2} .
      $$
      - **Increases linearly** in time meaning with the number of steps taken
      - Closely realted to the physics of **diffusion**
  - Analytical analysis
    - Write the position of walker after n steps as:
      $$
        x_{n}=\sum_{i=1}^{n}s_{i} ,\quad\mathrm{where~} s_{i}=\pm1 \mathrm{with~equal~probability}\\\langle s_{i}\rangle=0 ;\quad\langle s_{i}^{2}\rangle=1 ;\quad\langle s_{i}s_{j}\rangle=0 \mathrm{if} i\neq j
      $$
    - Therefore
      - $\langle x_n\rangle=\sum_{i=1}^n\langle s_i\rangle=0$
      - $\langle x_n^2\rangle=\langle\sum_{i=1}^n\sum_{j=1}^ns_is_j\rangle=\sum_{i=1}^n\langle s_i^2\rangle+\sum_{i=1}^n\sum_{j>i}^n\langle s_is_j\rangle=n+0 = n$
    - Assume duration of each step is $\Delta t$, $\langle x_n^2\rangle=n=\frac t{\Delta t}$
      > $\langle x_n^2\rangle $ incerases linearly with time t

### The diffusion equation

- Introduction
  - Consider the continuity equation
    $$
      \frac{\mathrm{d}\rho}{\mathrm{d}t}=-\nabla\mathbf{j}=-\nabla\rho\mathbf{v}
    $$
  - In diffusion, flux is proportional the gradient of $\rho$
    $$
      j=-D\nabla\rho 
    $$
    diffusion from high to low density. D > 0
  - Combining these yields the diffusion equation
    $$
      \frac{\mathrm{d}\rho}{\mathrm{d}t}=+D\nabla^2\rho 
    $$
    provided the diffusion coefficient, D, is uniform - the same everywhere in space

### Random walks connection to diffusion

- Consider random walk on 2D lattice with spacing $\Delta x$
- Let $P_{ij}(n)$ be the probability to find the walker at lattice position ij after n steps
- At step n - 1, there is an equal probability ot find the walker at any of its 2N neighbouring sites
  > N is the dimension, consider below N = 2
  Therefore
  $$
    P_{ij}(n)=\frac{1}{4}\left[P_{i-1j}(n-1)+P_{i+1j}(n-1)+P_{ij-1}(n-1)+P_{ij+1}(n-1)\right]
  $$
- This can be re-written as
  $$
    \begin{aligned}&P_{ij}(n)-P_{ij}(n-1)\\&=\quad\frac{1}{4}\left[P_{i-1j}(n-1)-2P_{ij}(n-1)+P_{i+1j}(n-1) +P_{ij-1}(n-1)-2P_{ij}(n-1)+P_{ij+1}(n-1)\right]\end{aligned}
  $$

We can convert this to the diffusion equation as follows
- Define time $t = n\Delta t$
  $$
    \begin{aligned}
    P(n)-P(n-1)& =\quad P(\frac t{\Delta t})-P(\frac{t-\Delta t}{\Delta t}) \\
    &\approx\quad\Delta t \frac{\mathrm{d}P(t)}{\mathrm{d}t}
    \end{aligned}
  $$
- Similarly, define position $x = i\Delta x$
  $$
    P_{i-1}-2P_i+P_{i+1}\approx(\Delta x)^2\frac{\mathrm{d}^2P(x)}{\mathrm{d}x^2}
  $$
- Combining these yields the diffusion equation,
  $$
    \dot{P}(t)=\frac{\Delta x^2}{2N\Delta t}\nabla^2P
  $$
  the diffusion constant is $D=\frac{\Delta x^2}{2N \Delta t}$, where N is the dimension of the lattice

  
