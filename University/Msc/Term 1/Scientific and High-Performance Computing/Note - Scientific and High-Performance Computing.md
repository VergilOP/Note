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



