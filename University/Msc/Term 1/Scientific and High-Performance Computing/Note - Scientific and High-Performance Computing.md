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

