####################################################################
#
# A class to solve first-order differential equations of the form
#
#   dx_i(t)/dt = f_i(x(t),t)
#
# where x and f can be vectors with components labelled with i.
#
# In general, a solution to such a problem is provided by
# discretisation in time, i.e. by labelling timesteps in t such
# that t(j+1) = t(j)+delta t, and simultaneously, by replacing
# x_i(t) with x_i(j), with j labelling the time steps.  Then
# x_i(j+1) = x_i(j)+delta x_i, and various methods differ in how
# they determine the delta x_i.
#
# Different methods will be supplemented by the students through 
# the homework exercises, in particular a simple Euler method, 
# Runge-Kutta algorithms of order 2 and 4, and the Euler-Cromer 
# method for oscillatory problems.  In the latter, a second-order 
# differential equation is translated into two first-order 
# differential equations.
#
# This class provides the boiler plate code. To use it one has to 
# inherit from it and supply the function that implement the time step
# makeStep().
#
# The function f has to be available as a member function and should
# provide a method dx_dt  
#  
#   dx_dt(x,t), 
#
# which depends on an array x and a scalar t.
#
# In order to solve the differential equation(s), the method
#  
#  solve(x0,t0,t1,delta_t)
#
# is invoked.  Here the arguments denote the boundary conditions
# x0 = x(t0), keeping in mind that the x (and therefore the x0)
# are vectors of length Nx, which are presented as numpy arrays.  
# The x_i(t) are evolved from the starting time t0 to the end time 
# t1 in steps of the size delta_t.  The result of this evolution 
# is stored in a list 'history'.
#
# In the evolution above, the boundary conditions etc. are used
# to initialise the problem, in 
#
#  initialise(self,x0,t0,t1,delta_t)
#
# Each new step is added to the history through the method
#
#  record(self,t,x)
#
# In their homeworks the students will derive from this class 
# and provide implementations for the individual steps
# yielding the values of delta x_i by overriding makeStep(self).

import numpy as np

class DEq_Solver:

    def solve(self,x0,t0,t1,delta_t,terminateCondition=None,circular_index=None):
        self.initialise(x0,t0,t1,delta_t)
        while self.t <= self.t1:
            self.record(self.t,self.x)
            if terminateCondition is not None:
                if terminateCondition(self.x):
                    break
            self.makeStep()
        return self.history

    def initialise(self,x0,t0,t1,delta_t):
        self.x       = x0.copy()
        self.t       = t0
        self.t1      = t1
        self.delta_t = delta_t
        self.history = []

    def record(self,t,x):
        self.history.append(  ( t, x.copy() )  )

    def timeSteps(self):
        ts,vs=list(zip(*self.history))
        return np.array(ts).T

    def coordinateSteps(self):
        ts,vs=list(zip(*self.history))
        return np.array(vs).T
    
    def makeStep(self):
        ### this needs to be implemented by classes derived from this class
        raise NotImplemented
    
