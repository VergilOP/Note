import sys
import os
sys.path.append(os.getcwd()+'/../')
from DEq_Solver import DEq_Solver

class EulerSolver(DEq_Solver):
    def __init__(self,kernel):
        self.kernel  = kernel
    def makeStep(self):
        self.t += self.delta_t
        dx_dt = self.kernel.dx_dt(self.x, self.t)
        self.x += dx_dt * self.delta_t
        
class RK2Solver(DEq_Solver):
    def __init__(self,kernel):
        self.kernel  = kernel
    def makeStep(self):
        dx_dt = self.kernel.dx_dt(self.x, self.t)
        x1 = self.x + dx_dt * self.delta_t / 2.
        t1 = self.t + self.delta_t / 2.
        dx_dt_1 = self.kernel.dx_dt(x1, t1)
        self.x += dx_dt_1 * self.delta_t
        self.t += self.delta_t
        
class RK4Solver(DEq_Solver):
    def __init__(self,kernel):
        self.kernel  = kernel
    def makeStep(self):
        x1 = self.x
        t1 = self.t
        f_1 = self.kernel.dx_dt(x1, t1)

        x2 = self.x + f_1 * self.delta_t / 2.
        t2 = self.t + self.delta_t / 2.
        f_2 = self.kernel.dx_dt(x2, t2)

        x3 = self.x + f_2 * self.delta_t / 2.
        t3 = self.t + self.delta_t / 2.
        f_3 = self.kernel.dx_dt(x3, t3)

        x4 = self.x + f_3 * self.delta_t
        t4 = self.t + self.delta_t
        f_4 = self.kernel.dx_dt(x4, t4)

        delta_x = ( f_1 + 2 * f_2 + 2 * f_3 + f_4 ) / 6.
        self.x += delta_x * self.delta_t
        self.t += self.delta_t
