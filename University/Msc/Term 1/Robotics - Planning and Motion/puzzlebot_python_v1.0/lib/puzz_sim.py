import math
import random
import numpy as np
import json
from lib import pid_controller
from lib import my_math

def AdjustU(u, u_max):
    u_adj = u*u_max
    
    if u_adj>u_max:
        return u_max
    if u_adj<-u_max:
        return -u_max
    return u_adj
    
    
class RobotParams():
    def __init__(self):
        # DC motor parameters
        self.Kt = 0.009
        self.Ke = 0.009
        self.Rar = 2.5
        self.Ral = 2.5
        self.La = 0.01
        self.u_max = 5
        
        # Gearbox parameters
        self.tr = 34
        self.miu = 1
        self.tau_fr = 0.0
        
        # Robot parameters
        self.Rr = 0.05
        self.Rl = 0.05
        self.L = 0.9
        self.d = 0.0
        self.M = 0.6
        self.mw = 0.05
        
        # Moments of inertia
        self.I0 = 0.001
        self.Im = 0.0002
        self.Iw = 0.0006
        
        # Friction parameters (static and viscous)
        self.Cs_r = 0.05
        self.Cs_l = 0.05
        self.Cv_r = 0.0002
        self.Cv_l = 0.0002


class PuzzSim():

    def __init__(self):
        self.params = RobotParams()
        
        self.M_inv = np.array([[0.0, 0.0],
                               [0.0, 0.0]])
        self.V = np.array([[0.0, 0.0],
                           [0.0, 0.0]])
                           
        self.w_r = 0.0
        self.w_l = 0.0
        
        self.Ir = 0.0
        self.Il = 0.0
        
        self.Vr = 0.0
        self.Wr = 0.0
        
        self.dt_step = 0.0001
        
        self.input_type = 1

        self.cmd_r = 0.0
        self.cmd_l = 0.0

        self.noise = 0.05

        self.pose = [0.0,0.0,0.0]
        
        self.pidR = pid_controller.PidController()
        self.pidL = pid_controller.PidController()
        self.pidR.SetParameters(0.05,0.03,0.0)
        self.pidL.SetParameters(0.05,0.03,0.0)

        self.InitModel()

    def InitRobot(self, robot_file):
        f = open(robot_file)
        
        params_json = json.load(f)
        
        f.close()
        
        self.input_type = params_json["InputType"]

        self.noise = params_json["WheelNoise"]

        self.params.Kt = params_json["DcMotor"]["Kt"]
        self.params.Ke = params_json["DcMotor"]["Ke"]
        self.params.Rar = params_json["DcMotor"]["R"]
        self.params.Ral = params_json["DcMotor"]["R"]
        self.params.La = params_json["DcMotor"]["L"]
        self.params.u_max = params_json["DcMotor"]["Umax"]
        
        self.params.tr = params_json["Gear"]["Ratio"]
        self.params.miu = params_json["Gear"]["Miu"]
        self.params.tau_fr = params_json["Gear"]["Fr"]
        
        self.params.Rr = params_json["Robot"]["R"]
        self.params.Rl = params_json["Robot"]["R"]
        self.params.L = params_json["Robot"]["L"]/2
        self.params.M = params_json["Robot"]["M"]
        self.params.d = params_json["Robot"]["d"]
        self.params.mw = params_json["Robot"]["Mw"]
        
        self.params.I0 = params_json["Inertia"]["I0"]
        self.params.Im = params_json["Inertia"]["Im"]
        self.params.Iw = params_json["Inertia"]["Iw"]
        
        self.params.Cs_r = params_json["Friction"]["Cs"]
        self.params.Cs_l = params_json["Friction"]["Cs"]
        self.params.Cv_r = params_json["Friction"]["Cv"]
        self.params.Cv_l = params_json["Friction"]["Cv"]

        self.InitModel()
        
        
    def InitModel(self):
        # Create M matrix for the Lagrange model (Inertia matrix)
        Mt = self.params.M+2*self.params.mw
        It = self.params.I0+self.params.M*self.params.d**2+2*self.params.mw*self.params.L**2+2*self.params.Im

        m00 = self.params.Rr**2/4*(Mt+It/self.params.L*2)+self.params.Iw
        m01 = self.params.Rr*self.params.Rl/4*(Mt-It/self.params.L**2)
        m10 = m01
        m11 = self.params.Rl**2/4*(Mt+It/self.params.L**2)+self.params.Iw

        self.M_inv[0,0] = m11/(m00*m11 - m01*m10)
        self.M_inv[0,1] = -m01/(m00*m11 - m01*m10)
        self.M_inv[1,0] = -m10/(m00*m11 - m01*m10)
        self.M_inv[1,1] = m00/(m00*m11 - m01*m10)

        self.V[0,0] = 0
        self.V[0,1] = self.params.M*self.params.d*self.params.Rr*self.params.Rl/(2*self.params.L)
        self.V[1,0] = -self.params.M*self.params.d*self.params.Rr*self.params.Rl/(2*self.params.L)
        self.V[1,1] = 0
    
        self.pose = [0.0,0.0,0.0]

        
    def SetCmdR(self,data):
        self.cmd_r = data

    def SetCmdL(self,data):
        self.cmd_l = data

    def GetVelocityR(self):
        return self.w_r+random.uniform(-self.noise*self.w_r,self.noise*self.w_r)
    
    def GetVelocityL(self):
        return self.w_l+random.uniform(-self.noise*self.w_l,self.noise*self.w_l)
    
    def GetPose(self):
        return self.pose
        
    def SetPose(self, pose):
        self.pose = pose
        
    def Update(self, dt):
        tmp = np.array([0.0, 0.0])
        
        N = int(dt/self.dt_step)

        Ur = 0.0
        Ul = 0.0
        
        for i in range(N):
            if self.input_type == 1:
                Ur = self.pidR.GetControl(self.cmd_r,self.w_r,self.dt_step)
                Ul = self.pidL.GetControl(self.cmd_l,self.w_l,self.dt_step)
            if self.input_type == 2:
                Ur = self.cmd_r
                Ul = self.cmd_l
            
            Ur = AdjustU(Ur,self.params.u_max)
            Ul = AdjustU(Ul,self.params.u_max)

            # Friction for each wheel
            fr_r = math.copysign(self.params.Cs_r,self.w_r)+self.params.Cv_r*self.w_r
            fr_l = math.copysign(self.params.Cs_l,self.w_l)+self.params.Cv_l*self.w_l
  
            # simulate the DC motor for each wheel
            Ir_dot = (Ur - self.params.Ke*self.params.tr*self.w_r - self.params.Rar*self.Ir)/self.params.La
            self.Ir += self.dt_step*Ir_dot
            Il_dot = (Ul - self.params.Ke*self.params.tr*self.w_l - self.params.Ral*self.Il)/self.params.La
            self.Il += self.dt_step*Il_dot
  
            tauR = self.params.tr*self.params.miu*(self.params.Kt*self.Ir)
            tauL = self.params.tr*self.params.miu*(self.params.Kt*self.Il)
  
            # Select the input
            #  1 - torque from DC motor;
            #  2 - torque from model input;
            if self.input_type <= 2:
                Tau = np.array([tauR, tauL])

            if self.input_type == 3:
                Tau = np.array([self.cmd_r, self.cmd_l])
  
            # Lagrange formula for the model - computes acceleration for each wheel
            tmp[0] = Tau[0] - self.V[0,1]*self.Wr*self.w_l - fr_r
            tmp[1] = Tau[1] - self.V[1,0]*self.Wr*self.w_r - fr_l
  
            w_r_dot = self.M_inv[0,0]*tmp[0] + self.M_inv[0,1]*tmp[1]
            w_l_dot = self.M_inv[1,0]*tmp[0] + self.M_inv[1,1]*tmp[1]
  
            # Integrate to get the velocity for each wheel
            self.w_r += self.dt_step*w_r_dot
            self.w_l += self.dt_step*w_l_dot
  
            # Compute robot velocities
            self.Vr = (self.params.Rr*self.w_r+self.params.Rl*self.w_l)/2
            self.Wr = (self.params.Rr*self.w_r-self.params.Rl*self.w_l)/(2*self.params.L)

            self.pose[0] = self.pose[0] + self.dt_step*self.Vr*math.cos(self.pose[2])
            self.pose[1] = self.pose[1] + self.dt_step*self.Vr*math.sin(self.pose[2])
            self.pose[2] = my_math.wrap_to_pi(self.pose[2] + self.dt_step*self.Wr)

