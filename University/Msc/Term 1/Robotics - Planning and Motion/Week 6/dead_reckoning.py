import time
import math
from lib import my_math
from lib import puzz_msgs
import numpy as np

class DeadReckoning():
    def __init__(self):
        self.pose = [0, 0, 0]

        self.k = 0.1

        self.w_r = 0.0
        self.w_l = 0.0

        self.R = 0.05
        self.L = 0.18

        self.Sig = np.array([[0.0, 0.0, 0.0],
                             [0.0, 0.0, 0.0],
                             [0.0, 0.0, 0.0]])
        
        self.t_start = time.time()
    
    def spin(self,topics):

        dt = time.time() - self.t_start
        self.t_start = time.time()

        # Read wheel angular velocities from topics (msg type is Float32)
        if "VelocityEncR" in topics:
            self.w_r = topics["VelocityEncR"].data
        if "VelocityEncL" in topics:
            self.w_l = topics["VelocityEncL"].data
        if "Pose" in topics:
            self.pose = topics["Pose"].pose
            self.Sig = topics["Pose"].cov


        # ====================================================================================================
        # =========== Task 1 - Dead Reckoning Localization ===================================================
        # =========== Start Here =============================================================================

        # Compute linear and angular velocities of the robot

        v = (self.R / 2) * (self.w_r + self.w_l)
        omega = (self.R / self.L) * (self.w_r - self.w_l)

        # Update pose (self.pose)

        x, y, theta = self.pose
        x += v * math.cos(theta) * dt
        y += v * math.sin(theta) * dt
        theta += omega * dt
        self.pose = [x, y, theta]

        # Computer robot coveriance Sig (self.Sig), using the jacobian matrix H and the covariance matrix Q.

        H = np.array([
            [1, 0, -v * math.sin(theta) * dt],
            [0, 1, v * math.cos(theta) * dt],
            [0, 0, 1]
        ])
        Q = np.array([
            [0.01, 0, 0],
            [0, 0.01, 0],
            [0, 0, 0.01]
        ])
        self.Sig = H @ self.Sig @ H.T + Q

        # ====================================================================================================
        # =========== End Here =============================================================================
        # ====================================================================================================


        # Publish dead-reckoning pose and covariance
        msg_pose = puzz_msgs.Pose()
        msg_pose.pose = self.pose
        msg_pose.cov = self.Sig

        topics["Pose"] = msg_pose

        return topics

