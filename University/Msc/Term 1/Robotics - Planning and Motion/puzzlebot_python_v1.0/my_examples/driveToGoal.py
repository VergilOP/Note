import time
import math
from lib import puzz_msgs
from my_examples import dead_reckoning as dr
# from my_examples import kalman
# from my_examples import BinaryMapping as mapping
from lib import my_math
import numpy as np

class DriveToGoal():
    def __init__(self):

        self.dead_reckon = dr.DeadReckoning()

        # Target points        
        # self.target_x = 2
        # self.target_y = 1
        # self.target_x = -3
        # self.target_y = 2
        self.target_x = 4
        self.target_y = -2
        
        self.v_max = 0.2
        self.w_max = 2

        self.w_setR = 0.0
        self.w_setL = 0.0
    
    def spin(self,topics):

        self.dead_reckon.spin(topics)

        est_pose = self.dead_reckon.pose

        # ====================================================================================================
        # =========== Task 1 - Motion Control ===================================================
        # =========== Start Here =============================================================================
        
        # Hint: You will need to caluclate the distance and angular error first, and then use these information for your propotional controller design. 
        # Note that, you need to set a condition such that the robot will stop moving once it is close enough to the goal, e.g., 0.1 m. 
        # Besides, considering the motor stauration, the directly generated control signal may be unrealistic, how you can address this issue in your controller design?
        # In the end, you will need to obtain the desired left and right motor speed to achieve the goal.
        # Good luck!

        dx = self.target_x - est_pose[0]
        dy = self.target_y - est_pose[1]

        distance_error = math.sqrt(dx**2 + dy**2)
        angle_to_goal = math.atan2(dy, dx)
        angle_error = angle_to_goal - est_pose[2]
        angle_error = (angle_error + math.pi) % (2 * math.pi) - math.pi
        
        k_linear = 0.5
        k_angular = 1.0

        v = k_linear * distance_error
        w = k_angular * angle_error

        v = max(min(v, self.v_max), -self.v_max)
        w = max(min(w, self.w_max), -self.w_max)
    
        self.L = 0.09
        self.R = 0.05

        # Linear velocity v = (w_setR * R + w_setL * R) / 2
        # Angular velocity w = (w_setR * R - w_setL * R) / L
        # Solving these equations for w_setR and w_setL:
        # w_setR = (2 * v + w * L) / (2 * R)
        # w_setL = (2 * v - w * L) / (2 * R)
        self.w_setR = (2 * v + w * self.L) / (2 * self.R)
        self.w_setL = (2 * v - w * self.L) / (2 * self.R)

        if distance_error < 0.1:
            self.w_setR = 0.0
            self.w_setL = 0.0
            topics["IsDone"] = True

        # ====================================================================================================
        # =========== End Here =============================================================================
        # ====================================================================================================

        # Publish wheel velocities
        msg_w_setR = puzz_msgs.Float32()
        msg_w_setL = puzz_msgs.Float32()
        msg_w_setR.data = self.w_setR
        msg_w_setL.data = self.w_setL

        topics["VelocitySetR"] = msg_w_setR
        topics["VelocitySetL"] = msg_w_setL

        return topics

