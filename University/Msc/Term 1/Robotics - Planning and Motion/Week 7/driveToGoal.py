import time
import math
from lib import puzz_msgs
from my_examples import dead_reckoning as dr
from my_examples import kalman
from my_examples import BinaryMapping as mapping
from lib import my_math
import numpy as np

class DriveToGoal():
    def __init__(self):

        self.dead_reckon = dr.DeadReckoning()

        # Target points        
        self.target_x = 2
        self.target_y = 1
        
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

