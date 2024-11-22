import time
from lib import puzz_msgs

class DriveSquare():
    def __init__(self):
        self.t_start = time.time()
    
    def spin(self,topics):

        t_total = time.time() - self.t_start

        cmdR = puzz_msgs.Float32()
        cmdL = puzz_msgs.Float32()

        # =====================================================================
        # ===== Start Here ====================================================   
        # =====================================================================     

        # Task 2
        # ==========
        # speed = 6.0
        # distance = 25.0
        # cmdR.data = speed
        # cmdL.data = speed
        # d = 10
        # t = distance / (speed * (d/2))

        # Task 3
        # =========
        t = 1.0
        d = 10.0
        pi = 3.1415926
        distance = pi * 18 / 4
        speed = distance / (t * (d / 2))
        cmdR.data = speed
        cmdL.data = speed

        if t_total > t:
            cmdR.data = -speed
            cmdL.data = speed

        if t_total > 2*t:
            cmdR.data = speed
            cmdL.data = speed

        if t_total > 3*t:
            cmdR.data = -speed
            cmdL.data = speed

        if t_total > 4*t:
            cmdR.data = speed
            cmdL.data = speed

        if t_total > 5*t:
            cmdR.data = -speed
            cmdL.data = speed

        if t_total > 6*t:
            cmdR.data = speed
            cmdL.data = speed

        if t_total > 7*t:
            cmdR.data = -speed
            cmdL.data = speed

        # ===================================================================
        # ===== End Here ====================================================   
        # ===================================================================  
        topics["VelocitySetR"] = cmdR
        topics["VelocitySetL"] = cmdL
        
        if t_total > 8*t:
            cmdR.data = 0
            cmdL.data = 0
            print(speed)
            print(t_total, t)
            topics["IsDone"] = True

        return topics