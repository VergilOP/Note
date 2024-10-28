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







        # ===================================================================
        # ===== End Here ====================================================   
        # ===================================================================  
        topics["VelocitySetR"] = cmdR
        topics["VelocitySetL"] = cmdL

        return topics