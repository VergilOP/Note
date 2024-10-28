import time
from lib import puzz_msgs

class DriveStraight():
    def __init__(self):
        self.t_start = time.time()
    
    def spin(self,topics):

        t_total = time.time() - self.t_start

        cmdR = puzz_msgs.Float32()
        cmdL = puzz_msgs.Float32()

        cmdR.data = 3.0
        cmdL.data = 3.0

        topics["VelocitySetR"] = cmdR
        topics["VelocitySetL"] = cmdL

        if t_total>5.0:
            topics["IsDone"] = True

        return topics

