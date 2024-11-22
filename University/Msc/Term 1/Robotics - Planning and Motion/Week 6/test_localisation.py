import time
from lib import puzz_msgs
from my_examples import dead_reckoning as dr
import importlib

importlib.reload(dr)

class TestLocalisation():
    def __init__(self):
        self.t_start = time.time()

        self.dead_reckon = dr.DeadReckoning()
    
    def spin(self,topics):
        
        self.dead_reckon.spin(topics)

        t_total = time.time() - self.t_start

        cmdR = puzz_msgs.Float32()
        cmdL = puzz_msgs.Float32()

        cmdR.data = 6.0
        cmdL.data = 6.0

        topics["VelocitySetR"] = cmdR
        topics["VelocitySetL"] = cmdL

        if t_total>5.0:
            topics["IsDone"] = True

        return topics

