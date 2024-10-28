import numpy as np

class Float32():
    def __init__(self):
        self.type = 1
        self.topic = ""
        self.data = 0.0
        self.stamp = 0.0
        self.status = -1

class Pose():        
    def __init__(self):
        self.type = 6
        self.topic = ""
        self.pose = [0.0, 0.0, 0.0]
        self.cov = np.array([[0.0, 0.0, 0.0],
                    [0.0, 0.0, 0.0],
                    [0.0, 0.0, 0.0]])
        self.stamp = 0.0
        self.status = -1

class LaserScan():
    def __init__(self):
        self.type = 5
        self.topic = ""
        self.ranges = []
        self.angle_min = -3.1415
        self.angle_max = 3.1415
        self.range_min = 0.0
        self.range_max = 4.0
        self.stamp = 0.0
        self.status = -1

class Range():        
    def __init__(self):
        self.type = 5
        self.topic = ""
        self.range = 0.0
        self.field_of_view = -3.1415/3
        self.range_min = 0.0
        self.range_max = 4.0
        self.stamp = 0.0
        self.status = -1

class MarkerAruco():
    def __init__(self):
        self.type = 6
        self.id = 0
        self.x = 0.0
        self.y = 0.0
        self.theta = 0.0

class MarkerBearing():
    def __init__(self):
        self.type = 7
        self.id = 0
        self.range = 0.0
        self.theta = 0.0

class MarkerBearingArray():
    def __init__(self):
        self.type = 8
        self.topic = ""
        self.stamp = 0.0
        self.status = -1
        self.markers = []
