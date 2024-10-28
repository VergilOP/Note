from lib import world
from lib import my_math
from lib import puzz_msgs
import matplotlib.patches as patches
import math
import json
import random

def GetRange(pose,angle,world,range_min,range_max):
    line_range = my_math.Line2D(pose[0]+range_min*math.cos(angle+pose[2]),
                                pose[1]+range_min*math.sin(angle+pose[2]),
                                pose[0]+range_max*math.cos(angle+pose[2]),
                                pose[1]+range_max*math.sin(angle+pose[2]))
    range = range_max
    for line in world.lines:
        ret,p_i = my_math.GetLineIntersection(line_range,line)
        if ret:
            d = math.sqrt((p_i.x-pose[0])**2+(p_i.y-pose[1])**2)
            if d<range:
                range = d
    for circle in world.circles:
        ret,p_i = my_math.GetLineCircleIntersection(circle,line_range)
        if ret:
            d = math.sqrt((p_i.x-pose[0])**2+(p_i.y-pose[1])**2)
            #print(range,d)
            if d<range:
                range = d
    return range

                          
class Lidar():
    def __init__(self):
        self.angle_min = -3.1415
        self.angle_max = 3.1415
        self.range_min = 0.0
        self.range_max = 4.0
        self.angle_res = 3.1415/300

        self.sigma = 0.01

        self.name = "lidar"
        self.parent_name = "robot"

        self.topic = "laser_scan"

        self.pose = [0.0, 0.0, 0.0]
        self.init_pose = [0.0, 0.0, 0.0]

        self.ranges = []
        self.angles = []

        self.parent = 0
        self.parent_set = False

        self.laser_msg = puzz_msgs.LaserScan()

        self.laser_msg.angle_min = self.angle_min
        self.laser_msg.angle_max = self.angle_max
        self.laser_msg.range_min = self.range_min
        self.laser_msg.range_max = self.range_max


    def Init(self, file, name, parent_name):
        f = open(file)
        
        self.sensor_json = json.load(f)
        
        f.close()

        self.name = name
        self.parent_name = parent_name

        self.angle_min = self.sensor_json["angle_min"]
        self.angle_max = self.sensor_json["angle_max"]
        self.angle_res = self.sensor_json["angle_res"]
        self.range_min = self.sensor_json["range_min"]
        self.range_max = self.sensor_json["range_max"]

        self.topic = self.sensor_json["topic"]

        self.sigma = self.sensor_json["range_standard_deviation"]

        self.laser_msg.angle_min = self.angle_min
        self.laser_msg.angle_max = self.angle_max
        self.laser_msg.range_min = self.range_min
        self.laser_msg.range_max = self.range_max
        self.laser_msg.topic = self.topic

    def GetPose(self):
        return self.pose

    def GetTopic(self):
        return self.topic

    def SetPose(self,pose):
        self.pose = pose

    def SetInitPose(self,pose):
        self.init_pose[0] = pose[0]
        self.init_pose[1] = pose[1]
        self.init_pose[2] = pose[2]
        self.pose[0] = pose[0]
        self.pose[1] = pose[1]
        self.pose[2] = pose[2]

    def SetParent(self, parent):
        self.parent = parent
        self.parent_set = True

    def Update(self,world):
        self.UpdatePose()

        self.ranges = []
        self.angles = []
        angle = self.angle_min
        while angle < self.angle_max:
            range = GetRange(self.pose,angle,world,self.range_min,self.range_max)
            range = range + random.uniform(-self.sigma*range,self.sigma*range)
            self.ranges.append(range)
            self.angles.append(angle)
            angle = angle + self.angle_res

        self.laser_msg.ranges = self.ranges

        return self.topic,self.laser_msg
    
    def UpdateTopic(self,topics):
        self.UpdatePose()

        for topic in topics:
            if topic == self.topic:
                self.ranges = topics[topic].ranges

    def UpdatePose(self):
        if self.parent_set:
            p_pose = self.parent.GetPose()

            self.pose[0] = p_pose[0] + self.init_pose[0]*math.cos(p_pose[2]) - self.init_pose[1]*math.sin(p_pose[2])
            self.pose[1] = p_pose[1] + self.init_pose[0]*math.sin(p_pose[2]) + self.init_pose[1]*math.cos(p_pose[2])
            self.pose[2] = my_math.wrap_to_pi(p_pose[2]+self.init_pose[2])

    def Plot(self, axes):
        x_m = []
        y_m = []

        angle = self.angle_min
        for i in range(len(self.ranges)):
            x_m.append(self.pose[0] + self.ranges[i]*math.cos(angle+self.pose[2]))
            y_m.append(self.pose[1] + self.ranges[i]*math.sin(angle+self.pose[2]))
            angle = angle + self.angle_res

        axes.fill(x_m,y_m,color='red',alpha=0.1)


class RangeSensor():
    def __init__(self):
        self.field_of_view = 3.1415/3
        self.range_min = 0.0
        self.range_max = 4.0

        self.sigma = 0.01

        self.name = "laser"
        self.parent_name = "robot"

        self.topic = "laser_range"

        self.pose = [0.0, 0.0, 0.0]
        self.init_pose = [0.0, 0.0, 0.0]

        self.range = 0.0

        self.parent = 0
        self.parent_set = False

        self.range_msg = puzz_msgs.Float32()



    def Init(self, file, name, parent_name):
        f = open(file)
        
        self.sensor_json = json.load(f)
        
        f.close()

        self.name = name
        self.parent_name = parent_name

        self.field_of_view = self.sensor_json["field_of_view"]
        self.range_min = self.sensor_json["range_min"]
        self.range_max = self.sensor_json["range_max"]

        self.topic = self.sensor_json["topic"]

        self.sigma = self.sensor_json["range_standard_deviation"]


    def GetPose(self):
        return self.pose

    def GetTopic(self):
        return self.topic

    def SetPose(self,pose):
        self.pose = pose

    def SetInitPose(self,pose):
        self.init_pose[0] = pose[0]
        self.init_pose[1] = pose[1]
        self.init_pose[2] = pose[2]
        self.pose[0] = pose[0]
        self.pose[1] = pose[1]
        self.pose[2] = pose[2]

    def SetParent(self, parent):
        self.parent_set = True
        self.parent = parent

    def Update(self,world):
        self.UpdatePose()

        self.range = self.range_max
        angle = -self.field_of_view/2
        while angle < self.field_of_view/2:
            range = GetRange(self.pose,angle,world,self.range_min,self.range_max)
            if range<self.range:
                self.range = range
            angle = angle + 0.01
        self.range = self.range + random.uniform(-self.sigma*self.range,self.sigma*self.range)
        self.range_msg.data = self.range
        
        return self.topic,self.range_msg
    
    def UpdatePose(self):
        if self.parent_set:
            p_pose = self.parent.GetPose()

            self.pose[0] = p_pose[0] + self.init_pose[0]*math.cos(p_pose[2]) - self.init_pose[1]*math.sin(p_pose[2])
            self.pose[1] = p_pose[1] + self.init_pose[0]*math.sin(p_pose[2]) + self.init_pose[1]*math.cos(p_pose[2])
            self.pose[2] = my_math.wrap_to_pi(p_pose[2]+self.init_pose[2])
       
    def UpdateTopic(self,topics):
        self.UpdatePose()

        for topic in topics:
            if topic == self.topic:
                self.range = topics[topic].data

    
    def Plot(self, axes):
        th1 = (self.pose[2]-self.field_of_view/2)*180/3.1415
        th2 = (self.pose[2]+self.field_of_view/2)*180/3.1415
        axes.add_patch(patches.Wedge(
                    (self.pose[0], self.pose[1]),         # (x,y)
                    self.range,                           # radius
                    th1,                                  # theta1 (in degrees)
                    th2,                                  # theta2
                    color="orange", alpha=0.2
                )
            )

class BearingRangeSensor():
    def __init__(self):
        self.field_of_view = 3.1415/3
        self.range_min = 0.0
        self.range_max = 4.0

        self.sigma = 0.01

        self.name = "bearing"
        self.parent_name = "robot"

        self.topic = "marker_bearing"

        self.pose = [0.0, 0.0, 0.0]
        self.init_pose = [0.0, 0.0, 0.0]

        self.range = 0.0

        self.parent = 0
        self.parent_set = False

        self.markers = puzz_msgs.MarkerBearingArray()


    def Init(self, file, name, parent_name):
        f = open(file)
        
        self.sensor_json = json.load(f)
        
        f.close()

        self.name = name
        self.parent_name = parent_name

        self.field_of_view = self.sensor_json["field_of_view"]
        self.range_min = self.sensor_json["range_min"]
        self.range_max = self.sensor_json["range_max"]

        self.topic = self.sensor_json["topic"]

        self.sigma = self.sensor_json["range_standard_deviation"]

    def GetPose(self):
        return self.pose

    def SetPose(self,pose):
        self.pose = pose

    def GetTopic(self):
        return self.topic

    def SetInitPose(self,pose):
        self.init_pose[0] = pose[0]
        self.init_pose[1] = pose[1]
        self.init_pose[2] = pose[2]
        self.pose[0] = pose[0]
        self.pose[1] = pose[1]
        self.pose[2] = pose[2]

    def SetParent(self, parent):
        self.parent = parent
        self.parent_set = True

    def Update(self,world):
        self.UpdatePose()

        self.range = self.range_max

        self.markers.markers.clear()

        for shape in world.world_json["shapes"]:
            if shape["type"] == "marker_bearing":
                x1 = shape["coordinates"][0]
                y1 = shape["coordinates"][1]
                dx = x1 - self.pose[0]
                dy = y1 - self.pose[1]
                range = math.sqrt(dx**2 + dy**2)
                range = range + random.uniform(-self.sigma*range,self.sigma*range)
                if range<self.range_max and range>self.range_min:
                    angle = my_math.wrap_to_pi(math.atan2(dy,dx) - self.pose[2])
                    angle = angle + random.uniform(-self.sigma/2,self.sigma/2)
                    if angle>-self.field_of_view/2 and angle<self.field_of_view/2:
                        marker = puzz_msgs.MarkerBearing()
                        marker.id = shape["id"]
                        marker.range = range
                        marker.theta = angle
                        self.markers.markers.append(marker)
        
        return self.topic,self.markers
    
    def UpdatePose(self):
        if self.parent_set:
            p_pose = self.parent.GetPose()
 
            self.pose[0] = p_pose[0] + self.init_pose[0]*math.cos(p_pose[2]) - self.init_pose[1]*math.sin(p_pose[2])
            self.pose[1] = p_pose[1] + self.init_pose[0]*math.sin(p_pose[2]) + self.init_pose[1]*math.cos(p_pose[2])
            self.pose[2] = my_math.wrap_to_pi(p_pose[2]+self.init_pose[2])

    def UpdateTopic(self,topics):
        self.UpdatePose()
        for topic in topics:
            if topic == self.topic:
                self.markers = topics[topic]
    
    def Plot(self, axes):
        for marker in self.markers.markers:
            x1 = self.pose[0]+marker.range*math.cos(self.pose[2]+marker.theta)
            y1 = self.pose[1]+marker.range*math.sin(self.pose[2]+marker.theta)
            axes.add_patch(patches.Circle((x1,y1), 0.05,color="green"))

