from lib import my_math
from lib import puzz_msgs
import math
import json

class ServoMotor():
    def __init__(self):
        self.angle_min = -90
        self.angle_max = 90

        self.name = "servo"
        self.parent_name = "robot"

        self.topic = "servo_angle"

        self.pose = [0.0, 0.0, 0.0]
        self.init_pose = [0.0, 0.0, 0.0]

        self.parent = 0

        self.angle = 0.7


    def Init(self, file, name,parent_name):
        f = open(file)
        
        self.actuator_json = json.load(f)
        
        f.close()

        self.name = name
        self.parent_name = parent_name

        self.angle_min = self.actuator_json["angle_min"]
        self.angle_max = self.actuator_json["angle_max"]

        self.topic = self.actuator_json["topic"]

    def SetParent(self, parent):
        self.parent = parent

    def SetPose(self,pose):
        self.pose = pose

    def SetInitPose(self,pose):
        self.init_pose[0] = pose[0]
        self.init_pose[1] = pose[1]
        self.init_pose[2] = pose[2]
        self.pose[0] = pose[0]
        self.pose[1] = pose[1]
        self.pose[2] = pose[2]

    def GetPose(self):
        return self.pose
    
    def GetTopic(self):
        return self.topic

    def Update(self,msg):
        self.angle = msg.data

        if self.angle<self.angle_min:
            self.angle = self.angle_min  
        if self.angle>self.angle_max:
            self.angle = self.angle_max  

        self.UpdatePose()


    def UpdateTopic(self,topics):
        for topic in topics:
            if topic == self.topic:
                self.angle = topics[topic].data
        self.UpdatePose()

       
    def UpdatePose(self):
        p_pose = self.parent.GetPose()

        self.pose[0] = p_pose[0] + self.init_pose[0]*math.cos(p_pose[2]) - self.init_pose[1]*math.sin(p_pose[2])
        self.pose[1] = p_pose[1] + self.init_pose[0]*math.sin(p_pose[2]) + self.init_pose[1]*math.cos(p_pose[2])
        self.pose[2] = my_math.wrap_to_pi(p_pose[2]+self.init_pose[2]+self.angle*3.1415/180.0)



    
