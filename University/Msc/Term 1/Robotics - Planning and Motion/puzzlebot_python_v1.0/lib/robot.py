import json
import math
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from lib import puzz_sim
from lib import puzz_comm
from lib import puzz_msgs
from lib import sensor
from lib import actuator
import os

class Robot():
    def __init__(self):
        self.comm = puzz_comm.PuzzComm()
        self.sim = puzz_sim.PuzzSim()

        self.type = "ddr"
        self.name = "robot"

        self.ip = "192.168.1.1"
        self.port = 3142

        self.pose_topic = "Pose"

        self.cmd_max = 10.0

        self.dt = 0.05

        self.pose = [0.0, 0.0, 0.0]

        self.subscribers = []
        self.publishers = []

        self.sensors = []
        self.actuators = []

        self.objects = {}
 
        self.is_sim = True

        self.robot_points_x = [0.07,  0.12, 0.12, 0.07,  -0.07,  -0.24, -0.24, -0.07]
        self.robot_points_y = [0.12, 0.07, -0.07, -0.12, -0.12, -0.06,  0.06,  0.12]
        self.wheel_r_x = [-0.07, 0.07]
        self.wheel_r_y = [-0.16, -0.16]
        self.wheel_l_x = [-0.07, 0.07]
        self.wheel_l_y = [0.16, 0.16]

        self.robot_triangle_x = [-0.2, 0.2, -0.2, -0.1]
        self.robot_triangle_y = [0.15, 0.0, -0.15, 0.0]

        self.arrow_size = 0.3

        self.robot_gui = 'Puzzlebot'

    def Init(self, robot_file):
        f = open(robot_file)       
        self.robot_json = json.load(f)      
        f.close()

        if self.is_sim == False:
            self.comm.Disconnect()
        self.is_sim = True

        self.sensors.clear()
        self.actuators.clear()

        self.type = self.robot_json["type"]
        self.name = self.robot_json["name"]

        self.objects[self.name] = self.sim
        
        self.ip = self.robot_json["comm"]["ip"]
        self.port = self.robot_json["comm"]["port"]

        self.dt = self.robot_json["dt"]

        self.cmdR_topic = self.robot_json["cmdR_topic"]
        self.cmdL_topic = self.robot_json["cmdL_topic"]
        self.encR_topic = self.robot_json["encR_topic"]
        self.encL_topic = self.robot_json["encL_topic"]
        self.pose_topic = self.robot_json["pose_topic"]

        self.cmd_max = self.robot_json["cmd_max"]

        self.publishers.append(self.encR_topic)
        self.publishers.append(self.encL_topic)
        self.subscribers.append(self.cmdR_topic)
        self.subscribers.append(self.cmdL_topic)

        self.comm.Init(self.ip,self.port)
        self.sim.InitRobot(os.getcwd()+"/hardware/defs/"+self.robot_json["model"])

        for sensor_json in self.robot_json["sensors"]:
            if sensor_json["type"] == "lidar-2d":
                sns = sensor.Lidar()
            if sensor_json["type"] == "range":
                sns = sensor.RangeSensor()
            if sensor_json["type"] == "bearing-range":
                sns = sensor.BearingRangeSensor()
            sns.Init(os.getcwd()+"/hardware/defs/"+sensor_json["file"],sensor_json["name"],sensor_json["parent"])
            sns.SetInitPose(sensor_json["pose"])
            self.sensors.append(sns)
            self.objects[sensor_json["name"]] = sns
            self.publishers.append(sns.GetTopic())
 
        for actuator_json in self.robot_json["actuators"]:
            if actuator_json["type"] == "servo-motor":
                act = actuator.ServoMotor()
                act.Init(os.getcwd()+"/hardware/defs/"+actuator_json["file"],actuator_json["name"],actuator_json["parent"])
                act.SetInitPose(actuator_json["pose"])
                self.actuators.append(act)
                self.objects[actuator_json["name"]] = act
                self.subscribers.append(act.GetTopic())

        for sns in self.sensors:
            sns.SetParent(self.objects[sns.parent_name])

        for act in self.actuators:
            act.SetParent(self.objects[act.parent_name])


    def SetIp(self, ip):
        self.ip = ip
        self.comm.Init(self.ip,self.port)

    def Connect(self):
        self.comm.Connect()
        self.is_sim = False

    def Disconnect(self):
        if self.is_sim == False:
            msg = puzz_msgs.Float32()
            self.SetTopic(self.cmdR_topic,msg)
            self.SetTopic(self.cmdL_topic,msg)

            self.comm.Disconnect()
            self.is_sim = True

    def Update(self,topics, world, dt):
        if self.is_sim:
            for topic in topics:
                if topic == self.cmdR_topic:
                    self.sim.SetCmdR(topics[topic].data)
                if topic == self.cmdL_topic:
                    self.sim.SetCmdL(topics[topic].data)

            self.sim.Update(dt)
            self.pose = self.sim.GetPose()

            msg_encR = puzz_msgs.Float32()
            msg_encR.data = self.sim.GetVelocityR()   
            topics[self.encR_topic] = msg_encR
            msg_encL = puzz_msgs.Float32()
            msg_encL.data = self.sim.GetVelocityL()   
            topics[self.encL_topic] = msg_encL


            for act in self.actuators:
                if act.topic in topics:
                    act.Update(topics[act.topic])
                else:
                    act.UpdatePose()

            for sns in self.sensors:
                topic,msg = sns.Update(world)
                topics[topic] = msg
        else:
            for topic in self.subscribers:
                if topic in topics:
                    self.SetTopic(topic,topics[topic])

            self.comm.SpinOnce()

            for topic in self.publishers:
                msg, ret = self.comm.GetTopicMsg(topic)
                if ret:
                    topics[topic] = msg

        return topics

    def UpdateTopic(self, topics):
        for topic in topics:
            if self.is_sim:
                if topic == self.pose_topic+"Sim":
                    self.pose = topics[topic]
            else:
                if topic == self.pose_topic:
                    self.pose = topics[topic].pose

        self.sim.SetPose(self.pose)
        for act in self.actuators:
            act.UpdateTopic(topics)
        for sns in self.sensors:
            sns.UpdateTopic(topics)


    def SetTopic(self,topic,msg_set):
        # Limit the control signal for the wheels
        if topic == self.cmdR_topic or topic == self.cmdL_topic:
            if msg_set.data > self.cmd_max:
                msg_set.data = self.cmd_max
            if msg_set.data < -self.cmd_max:
                msg_set.data = -self.cmd_max

        msg_set.topic = topic
        self.comm.SendMessage(msg_set)

    def GetPose(self):
        return self.pose
    
    def SetPose(self, pose):
        self.pose = pose
        self.sim.SetPose(pose)

    def SetSim(self, is_sim):
        self.is_sim = is_sim
    
    def Plot(self, axes,sensor,gui):
        self.robot_gui = gui
        robot_poly_x = []
        robot_poly_y = []
        if self.robot_gui == 'Triangle':
            for i in range(len(self.robot_triangle_x)):
                x = self.pose[0] + self.robot_triangle_x[i]*math.cos(self.pose[2]) - self.robot_triangle_y[i]*math.sin(self.pose[2])
                y = self.pose[1] + self.robot_triangle_x[i]*math.sin(self.pose[2]) + self.robot_triangle_y[i]*math.cos(self.pose[2])
                robot_poly_x.append(x)
                robot_poly_y.append(y)
        elif self.robot_gui == 'Puzzlebot':
            for i in range(len(self.robot_points_x)):
                x = self.pose[0] + self.robot_points_x[i]*math.cos(self.pose[2]) - self.robot_points_y[i]*math.sin(self.pose[2])
                y = self.pose[1] + self.robot_points_x[i]*math.sin(self.pose[2]) + self.robot_points_y[i]*math.cos(self.pose[2])
                robot_poly_x.append(x)
                robot_poly_y.append(y)


        axes.fill(robot_poly_x,robot_poly_y,color="blue")
        axes.add_patch(plt.Circle([self.pose[0],self.pose[1]], 0.03,color="white"))

        if self.robot_gui == 'Puzzlebot':
            x1 = self.pose[0] + self.wheel_r_x[0]*math.cos(self.pose[2]) - self.wheel_r_y[0]*math.sin(self.pose[2])
            y1 = self.pose[1] + self.wheel_r_x[0]*math.sin(self.pose[2]) + self.wheel_r_y[0]*math.cos(self.pose[2])
            x2 = self.pose[0] + self.wheel_r_x[1]*math.cos(self.pose[2]) - self.wheel_r_y[1]*math.sin(self.pose[2])
            y2 = self.pose[1] + self.wheel_r_x[1]*math.sin(self.pose[2]) + self.wheel_r_y[1]*math.cos(self.pose[2])
            axes.plot([x1,x2],[y1,y2],color="black")

            x1 = self.pose[0] + self.wheel_r_x[0]*math.cos(self.pose[2]) - self.wheel_l_y[0]*math.sin(self.pose[2])
            y1 = self.pose[1] + self.wheel_r_x[0]*math.sin(self.pose[2]) + self.wheel_l_y[0]*math.cos(self.pose[2])
            x2 = self.pose[0] + self.wheel_r_x[1]*math.cos(self.pose[2]) - self.wheel_l_y[1]*math.sin(self.pose[2])
            y2 = self.pose[1] + self.wheel_r_x[1]*math.sin(self.pose[2]) + self.wheel_l_y[1]*math.cos(self.pose[2])
            axes.plot([x1,x2],[y1,y2],color="black")

        if self.robot_gui == 'Arrow':
            dx = self.arrow_size*math.cos(self.pose[2])
            dy = self.arrow_size*math.sin(self.pose[2])
            axes.add_patch(patches.Arrow(self.pose[0],self.pose[1],dx,dy,width=0.1,color="blue"))
            axes.add_patch(patches.Circle((self.pose[0],self.pose[1]), 0.03,color="blue"))

        if sensor:
            for sns in self.sensors:
                sns.Plot(axes)



