
from lib import world
from lib import robot
from lib import puzz_msgs
import time
import threading
import os
import sys
import inspect
import importlib
 

class PuzzLoop:

    def __init__(self):
        self._ip = "192.168.1.1"
        self._world = "world_empty.json"
        self._robot = "puzzlebot01.json"
        self._module = "drive_straight.py"
        self._init_pose = "0.0, 0.0, 0.0"

        self.user_instance = None

        self.W1 = world.World()
        self.R1 = robot.Robot()

        self.pose_est = puzz_msgs.Pose()
        self.pose = [0.0, 0.0, 0.0]
        self.init_pose = [0.0, 0.0, 0.0]

        self.topics = {}

        self.is_sim = True

        self.is_running = False
        self.is_done = False
        self.dt_spin = 0.05
        self.t_run = 0.0


    def SetWorld(self,file):
        self.W1.LoadWorld(os.getcwd()+"/worlds/"+file)
        self._world = file
        
    def SetRobot(self,file):
        self.R1.Init(os.getcwd()+"/hardware/"+file)
        self._robot = file
        
    def SetModule(self, file):
        self._module = file

    def SetInitPose(self, pose):
        self.init_pose = pose
        self.pose = pose
        self.R1.SetPose(self.pose)

    def SetPose(self, pose):
        self.pose = pose
        self.R1.SetPose(self.pose)

    def SetSim(self, sim):
        self.is_sim = sim

    def SetIp(self, ip):
        self._ip = ip
        
    def on_play(self):

        if not self.is_running:
            self.R1.Init(os.getcwd()+"/hardware/"+self._robot)

            self.dt_spin = self.R1.dt
            
            if not self.is_sim:
                self.R1.SetIp(self._ip)
                self.R1.Connect()

            self.is_running = True
            self.UpdateUserInstance()
            self.timer_spin = time.time()
            #self.task_spin = threading.Thread(target=self.spin)
            #self.task_spin.start()

    def on_stop(self):
        if self.is_running:
            if self.is_sim == False:
                self.R1.Disconnect()
            
            self.is_running = False
            self.is_done = False
            #self.task_spin.join()
            self.t_run = 0.0
            self.pose = self.init_pose
            self.R1.SetPose(self.pose)
            self.topics = {}
            self.topics["PoseSim"] = self.pose

    def UpdateUserInstance(self):
        module_name = 'my_examples.'+self._module
        if module_name in sys.modules:
            my_module = sys.modules[module_name]
            importlib.reload(my_module)
        else:
            __import__(module_name)
            my_module = sys.modules[module_name]           
        # __import__(module_name)
        # my_module = sys.modules[module_name]

        classes = [cls_name for cls_name, cls_obj in inspect.getmembers(my_module) if inspect.isclass(cls_obj)]    
        class_ = getattr(my_module, classes[0])
        self.user_instance = class_()

    def spin(self):
        
        if self.is_running:
            if not self.is_done:
                dt = time.time() - self.timer_spin
    
                if dt>self.dt_spin:
                    self.timer_spin = time.time()
                    self.t_run = self.t_run + dt

                    #print("Dt run: ",dt)
                    self.topics["IsDone"] = False

                    self.topics["Time"] = self.t_run

                    self.topics = self.user_instance.spin(self.topics)

                    self.topics = self.R1.Update(self.topics,self.W1,dt)

                    self.pose = self.R1.GetPose()
                    self.topics["PoseSim"] = self.pose
                    
                    if "Pose" in self.topics:
                        self.pose_est = self.topics["Pose"]

                    if self.topics["IsDone"]:
                        self.is_done = True

            #time.sleep(0.001)
  
    def GetTopics(self):
        return self.topics
    
    def on_closing(self):
        self.on_stop()
        #sys.exit(0)

    def signal_handler(self,sig, frame):
        self.on_closing()
        
