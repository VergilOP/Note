import tkinter as tk
from tkinter import filedialog as fd
from tkinter import ttk
import numpy as np
import json
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg,NavigationToolbar2Tk
import math
from lib import world
from lib import robot
from lib import puzz_msgs
from lib import my_math
from lib import puzz_loop
import time
import threading, traceback
import os
import sys
import inspect
import signal
import queue
from multiprocessing import Lock, Process, Queue
 

class PuzzGui:

    def __init__(self, toplevel):
        self.toplevel = toplevel

        self._ip = "192.168.1.1"
        self._world = "world_empty.json"
        self._robot = "puzzlebot01.json"
        self._module = "drive_straight.py"
        self._init_pose = "0, 0, 0"
        self._real = tk.BooleanVar()
        self._path = tk.BooleanVar()
        self._sensor = tk.BooleanVar()
        self._robot_gui = tk.StringVar() 

        self.toplevel.columnconfigure(0, weight=0, minsize=280)
        self.toplevel.columnconfigure(1, weight=1, minsize=720)
        self.toplevel.rowconfigure(0, weight=1, minsize=720)

        self.img_play = tk.PhotoImage(file = os.getcwd()+"/gui/img/play_small.png") 
        self.img_pause = tk.PhotoImage(file = os.getcwd()+"/gui/img/pause_small.png") 
        self.img_stop = tk.PhotoImage(file = os.getcwd()+"/gui/img/stop_small.png") 
        self.img_open = tk.PhotoImage(file = os.getcwd()+"/gui/img/open.png") 

        self.frame_edit = tk.Frame(master=self.toplevel,relief=tk.RAISED,borderwidth=1)
        self.frame_right = tk.Frame(master=self.toplevel,borderwidth=1)

        self.frame_top = tk.Frame(master=self.frame_right,relief=tk.RAISED,borderwidth=1)
        self.frame_plot = tk.Frame(master=self.frame_right,relief=tk.RAISED,borderwidth=1)
        self.frame_info = tk.Frame(master=self.frame_top,relief=tk.SUNKEN,borderwidth=1)
        self.frame_nav = tk.Frame(master=self.frame_top,borderwidth=1)

        self.frame_edit.grid(row=0, column=0, padx=3, pady=3, sticky="nsew")
        self.frame_right.grid(row=0, column=1, padx=3, pady=3, sticky="nsew")

        self.txt_pose = tk.Label(master=self.frame_info,text="Pose: 0.0, 0.0, 0.0")
        self.txt_pose.pack()
        self.txt_time = tk.Label(master=self.frame_info,text="Time: 0.0")
        self.txt_time.pack()
 
        self.frame_info.pack(side = "right")

        b_play = tk.Button(self.frame_top,image = self.img_play,command=self.on_play)
        b_play.pack(side = "left",padx=1)
        #b_pause = tk.Button(self.frame_top,image = self.img_pause,command=self.on_pause)
        #b_pause.pack(side = "left",padx=1)
        b_stop = tk.Button(self.frame_top,image = self.img_stop,command=self.on_stop)
        b_stop.pack(side = "left",padx=1)
        self.frame_nav.pack(side = "left",padx=20)

        self.frame_top.pack(fill=tk.X,pady=3)
        self.frame_plot.pack(fill=tk.BOTH,expand=tk.TRUE)

        self.ip_label = tk.Label(self.frame_edit, text = 'Robot Ip', font=('calibre',10, 'bold'))
        self.ip_entry = tk.Entry(self.frame_edit, font=('calibre',10,'normal'))
        self.ip_entry.insert(tk.END, self._ip)
        self.b_real = tk.Checkbutton(self.frame_edit, text="Connect", onvalue=1, offvalue=0,variable=self._real)

        self.world_label = tk.Label(self.frame_edit, text = 'World', font=('calibre',10, 'bold'))
        self.world_entry = tk.Entry(self.frame_edit, font=('calibre',10,'normal'))
        self.b_open_world = tk.Button(self.frame_edit,image = self.img_open,command=self.on_open_world)

        self.robot_label = tk.Label(self.frame_edit, text = 'Robot', font=('calibre',10, 'bold'))
        self.robot_entry = tk.Entry(self.frame_edit,textvariable = self._robot, font=('calibre',10,'normal'))
        self.b_open_robot = tk.Button(self.frame_edit,image = self.img_open,command=self.on_open_robot)

        self.pose_label = tk.Label(self.frame_edit, text = 'Init Pose', font=('calibre',10, 'bold'))
        self.pose_entry = tk.Entry(self.frame_edit, font=('calibre',10,'normal'))
        self.b_update_pose = tk.Button(self.frame_edit,text=">>",command=self.on_update_pose)
        
        self.module_label = tk.Label(self.frame_edit, text = 'Module', font=('calibre',10, 'bold'))
        self.module_entry = tk.Entry(self.frame_edit,textvariable = self._module, font=('calibre',10,'normal'))
        self.b_open_module = tk.Button(self.frame_edit,image = self.img_open,command=self.on_open_module)

        self.robot_gui_label = tk.Label(self.frame_edit, text = 'Robot gui shape', font=('calibre',10, 'bold'))
        self.robot_gui_combo = ttk.Combobox(self.frame_edit, width = 27,  textvariable = self._robot_gui) 
        self.robot_gui_combo['values'] = ('Puzzlebot',   
                                     'Triangle',
                                     'Arrow') 
        self.robot_gui_combo.set('Puzzlebot')

        self.b_path = tk.Checkbutton(self.frame_edit, text="Plot robot path", onvalue=1, offvalue=0,variable=self._path)
        self.b_sensor = tk.Checkbutton(self.frame_edit, text="Plot sensors", onvalue=1, offvalue=0,variable=self._sensor)
        
        self.empty_label = tk.Label(self.frame_edit, text = '                  ', font=('calibre',10, 'bold'))

        self.LoadState()

        self.ip_label.grid(row=0,column=0,padx=3,pady=5)
        self.ip_entry.grid(row=0,column=1,padx=3,pady=5)
        self.b_real.grid(row=0,column=2,padx=3,pady=5)
        self.world_label.grid(row=1,column=0,padx=3,pady=5)
        self.world_entry.grid(row=1,column=1,padx=3,pady=5)
        self.b_open_world.grid(row=1,column=2,padx=3,pady=5)
        self.robot_label.grid(row=2,column=0,padx=3,pady=5)
        self.robot_entry.grid(row=2,column=1,padx=3,pady=5)
        self.b_open_robot.grid(row=2,column=2,padx=3,pady=5)
        self.pose_label.grid(row=3,column=0,padx=3,pady=5)
        self.pose_entry.grid(row=3,column=1,padx=3,pady=5)
        self.b_update_pose.grid(row=3,column=2,padx=3,pady=5)
        self.module_label.grid(row=4,column=0,padx=3,pady=5)
        self.module_entry.grid(row=4,column=1,padx=3,pady=5)
        self.b_open_module.grid(row=4,column=2,padx=3,pady=5)
        self.empty_label.grid(row=5,column=0,padx=3,pady=5)
        self.robot_gui_label.grid(row=6,column=0,padx=3,pady=5)
        self.robot_gui_combo.grid(row=6,column=1,padx=3,pady=5)
        self.b_path.grid(row=7,column=0,padx=3,pady=5,sticky="W")
        self.b_sensor.grid(row=8,column=0,padx=3,pady=5,sticky="W")
 
        self.figure = Figure(figsize=(5, 5),tight_layout=True)
        self.axes = self.figure.add_subplot(111)

        self.canvas = FigureCanvasTkAgg(self.figure, master=self.frame_plot)
        self.canvas.get_tk_widget().pack(fill=tk.BOTH,expand=tk.TRUE)
        NavigationToolbar2Tk(self.canvas, self.frame_nav)
        self.canvas.draw()

        self.W1 = world.World()
        self.W1.LoadWorld(os.getcwd()+"/worlds/"+self._world)
        self.W1.Plot(self.axes)

        self.min_corner, self.max_corner = self.W1.GetCorners()
        self.axes.set_xlim([self.min_corner[0],self.max_corner[0]])
        self.axes.set_ylim([self.min_corner[1],self.max_corner[1]])
        self.axes.set_aspect("equal")

        self.init_pose = np.fromstring(self._init_pose,sep=',')
        self.pose = self.init_pose

        self.topics = {}

        self.R1 = robot.Robot()

        self.dt_plot = 0.1
        self.t_run = 0

        self.real_path_x = []
        self.real_path_y = []
        self.est_path_x = []
        self.est_path_y = []

        self.pose_est = puzz_msgs.Pose()

        self.is_sim = True
        self.is_running = False

        self.queue_msgs = 0
        self.queue_topics = 0


    def on_open_world(self):
        filetypes = (('world files', '*.json'),('All files', '*.*'))
        self._world = fd.askopenfilename(title='Open a world file',initialdir=os.getcwd()+"/worlds",filetypes=filetypes)

        self._world = os.path.basename(self._world)
        self.world_entry.delete(0, 'end')
        self.world_entry.insert(tk.END, self._world)

        self.queue_msgs.put({"World": self._world})

        self.W1.LoadWorld(os.getcwd()+"/worlds/"+self._world)
        
    def on_open_robot(self):
        filetypes = (('robot files', '*.json'),('All files', '*.*'))
        self._robot = fd.askopenfilename(title='Open a robot file',initialdir=os.getcwd()+"/hardware",filetypes=filetypes)
        self._robot = os.path.basename(self._robot)
        self.robot_entry.delete(0, 'end')
        self.robot_entry.insert(tk.END, self._robot)

        self.queue_msgs.put({"Robot": self._robot})
        
    def on_open_module(self):
        filetypes = (('python files', '*.py'),('All files', '*.*'))
        self._module = fd.askopenfilename(title='Open a python file',initialdir=os.getcwd()+"/my_examples",filetypes=filetypes)
        self._module = os.path.basename(os.path.splitext(self._module)[0])
        self.module_entry.delete(0, 'end')
        self.module_entry.insert(tk.END, self._module)

        self.queue_msgs.put({"Module": self._module})

    def on_update_pose(self):
        self._init_pose = self.pose_entry.get()
        self.init_pose = np.fromstring(self._init_pose,sep=',')
        self.pose = self.init_pose
        self.R1.SetPose(self.pose)

        self.queue_msgs.put({"InitPose": self.init_pose})
        
    def on_play(self):
        if not self.is_running:
            self._ip = self.ip_entry.get()
            self._world = self.world_entry.get()
            self._module = self.module_entry.get()
            self._robot = self.robot_entry.get()
            self._init_pose = self.pose_entry.get()
            self.init_pose = np.fromstring(self._init_pose,sep=',')
            self.is_sim = not self._real.get()

            self.R1.Init(os.getcwd()+"/hardware/"+self._robot)
            self.R1.SetSim(self.is_sim)

            self.real_path_x.clear()
            self.real_path_y.clear()
            self.est_path_x.clear()
            self.est_path_y.clear()

            self.queue_msgs.put({"Ip": self._ip})
            self.queue_msgs.put({"IsSim": self.is_sim})
            self.queue_msgs.put({"World": self._world})
            self.queue_msgs.put({"Robot": self._robot})
            self.queue_msgs.put({"Module": self._module})
            self.queue_msgs.put({"InitPose": self.init_pose})
            self.queue_msgs.put({"Play": 0})

            self.is_running = True

    def on_stop(self):
        self.real_path_x.clear()
        self.real_path_y.clear()
        self.est_path_x.clear()
        self.est_path_y.clear()
        self.pose = self.init_pose
        #self.R1.SetPose(self.pose)
        self.t_run = 0.0
        self.topics = {}
        self.pose_est = puzz_msgs.Pose()
        self.queue_msgs.put({"Stop": 0})

        self.is_running = False

    def Update(self):
        
        while self.queue_topics.qsize()>0:
            if not self.queue_topics.empty():
                self.topics = self.queue_topics.get_nowait()

        if "PoseSim" in self.topics:
            self.pose = self.topics["PoseSim"]
            #self.R1.SetPose(self.pose)

        if "Pose" in self.topics:
            if self.is_sim:
                self.pose_est = self.topics["Pose"]
            else:
                self.pose_est = self.topics["Pose"]
                #self.pose = self.topics["Pose"].pose
                #self.R1.SetPose(self.topics["Pose"].pose)

        if "Time" in self.topics:
            self.t_run = self.topics["Time"]


        if self.is_running:
            self.real_path_x.append(self.pose[0])
            self.real_path_y.append(self.pose[1])
            self.est_path_x.append(self.pose_est.pose[0])
            self.est_path_y.append(self.pose_est.pose[1])

        x_lim = self.axes.get_xlim()
        y_lim = self.axes.get_ylim()
        self.axes.cla()
        self.axes.set_xlim(x_lim)
        self.axes.set_ylim(y_lim)

        #if "Map" in self.topics:
        #    plt.pcolor(self.topics["Map"],cmap=plt.get_cmap('Blues'),edgecolors=None, linewidths=1)
            
        self.W1.Plot(self.axes)
        self.R1.UpdateTopic(self.topics)
        self.R1.Plot(self.axes,self._sensor.get(),self.robot_gui_combo.get())

        if self._path.get():
            self.axes.plot(self.real_path_x,self.real_path_y,"r--")
            self.axes.plot(self.est_path_x,self.est_path_y,"g--")

            #self.axes.add_patch(patches.Circle((self.pose_est.pose[0],self.pose_est.pose[1]), 0.03,color="blue"))
            th1 = (self.pose_est.pose[2]-2.5*self.pose_est.cov[2][2])*180/3.1415
            th2 = (self.pose_est.pose[2]+2.5*self.pose_est.cov[2][2])*180/3.1415
            self.axes.add_patch(patches.Wedge(
                                (self.pose_est.pose[0],self.pose_est.pose[1]),         # (x,y)
                                0.3,                           # radius
                                th1,                                  # theta1 (in degrees)
                                th2,                                  # theta2
                                color="g", alpha=0.4
                                )
                            )

            my_math.plot_confidence_ellipse(self.pose_est.pose[0],
                                            self.pose_est.pose[1],
                                            self.pose_est.cov[0:2,0:2],
                                            self.axes,0.95,linewidth=1, fill=False, color='green')

        self.canvas.draw()

        self.txt_time.config(text=f"Time: {self.t_run:.2f}")
        self.txt_pose.config(text=f"Pose: {self.pose[0]:.2f}, {self.pose[1]:.2f}, {self.pose[2]:.2f}")

        self.toplevel.after(int(1000*self.dt_plot),self.Update)


    def LoadState(self):
        f = open(os.getcwd()+"/gui/state.json")   
        state_json = json.load(f)
        f.close()

        self.ip_entry.delete(0, 'end')
        self.ip_entry.insert(tk.END, state_json["ip"])
        self._ip = state_json["ip"]
        self.is_sim = not state_json["real"]
        if not self.is_sim:
            self._real.set(True)

        self.world_entry.delete(0, 'end')
        self.world_entry.insert(tk.END, state_json["world"])
        self._world = state_json["world"]

        self.robot_entry.delete(0, 'end')
        self.robot_entry.insert(tk.END, state_json["robot"])
        self._robot = state_json["robot"]

        self.pose_entry.delete(0, 'end')
        self.pose_entry.insert(tk.END, state_json["pose"])
        self._init_pose = self.pose_entry.get()

        self.module_entry.delete(0, 'end')
        self.module_entry.insert(tk.END, state_json["module"])
        self._module = state_json["module"]

        self._path.set(state_json["path"])
        self._sensor.set(state_json["sensor"])

        self.robot_gui_combo.set(state_json["robot_gui"])

    def SaveState(self):
        state = {
            "ip": self.ip_entry.get(),
            "world": self.world_entry.get(),
            "robot": self.robot_entry.get(),
            "pose": self.pose_entry.get(),
            "module": self.module_entry.get(),
            "real": self._real.get(),
            "path": self._path.get(),
            "sensor": self._sensor.get(),
            "robot_gui": self.robot_gui_combo.get()
        }
        state_json = json.dumps(state, indent=4)
 
        with open("gui/state.json", "w") as outfile:
            outfile.write(state_json)

    def on_closing(self):
        self.SaveState()
        self.queue_msgs.put({"Close": 0})
        self.toplevel.destroy()

    def signal_handler(self,sig, frame):
        print('You pressed Ctrl+C!')
        self.on_closing()
        sys.exit(0)

    def set_queues(self, queue_msgs, queue_topics):
        self.queue_msgs = queue_msgs
        self.queue_topics = queue_topics


def loop_robot(queue_msgs,queue_topics):
    main_loop = puzz_loop.PuzzLoop()

    signal.signal(signal.SIGINT, main_loop.signal_handler)

    #plt.ion()
    #fig = plt.figure(figsize=(6,6))
    #plt.show()

    exit = False

    while not exit:
        while not queue_msgs.empty():
            task = queue_msgs.get_nowait()
            if "Play" in task:
                main_loop.on_play()
            if "Stop" in task:
                main_loop.on_stop()
                queue_topics.put(main_loop.GetTopics())
            if "World" in task:
                main_loop.SetWorld(task["World"])
            if "Robot" in task:
                main_loop.SetRobot(task["Robot"])
            if "Module" in task:
                main_loop.SetModule(task["Module"])
            if "Ip" in task:
                main_loop.SetIp(task["Ip"])
            if "IsSim" in task:
                main_loop.SetSim(task["IsSim"])
            if "InitPose" in task:
                main_loop.SetInitPose(task["InitPose"])
            if "Close" in task:
                main_loop.on_closing()
                exit = True

        if not exit:
            main_loop.spin()
        
            if main_loop.is_running and not main_loop.is_done:
                queue_topics.put(main_loop.GetTopics())

        time.sleep(0.001)

    queue_msgs.close()
    queue_topics.close()
    return True

def main():
    queue_msgs = Queue()
    queue_topics = Queue()

    proc = Process(target=loop_robot, args=(queue_msgs,queue_topics))
    proc.start()

    window = tk.Tk()
    window.title("Puzzlebot GUI")


    puzz_gui = PuzzGui(window)

    signal.signal(signal.SIGINT, puzz_gui.signal_handler)

    puzz_gui.set_queues(queue_msgs,queue_topics)

    puzz_gui.Update()

    window.protocol("WM_DELETE_WINDOW", puzz_gui.on_closing)

    window.mainloop()

    time.sleep(0.5)

    queue_msgs.close()
    queue_topics.close()


if __name__ == '__main__':
    main()