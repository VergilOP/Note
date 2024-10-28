import json
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import math
from lib import my_math
        
class World():

    def __init__(self):

        self.min_corner = [-5,-5]
        self.max_corner = [5,5]

        self.arrow_size = 0.3

    def LoadWorld(self, world_file):
        f = open(world_file)
        
        self.world_json = json.load(f)
        
        f.close()

        self.min_corner = self.world_json["min_corner"]
        self.max_corner = self.world_json["max_corner"]

        self.lines = []
        self.circles = []
        for shape in self.world_json["shapes"]:
            if shape["type"] == "polygon":
                for i in range(len(shape["x_points"])-1):
                    self.lines.append(my_math.Line2D(shape["x_points"][i],
                                                  shape["y_points"][i],
                                                  shape["x_points"][i+1],
                                                  shape["y_points"][i+1]))
                self.lines.append(my_math.Line2D(shape["x_points"][len(shape["x_points"])-1],
                                              shape["y_points"][len(shape["x_points"])-1],
                                              shape["x_points"][0],
                                              shape["y_points"][0]))
            if shape["type"] == "rectangle":
                x1 = shape["x_points"][0]
                x2 = shape["x_points"][1]
                y1 = shape["y_points"][0]
                y2 = shape["y_points"][1]
                self.lines.append(my_math.Line2D(x1,y1,x1,y2))
                self.lines.append(my_math.Line2D(x1,y2,x2,y2))
                self.lines.append(my_math.Line2D(x2,y2,x2,y1))
                self.lines.append(my_math.Line2D(x2,y1,x1,y1))
            if shape["type"] == "circle":
                self.circles.append(my_math.Circle(shape["center"][0],shape["center"][1],shape["radius"]))

    def GetCorners(self):
        return self.min_corner, self.max_corner
    
    def SetCorners(self,min_corner, max_corner):
        self.min_corner = min_corner
        self.max_corner = max_corner
    
    def Plot(self,axes):

        for shape in self.world_json["shapes"]:
            if shape["type"] == "polygon":
                axes.fill(shape["x_points"],shape["y_points"])

            if shape["type"] == "circle":
                axes.add_patch(patches.Circle(shape["center"], shape["radius"]))

            if shape["type"] == "rectangle":
                x1 = shape["x_points"][0]
                x2 = shape["x_points"][1]
                y1 = shape["y_points"][0]
                y2 = shape["y_points"][1]
                axes.add_patch(patches.Rectangle((x1,y1),x2-x1,y2-y1))

            if shape["type"] == "marker_aruco":
                x1 = shape["coordinates"][0]
                y1 = shape["coordinates"][1]
                dx = self.arrow_size*math.cos(shape["angle"])
                dy = self.arrow_size*math.sin(shape["angle"])
                axes.add_patch(patches.Arrow(x1,y1,dx,dy,width=0.2))
                axes.add_patch(patches.Circle((x1,y1), 0.05,color="red"))

            if shape["type"] == "marker_bearing":
                x1 = shape["coordinates"][0]
                y1 = shape["coordinates"][1]
                axes.add_patch(patches.Circle((x1,y1), 0.05,color="red"))

