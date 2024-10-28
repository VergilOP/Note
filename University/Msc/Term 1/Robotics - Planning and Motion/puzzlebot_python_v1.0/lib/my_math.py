
import math          
import numpy as np
import matplotlib.patches as patches
import matplotlib.pyplot as plt

class Point2D():
    def __init__(self,x,y):
        self.x = x
        self.y = y

class Line2D():
    def __init__(self,x1,y1,x2,y2):
        self.p1 = Point2D(x1,y1)
        self.p2 = Point2D(x2,y2)
        self.n = Point2D(y2-y1,x2-x1)

        norm = math.sqrt(self.n.x**2+self.n.y**2)

        if norm>0:
            self.n.x = self.n.x/norm
            self.n.y = self.n.y/norm

        self.d = -self.n.x*self.p1.x-self.n.y*self.p1.y

class Circle():
    def __init__(self,cx,cy,r):
        self.cx = cx
        self.cy = cy
        self.r = r

def GetLineIntersection(line1,line2):
    p_i = Point2D(0.0,0.0)

    s1_x = line1.p2.x - line1.p1.x
    s1_y = line1.p2.y - line1.p1.y
    s2_x = line2.p2.x - line2.p1.x
    s2_y = line2.p2.y - line2.p1.y

    s = (-s1_y * (line1.p1.x - line2.p1.x) + s1_x * (line1.p1.y - line2.p1.y)) / (-s2_x * s1_y + s1_x * s2_y)
    t = ( s2_x * (line1.p1.y - line2.p1.y) - s2_y * (line1.p1.x - line2.p1.x)) / (-s2_x * s1_y + s1_x * s2_y)

    if s >= 0 and s <= 1 and t >= 0 and t <= 1:
        p_i.x = line1.p1.x + (t * s1_x)
        p_i.y = line1.p1.y + (t * s1_y)
        return True,p_i
    
    return False,p_i

def GetLineCircleIntersection(circle,line):
    p_i = Point2D(0.0,0.0)
    dx = line.p2.x - line.p1.x
    dy = line.p2.y - line.p1.y

    A = dx * dx + dy * dy
    B = 2 * (dx * (line.p1.x - circle.cx) + dy * (line.p1.y - circle.cy))
    C = (line.p1.x - circle.cx)**2 + (line.p1.y - circle.cy)**2 - circle.r**2

    det = B**2 - 4*A*C

    if A <= 0.0000001 or det < 0:
        return False, p_i
    elif det == 0:
        t = -B / (2 * A)
        p_i.x = line.p1.x + t * dx
        p_i.y = line.p1.y + t * dy
        return True, p_i
    else:
        inters = []
        t1 = (-B + math.sqrt(det)) / (2 * A)
        t2 = (-B - math.sqrt(det)) / (2 * A)
        if 0 <= t1 <= 1:
            p_i.x = line.p1.x + t1 * dx
            p_i.y = line.p1.y + t1 * dy
            dist1 = math.sqrt((t1*dx)**2+(t1*dy)**2)
            inters.append(p_i)
        if 0 <= t2 <= 1:
            p_i.x = line.p1.x + t2 * dx
            p_i.y = line.p1.y + t2 * dy
            dist2 = math.sqrt((t2*dx)**2+(t2*dy)**2)
            inters.append(p_i)

        if len(inters) == 2:
            if dist1<dist2:
                return True,inters[0]
            else:
                return True,inters[1]
        elif len(inters) == 1:
            return True,inters[0]
        
        return False, p_i

           
def line_circle_intersection(x1, y1, x2, y2, cx, cy, r):
    # Calculate the coefficients for the quadratic equation at^2 + bt + c = 0
    dx = x2 - x1
    dy = y2 - y1
    fx = x1 - cx
    fy = y1 - cy
    
    a = dx**2 + dy**2
    b = 2 * (fx * dx + fy * dy)
    c = (fx**2 + fy**2) - r**2
    
    # Calculate the discriminant
    discriminant = b**2 - 4 * a * c
    
    # If discriminant is negative, no intersection
    if discriminant < 0:
        return []  # No intersection
    
    # Otherwise, there can be one or two intersections
    discriminant = math.sqrt(discriminant)
    
    # Solve the quadratic equation for t
    t1 = (-b - discriminant) / (2 * a)
    t2 = (-b + discriminant) / (2 * a)
    
    intersections = []
    
    # Check if the intersections are within the bounds of the line segment
    if 0 <= t1 <= 1:
        ix1 = x1 + t1 * dx
        iy1 = y1 + t1 * dy
        intersections.append((ix1, iy1))
    
    if 0 <= t2 <= 1:
        ix2 = x1 + t2 * dx
        iy2 = y1 + t2 * dy
        intersections.append((ix2, iy2))

    if len(intersections) == 2:
        dist1 = math.sqrt((t1*dx)**2+(t1*dy)**2)
        dist2 = math.sqrt((t2*dx)**2+(t2*dy)**2)
    
    return intersections



def truncated_remainder(dividend, divisor):
    divided_number = dividend / divisor
    divided_number = \
        -int(-divided_number) if divided_number < 0 else int(divided_number)

    remainder = dividend - divisor * divided_number

    return remainder
    
def wrap_to_pi(input_angle):
    p1 = truncated_remainder(input_angle + np.sign(input_angle) * math.pi, 2 * math.pi)
    p2 = (np.sign(np.sign(input_angle)
                  + 2 * (np.sign(abs((truncated_remainder(input_angle + math.pi, 2 * math.pi))
                                      / (2 * math.pi))) - 1))) * math.pi

    output_angle = p1 - p2

    return output_angle
    
def plot_confidence_ellipse(x, y, covariance, ax=None, confidence=0.95, **kwargs):
    """
    Plots a confidence ellipse based on a 2D array covariance using a chi-square distribution.

    Parameters:
        covariance (ndarray): The 2D covariance array.
        ax (Axes, optional): The axes on which to plot the ellipse. If None, a new figure and axes will be created.
        confidence (float, optional): The desired confidence level for the ellipse (default: 0.95).
        **kwargs: Additional keyword arguments to pass to the matplotlib Ellipse patch.

    Returns:
        matplotlib.patches.Ellipse: The ellipse object.

    """
    if ax is None:
        fig, ax = plt.subplots()

    # Calculate the eigenvalues and eigenvectors of the covariance matrix
    eigenvalues, eigenvectors = np.linalg.eigh(covariance)

    # Sort the eigenvalues and eigenvectors in descending order
    order = eigenvalues.argsort()[::-1]
    eigenvalues = eigenvalues[order]
    eigenvectors = eigenvectors[:, order]

    # Calculate the angle between the x-axis and the first eigenvector
    angle = np.degrees(np.arctan2(eigenvectors[1, 0], eigenvectors[0, 0]))

    # Calculate the chi-square threshold based on the desired confidence level
    #dof = 2  # Degrees of freedom for a 2D ellipse
    chi2_threshold = 2.5 #np.sqrt(scipy.stats.chi2.ppf(confidence, dof))

    # Create the ellipse
    ellipse = patches.Ellipse((x, y), 2 * chi2_threshold * np.sqrt(eigenvalues[0]), 2 * chi2_threshold * np.sqrt(eigenvalues[1]), angle=angle, **kwargs)

    # Add the ellipse to the plot
    ax.add_patch(ellipse)


