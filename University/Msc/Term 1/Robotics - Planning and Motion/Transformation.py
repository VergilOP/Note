from sympy import symbols, cos, sin, pi, sqrt, Matrix

def dh_transform_matrix_correct(a, alpha, theta, d):
    # Convert degrees to radians for symbolic calculations
    alpha_rad = alpha * pi / 180
    theta_rad = theta * pi / 180

    # Construct the transformation matrix
    T = Matrix([
        [cos(theta_rad), -sin(theta_rad) , 0 , a],
        [sin(theta_rad) * cos(alpha_rad), cos(theta_rad) * cos(alpha_rad), -sin(alpha_rad), - sin(alpha_rad) * d],
        [sin(theta_rad) * sin(alpha_rad), cos(theta_rad) * sin(alpha_rad), cos(alpha_rad), cos(alpha_rad) * d],
        [0, 0, 0, 1]
    ])
    
    return T

# Input parameters
a = 0.5
alpha = 90
theta = 30
d = 0.2

# Calculate the transformation matrix
T_correct = dh_transform_matrix_correct(a, alpha, theta, d)

# Display the result
print(T_correct)