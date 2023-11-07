# Copyright 2019, Imperial College London
# 
# Tutorial for CO416 - Machine Learning for Imaging
#
# This file: Functions for feedforward neural nets.

import torch

def torch_repeat(in_tens, repeats, axis):
    # Repeat FM in the 2 last dimensions, to upsample back to the normal resolution space.
    # in_tens: [batch size, num of FMs, H, W]. Input/output of conv layers.
    # repeat_per_dim: [repetitions axis H, repetitions axis W]
    # returns: Tensor of size: [batch size, num of FMs, H*repeat[0], W*repeat[1] ]
    
    # NOTE: Exactly the same API and behaviour as numpy.repeat(...)
    # Because torch.repeat is similar to numpy.tile(...), not what we want.
    # See: https://stackoverflow.com/questions/35227224/torch-repeat-tensor-like-numpy-repeat
    tens = in_tens
    assert axis>0
    # Combine dimension to previous
    shape_flat = list(in_tens.shape)
    shape_flat[axis-1] = in_tens.shape[axis-1]*in_tens.shape[axis]
    shape_flat[axis] = 1
    tens_flat = tens.reshape( shape_flat )
    # Tile
    repeat_per_dim = [1]*len(in_tens.shape)
    repeat_per_dim[axis] = repeats
    tens_rep_flat = tens_flat.repeat( repeat_per_dim ) # This is what numpy.tile(...) does.
    # Reshape to what it should be.
    shape_result = list(in_tens.shape)
    shape_result[axis] = in_tens.shape[axis]*repeats
    tens_rep = tens_rep_flat.reshape( shape_result )
    return tens_rep

def torch_repeat_2_last_dims(in_tens, repeat_per_dim):
    # Repeat FM in the 2 last dimensions, to upsample back to the normal resolution space.
    # in_tens: [batch size, num of FMs, H, W]. Input/output of conv layers.
    # repeat_per_dim: [repetitions axis H, repetitions axis W]
    # returns: Tensor of size: [batch size, num of FMs, H*repeat[0], W*repeat[1] ]
    
    # NOTE: Similar behaviour to numpy.repeat(...)
    # Because torch.repeat is similar to numpy.tile(...), not what we want.
    # See: https://stackoverflow.com/questions/35227224/torch-repeat-tensor-like-numpy-repeat
    tens = in_tens
    
    tens_shape = in_tens.shape
    tens_resh = tens.reshape( [tens_shape[0], tens_shape[1]*tens_shape[2], 1, tens_shape[3]] )
    tens_rep = tens_resh.repeat( 1, 1, repeat_per_dim[0], 1 )
    tens = tens_rep.reshape( [tens_shape[0], tens_shape[1], tens_shape[2]*repeat_per_dim[0], tens_shape[3]] )
    
    tens_shape = tens.shape
    tens_resh = tens.reshape( [tens_shape[0], tens_shape[1], tens_shape[2]*tens_shape[3], 1] )
    tens_rep = tens_resh.repeat( [1, 1, 1, repeat_per_dim[1]] )
    tens = tens_rep.reshape( [tens_shape[0], tens_shape[1], tens_shape[2], tens_shape[3]*repeat_per_dim[1]] )
    return tens



def grad_of_xentropy_wrt_input_to_softmax_pytorch(y_pred,
                                                  y_real,
                                                  batch_size,
                                                  num_classes,
                                                  verbose=False):
    # y_pred: Class posteriors. Pytorch tensor of floats, of shape: [batch_size, number_of_classes]
    # y_real: True labels for training, given in one-hot representation. Same shape as y_pred
    # Returns: grad_input, a tensor of shape [N, D_out]
    
    # Derivative of cross-entropy loss with respect to input of softmax.
    N = batch_size
    D_out = num_classes
    
    # Derivative of loss wrt y_pred
    grad_y_pred = - (1./batch_size) * y_real/y_pred # division is elemwise.
    if verbose: print("[grad_y_pred] shape: ", grad_y_pred.shape)
    
    # Compute Jacobian of Softmax. i.e., derivaties of each output of SM wrt each input of SM.
    # Useful info: https://stackoverflow.com/questions/40575841/numpy-calculate-the-derivative-of-the-softmax-function
    diag_ones = torch.eye(n=D_out, m=D_out) # identity matrix, shape [D_out, D_out]
    diag_ones = diag_ones.reshape([1, D_out, D_out])
    diag_ones = diag_ones.repeat(N, 1, 1) # diag_ones = diag_ones.repeat(N, axis=0)
    if verbose: print("[diag_ones] shape: ", diag_ones.shape)
    repeat_y_pred = y_pred.reshape(N, D_out, 1) # Add one more dimension, to repeat along it.
    repeat_y_pred = repeat_y_pred.repeat(1,1, D_out) # Now a [N, D_out, D_out] matrix
    if verbose: print("[repeat_y_pred] shape: ", repeat_y_pred.shape)
    diag_y_pred = diag_ones * repeat_y_pred # [N, D_out, D_out], where each [n,:,:] submatrix is diagonal and y_pred its values.
    if verbose: print("[diag_y_pred] shape: ", diag_y_pred.shape)
    # Jacobian of SM = np.diag( SM ) - np.dot( SM, SM.T ): Symmetric matrix of shape [D_out, D_in]. https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant
    batch_jacobian = - torch.matmul( y_pred.reshape((N, D_out, 1)), y_pred.reshape((N, 1, D_out)) ) # batch matmul: https://pytorch.org/docs/stable/torch.html#torch.matmul
    batch_jacobian =  batch_jacobian + diag_y_pred
    if verbose: print("[batch_jacobian] shape: ", batch_jacobian.shape) # [ N, D_out, D_out ]
        
    # Compute derivative of loss wrt each input of SM.
    grad_input = torch.matmul( grad_y_pred.reshape(N, 1, D_out), batch_jacobian)  # [N, D_out] * [N, D_out, D_out]
    grad_input = grad_input.reshape((N, D_out)) # remove the singleton dimension.
    if verbose: print("[grad_input] shape: ", grad_input.shape)
    
    return grad_input

def grad_of_xentropy_wrt_input_to_softmax_simplified_pytorch( y_pred,
                                                              y_real,
                                                              batch_size,
                                                              num_classes,
                                                              verbose=False):
    # Arguments and return values similar to: grad_of_xentropy_wrt_input_to_softmax_pytorch()
    # Difficult derivation. Nice explanation at: https://deepnotes.io/softmax-crossentropy
    grad_input = (y_pred - y_real) / batch_size
    return grad_input