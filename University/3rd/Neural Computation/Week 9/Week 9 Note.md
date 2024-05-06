# Week 9 Note

## Neurons with Recurrence(intuitions of RNNs)

![](./images/Screenshot%202024-04-18%20183519.png)

![](./images/Screenshot%202024-04-18%20183638.png)

![](./images/Screenshot%202024-04-18%20190520.png)

![](./images/Screenshot%202024-04-18%20190601.png)

## Vanilla Recurrent Neural Networks(RNNs)

![](./images/Screenshot%202024-04-18%20190629.png)

![](./images/Screenshot%202024-04-18%20190740.png)

![](./images/Screenshot%202024-04-18%20190912.png)

![](./images/Screenshot%202024-04-18%20191049.png)

### Sequence Modeling: Design Criteria
- To model sequences, we need to:
  1. Handle variable-length sequences
  2. Track log-term dependencies
  3. Maintain information about order
  4. Share parameters across the sequence

- Recurrent Neural Networks(RNNs) meet these sequence modeling design criteria

## Sequence Modeling Problem: Predict the Next Word

![](./images/Screenshot%202024-04-18%20191519.png)

## Backpropagation Through Time(BPTT)

![](./images/Screenshot%202024-04-18%20222158.png)

![](./images/Screenshot%202024-04-18%20222732.png)

![](./images/Screenshot%202024-04-18%20222759.png)

![](./images/Screenshot%202024-04-18%20224259.png)

### Trick 1: Activation Functions

![](./images/Screenshot%202024-04-19%20211447.png)

### Trick 2: Parameter Initilization

![](./images/Screenshot%202024-04-19%20211507.png)

### Solution 3: Gated Cells

![](./images/Screenshot%202024-04-19%20211547.png)

## Long Short Term Memory(LSTM) Networks

![](./images/Screenshot%202024-04-19%20211606.png)

![](./images/Screenshot%202024-04-19%20211616.png)

![](./images/Screenshot%202024-04-19%20211625.png)

1. Forget
  - LSTMs forget irrelevant parts of the previous state
2. Store
  - LSTMs store relevant new information into the cell state
3. Update
  - LSTMs selectively update cell state values
4. Output
  - The output gate controls what information is sent to the next time step

![](./images/Screenshot%202024-04-19%20211636.png)

### Key Concepts

- Maintain a **separate cell state** from what is outputted
- Use **gates** to control the **flow of information**
  - **Forget** gate gets rid of irrelevant information
  - **Store** relevant information from current input
  - Selective **update** cell state
  - **Output** gate returns a filtered version of the cell state
- Backpropagation through time with **uniterrupted gradient flow**

## RNN Applications

- Deep Learning for Sequence Modeling: Summary
  - RNNs are well suited for **sequence modeling** tasks
  - Model sequences via a **recurrence relation**
  - Traning RNNs with **backpropagation through time**
  - Gated cells like *LSTMs* let us model **long-term dependencies**
