import torch
import torchvision
import torchvision.transforms as transforms
from torchvision import datasets
from torch.utils.data import DataLoader
import os
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim

def main():

    # 更改当前工作目录
    os.chdir('C:/_Study_Resource/Study_Note/University/3rd/Neural Computation/Code')
    # print(os.getcwd())

    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
    print(f"The current device is {device}")

    data_transforms = {
        'train': transforms.Compose([
            transforms.RandomResizedCrop(224),   # Randomly crop and resize to 224x224
            transforms.RandomHorizontalFlip(),
            transforms.RandomRotation(10),
            transforms.ToTensor(),               # Convert image to tensor
            transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])  # Normalize using the mean and std for pretrained models
        ]),
        'test': transforms.Compose([
            transforms.Resize(256),              # Resize the shorter side to 256
            transforms.CenterCrop(224),          # Center crop to 224x224
            transforms.ToTensor(),
            transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])
        ]),
    }

    data_dir = 'CatsAndDogsImageClassification'
    image_datasets = {
        x: datasets.ImageFolder(os.path.join(data_dir, x), data_transforms[x])
        for x in ['train', 'test']
    }

    dataloaders = {
        x: DataLoader(image_datasets[x], batch_size=32, shuffle=True, num_workers=4)
        for x in ['train', 'test']
    }

    import matplotlib.pyplot as plt
    import numpy as np

    # functions to show an image
    def imshow(img):
        img = img / 2 + 0.5     # unnormalize
        npimg = img.numpy()
        plt.imshow(np.transpose(npimg, (1, 2, 0)))
        plt.show()

    # Assuming you have classes as ['cats', 'dogs']
    classes = ['cats', 'dogs']

    # get some random training images
    dataiter = iter(dataloaders['train'])
    images, labels = next(dataiter)

    # show images
    imshow(torchvision.utils.make_grid(images))
    # print labels
    print(' '.join('%5s' % classes[labels[j]] for j in range(32)))  # assuming batch_size is 32

    class CatDogNet(nn.Module):
        def __init__(self):
            super(CatDogNet, self).__init__()
            self.conv1 = nn.Conv2d(3, 32, 3, padding=1)
            self.conv2 = nn.Conv2d(32, 64, 3, padding=1)
            self.conv3 = nn.Conv2d(64, 128, 3, padding=1)
            self.pool = nn.MaxPool2d(2, 2)
            self.fc1 = nn.Linear(28 * 28 * 128, 512)
            self.fc2 = nn.Linear(512, 2)  # 两个输出，对应猫和狗
            self.dropout = nn.Dropout(0.25)

        def forward(self, x):
            x = self.pool(F.relu(self.conv1(x)))
            x = self.pool(F.relu(self.conv2(x)))
            x = self.pool(F.relu(self.conv3(x)))
            x = torch.flatten(x, 1) 
            x = self.dropout(x)
            x = F.relu(self.fc1(x))
            x = self.dropout(x)
            x = self.fc2(x)  # 使用fc2，而不是fc3
            return x


    net = CatDogNet().to(device)

    criterion = nn.CrossEntropyLoss()
    optimizer = optim.SGD(net.parameters(), lr=0.001, momentum=0.9)

    num_epochs = 10  # Number of times to loop over the entire dataset. Change as needed.
    print_every = 50  # Print after every 50 mini-batches. Adjust this value based on your preference.

    for epoch in range(num_epochs):  # Loop over the dataset multiple times.

        running_loss = 0.0
        for i, (inputs, labels) in enumerate(dataloaders['train']):
            # Move inputs and labels to the current device
            inputs, labels = inputs.to(device), labels.to(device)

            # Zero the parameter gradients
            optimizer.zero_grad()

            # Forward pass
            outputs = net(inputs)
            loss = criterion(outputs, labels)

            # Backward pass and optimization
            loss.backward()
            optimizer.step()

            # Print statistics - epoch and loss
            running_loss += loss.item()
            if (i + 1) % print_every == 0:  # Print every 'print_every' mini-batches
                print(f"Epoch [{epoch + 1}/{num_epochs}], Step [{i + 1}/{len(dataloaders['train'])}], Loss: {running_loss / print_every:.4f}")
                running_loss = 0.0

    print('Finished Training')

    PATH = './catdog_net.pth'
    torch.save(net.state_dict(), PATH)

    # 获取一些随机的测试图像
    dataiter = iter(dataloaders['test'])
    images, labels = next(dataiter)
    
    # 展示图像
    imshow(torchvision.utils.make_grid(images))
    print('GroundTruth: ', ' '.join('%5s' % classes[labels[j]] for j in range(4)))
    
    # 加载之前训练的模型
    net = CatDogNet()
    net.load_state_dict(torch.load(PATH))
    net = net.to(device)
    
    # 对上面的随机图像进行预测
    images = images.to(device)
    labels = labels.to(device)
    outputs = net(images)
    _, predicted = torch.max(outputs, 1)
    
    print('Predicted: ', ' '.join('%5s' % classes[predicted[j]] for j in range(4)))
    
    # 在整个测试数据集上测试模型的准确性
    correct = 0
    total = 0
    with torch.no_grad():
        for images, labels in dataloaders['test']:
            images = images.to(device)
            labels = labels.to(device)
    
            outputs = net(images)
            _, predicted = torch.max(outputs.data, 1)
            total += labels.size(0)
            correct += (predicted == labels).sum().item()
    
    print('Accuracy of the network on the test images: %d %%' % (100 * correct / total))
    
    # 为每个类别准备计数预测
    correct_pred = {classname: 0 for classname in classes}
    total_pred = {classname: 0 for classname in classes}
    
    with torch.no_grad():
        for images, labels in dataloaders['test']:
            images = images.to(device)
            labels = labels.to(device)
            
            outputs = net(images)
            _, predictions = torch.max(outputs, 1)
            
            for label, prediction in zip(labels, predictions):
                if label == prediction:
                    correct_pred[classes[label]] += 1
                total_pred[classes[label]] += 1
    
    # 打印每个类别的准确率
    for classname, correct_count in correct_pred.items():
        accuracy = 100 * float(correct_count) / total_pred[classname]
        print("Accuracy for class {:5s} is: {:.1f} %".format(classname, accuracy))

if __name__ == '__main__':
    main()

