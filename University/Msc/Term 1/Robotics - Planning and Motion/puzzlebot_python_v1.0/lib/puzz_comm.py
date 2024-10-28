import socket
import struct
import time
import errno
from lib import puzz_msgs

        
class PuzzComm():

    def __init__(self):
        
        self.buffer = bytearray()
        
        self.status = 0
        self.indx = 0
        self.type = 1
        self.topic = ""
        self.name_size = 0
        self.data_size = 0
        self.data_raw = bytearray();
        
        self.msgs = dict([])

        self.connected = False
        
        self.t_start = time.time()
                
    def Init(self, ip, port):
        self.ip = ip
        self.port = port

    def Connect(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.connect((self.ip,self.port))
        self.sock.setblocking(False)
        self.t_start = time.time()
        self.connected = True

    def Disconnect(self):
        if self.connected:
            self.sock.close()
            self.connected = False
        
    def SendMessage(self, msg):
        #print("Send message: "+msg.topic+"  ",msg.data)
        head_bytes = bytearray()
        data_bytes = bytearray()
        
        head_bytes.append(254)
        head_bytes.append(238)
        head_bytes.append(msg.type)
        head_bytes.extend(struct.pack("i", len(msg.topic)))
        
        data_bytes.extend(bytes(msg.topic, 'utf-8'))
        
        if msg.type == 1:
            head_bytes.extend(struct.pack("i", 8))
            data_bytes.extend(struct.pack("f", msg.data))
            data_bytes.extend(struct.pack("f", msg.stamp))  
        if msg.type == 2:
            head_bytes.extend(struct.pack("i", 8))
            data_bytes.extend(struct.pack("i", msg.data))
            data_bytes.extend(struct.pack("f", msg.stamp))  
        if msg.type == 3:
            head_bytes.extend(struct.pack("i", 4*len(msg.data)+8))
            data_bytes.extend(struct.pack('%sf' % len(msg.data), *msg.data))
            data_bytes.extend(struct.pack("f", msg.stamp))  
        if msg.type == 4:
            head_bytes.extend(struct.pack("i", 4*len(msg.data)+12))
            data_bytes.extend(struct.pack('%sf' % len(msg.data), *msg.data))
            data_bytes.extend(struct.pack("f", msg.stamp))  
            data_bytes.extend(struct.pack("f", msg.t_sampling))  
            
        #Checksums
        head_bytes.extend(struct.pack("i", sum(head_bytes[2:]))) 
        data_bytes.extend(struct.pack("i", sum(data_bytes)))


        self.sock.send(head_bytes)
        self.sock.send(data_bytes)

        
    def SpinOnce(self):
        exit = 0
        t = time.time()
        #print(self.sock.recvfrom(1,socket.MSG_PEEK))
        while exit == 0:
            try:
                data, server = self.sock.recvfrom(2048)
                self.buffer.extend(data)
            except socket.error as e:
                err = e.args[0]
                if err == errno.EAGAIN or err == errno.EWOULDBLOCK:
                    #print("No data available")
                    exit = 1
        #    if len(data)==0:
        #        exit = 1        
        
        #print(self.buffer)   
        #print(time.time() - t)
             
        exit = 0

        while exit==0:
            if self.status == 0:      # status == start
                if self.indx <= len(self.buffer)-1:
                    if self.buffer[self.indx] == 254 and self.buffer[self.indx+1] == 238:
                        self.indx = self.indx + 2
                        self.status = 1
                    else:
                        self.indx = self.indx + 1
                else:
                    exit = 1
            if self.status == 1:     # status == type
                if self.indx <= len(self.buffer):
                    self.type = self.buffer[self.indx]
                    self.indx = self.indx + 1
                    self.status = 2
                else:
                    exit = 1
            if self.status == 2:     # status == name_size
                if self.indx <= len(self.buffer)-3:
                    self.name_size = struct.unpack('i', self.buffer[self.indx:self.indx+4])[0]
                    self.indx = self.indx + 4
                    self.status = 3
                else:
                    exit = 1
            if self.status == 3:     # status == data_size
                if self.indx <= len(self.buffer)-3:
                    self.data_size = struct.unpack('i', self.buffer[self.indx:self.indx+4])[0]
                    self.indx = self.indx + 4
                    self.status = 4
                else:
                    exit = 1
            if self.status == 4:     # status == checksum1
                if self.indx <= len(self.buffer)-3:
                    msg_checksum = struct.unpack('i', self.buffer[self.indx:self.indx+4])[0]
                    checksum = self.type + sum(bytearray(struct.pack("i", self.name_size))) + sum(bytearray(struct.pack("i", self.data_size)))
                    self.indx = self.indx + 4
                    if msg_checksum==checksum:
                        self.status = 5
                    else:
                        self.status = 0
                        print("Message Lost")
                else:
                    exit = 1
            if self.status == 5:     # status == name
                if self.indx <= len(self.buffer)-self.name_size+1:
                    self.topic = self.buffer[self.indx:self.indx+self.name_size].decode("utf-8")
                    self.indx = self.indx + self.name_size
                    self.status = 6
                else:
                    exit = 1
            if self.status == 6:     # status == data
                if self.indx <= len(self.buffer)-self.data_size+1:
                    self.data_raw = self.buffer[self.indx:self.indx+self.data_size]
                    self.indx = self.indx + self.data_size
                    self.status = 7
                else:
                    exit = 1
            if self.status == 7:     # status == checksum2
                if self.indx <= len(self.buffer)-3:
                    msg_checksum = struct.unpack('i', self.buffer[self.indx:self.indx+4])[0]
                    checksum = sum(self.topic.encode('utf-8')) + sum(self.data_raw)
                    self.indx = self.indx + 4
                    if msg_checksum==checksum:
                        self.UpdateTopic()
                    else:
                        print("Message Lost")
                    self.status = 0
                else:
                    exit = 1
                    
            self.buffer = self.buffer[self.indx:]
            self.indx = 0

        
    def UpdateTopic(self):
        if self.type == 1:
            msg = puzz_msgs.Float32()
            msg.data = struct.unpack('f', self.data_raw[0:4])[0]
            msg.stamp = struct.unpack('f', self.data_raw[4:8])[0]
            msg.status = 1
            
            self.msgs[self.topic] = msg
            #print(self.topic,"  ",msg.data)
            #print(time.time() - self.t_start)

    def GetTopicMsg(self, topic):
        ret = -1
        if topic in self.msgs:
            if self.msgs[topic].type == 1:
                #ret = Float32()
                ret = self.msgs[topic]
                self.msgs[topic].status = 0   
                return ret, True        
        return ret, False


