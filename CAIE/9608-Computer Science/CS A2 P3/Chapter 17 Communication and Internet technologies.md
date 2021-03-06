# Table of contents

- [Communication and Internet technologies](#communication-and-internet-technologies)
  - [Why `protocols` are essential for communication between computers \[2\]](#why-protocols-are-essential-for-communication-between-computers-2)
  - [Describe the `TCP/IP` protocol suite \[5\]](#describe-the-tcpip-protocol-suite-5)
  - [Identify the four layers of the `TCP/IP` protocol suite \[4\]](#identify-the-four-layers-of-the-tcpip-protocol-suite-4)
  - [Identify the most appropriate `TCP/IP` protocol for sharing this file over the Internet and describe the way this protocol works \[5\]](#identify-the-most-appropriate-tcpip-protocol-for-sharing-this-file-over-the-internet-and-describe-the-way-this-protocol-works-5)
  - [Describe `circuit switching` \[3\]](#describe-circuit-switching-3)
  - [Why the company uses `circuit switching` for voice calls \[2\]](#why-the-company-uses-circuit-switching-for-voice-calls-2)
  - [Describe `packet switching` \[3\]](#describe-packet-switching-3)
  - [Why the company uses `packet switching` to send and receive other data \[2\]](#why-the-company-uses-packet-switching-to-send-and-receive-other-data-2)
  - [Describe the purpose of a `packet header` \[2\]](#describe-the-purpose-of-a-packet-header-2)
  - [Identify **three** items that should be contained in a `packet header` \[3\]](#identify-three-items-that-should-be-contained-in-a-packet-header-3)
  - [Explain the benefits to the admissions department of using a `star topology` \[4\]](#explain-the-benefits-to-the-admissions-department-of-using-a-star-topology-4)
  - [Explain how the following devices are used to support the university `LAN`](#explain-how-the-following-devices-are-used-to-support-the-university-lan)
  - [Explain what is meant by `CSMA/CD` \[4\]](#explain-what-is-meant-by-csmacd-4)

Communication and Internet technologies
---------------------------------------

### Why `protocols` are essential for communication between computers \[2\]
> w19_33_Q3

- Provide a set of standards for transmission of data
- ...that gives a known/accepted set of rules for transmitting and receiving data
- This enables communication between devices from different manufactures

### Describe the `TCP/IP` protocol suite \[5\]
> w20_33_Q3
>
> 4 layer, use protocols, transmission of data, control protocol with internet protocol, four names

- ...with 4 layers
- Uses a set of protocols for
- A layered model/stack transmission of data
- ...transport control protocol with internet protocol
- Application Layer, Transport Layer, Network Layer, Data Link Layer

### Identify the four layers of the `TCP/IP` protocol suite \[4\]
> w19_33_Q7

- Application
- Transport
- Internet/Network
- Data Link

### Identify the most appropriate `TCP/IP` protocol for sharing this file over the Internet and describe the way this protocol works \[5\]
> w20_33_Q3
>
> Key: available, split into pieces, torrent descriptor file, piece -> seed, download and upload, records in the swarm, connection to each other

- BitTorrent
- BitTorrent client software made available to friends and family's computers
- a complete copy of the file to be shared is available on at least one computer
- the file is split into small pieces...
- ... rare pieces are given priority for downloading
- the torrent descriptor is made available
- a computer joins by using the BitTorrent software to load the torrent descriptor file
- the computer can now download a piece of the file
- once a computer has a piece, it can become a seed
- pieces of the file are both downloaded and uploaded
- a server called a tracker keeps records of all the computers in the swarm
- the tracker shares their IP addresses allowing connection to each other

### Describe `circuit switching` \[3\]
> s20_33_Q3
>
> Establish start, sender to receiver, last duration, remove end

- A circuit is established at the start of the communication
- Between sender and receiver
- This lasts for the duration of the call/data transfer
- Then the links that make up the circuit are removed

### Why the company uses `circuit switching` for voice calls \[2\]
> s20_33_Q3
>
> dedicate, all bandwidth, two-way real time, no delay, in order

- A dedicated channel
- ... can use all bandwidth
- Two-way real time conversation
- No delay as no switching
- Data arrives in order it is sent

### Describe `packet switching` \[3\]
> s20_33_Q3
>
> not establish begin, data into packets, travel different routes, node to node, reassemble in end, wait for last

- A circuit does not have to be established at the start of the communication
- The data to be sent is divided into packets
- That can travel along different routes
- From node to node
- Packets are reassembled in the correct order at the receiver's end
- Must wait until the last packet is received to put the data back together

### Why the company uses `packet switching` to send and receive other data \[2\]
> s20_33_Q3
>
> asynchronous, error checking, no need real time, small data, no need in order

- Communication is asynchronous
- Allows for error checking
- Real time transmission is not required
- Smaller amounts of data are sent (than voice calls) therefore dedicated line/higher bandwidth not required
- Doesn't matter if data arrives out of order

### Describe the purpose of a `packet header` \[2\]
> w19_33_Q7

- To store data about packet
- ...and its routing
- ...to ensure that message can be properly reconstructed

### Identify **three** items that should be contained in a `packet header` \[3\]
> w19_33_Q7

- IP address of sender
- IP address of destination
- IP version
- Number of packets the message consists of
- ID number of that packet
- Protocol length
- Time to live
- Synchronisation data
- Source port
- Destination Port
- Checksum

### Explain the benefits to the admissions department of using a `star topology` \[4\]
> s19_33_Q2

- Personal data is kept secure
- ...transmissions only go between server and destination
- A new device/employee can be easily added to the network
- ...only one connection direct to server needs setting up
- If one node or link fails
- ...the other employees can continue working
- If the department has a range of different devices
- ...they can all operate at different speeds or with different protocols

### Explain how the following devices are used to support the university `LAN`
> s19_33_Q2

- ***Router*** \[2\]
  - Allow (internal) connections between the university LANs
  - Allow external connection from the main LAN

- ***Network Interface Card(NIC)*** \[2\]
  - Provides device with a MAC address
  - ...to uniquely identify it on the network
  - Allows each individual device to connect to the network

- ***Wireless Access Point*** \[2\]
  - Allowing devices to connect to the LAN via radio communication
  - ...instead of using a cable
  - ...easy to move a device to as different location

### Explain what is meant by `CSMA/CD` \[4\]
> w19_33_Q3
> s19_33_Q2
>
> Full name, check busy when transmission, if busy wait, when transmission listen for other, if collision a jam signal, both wait try again

- Carrier Sense Multiple Access (with) Collision Detection
- Before transmitting a device checks if the channel is busy
- If it is busy the device waits
- When transmission begins the device listens for other devices also beginning transmission
- If there is a collision, transmission is aborted
- Both devices wait a random time, then try again
