- [Security and Networks Note](#security-and-networks-note)
  - [Cryptography](#cryptography)
    - [Codes Versus Ciphers](#codes-versus-ciphers)
    - [Symmetric Cryptography](#symmetric-cryptography)
      - [Advanced Encryption Standard(AES)](#advanced-encryption-standardaes)
        - [SubBytes: S-box](#subbytes-s-box)
        - [ShiftRows](#shiftrows)
        - [MixColumn](#mixcolumn)
        - [AddRoundKey](#addroundkey)
        - [Key Schedule](#key-schedule)
        - [Security of AES](#security-of-aes)
      - [Data Encryption Standard(DES)](#data-encryption-standarddes)
      - [Padding](#padding)
      - [Block Cipher Modes](#block-cipher-modes)
    - [Introduction to Public-Key Cryptography](#introduction-to-public-key-cryptography)
    - [Secure Key Exchange](#secure-key-exchange)
    - [RSA Encryption](#rsa-encryption)
    - [Digital Signatures](#digital-signatures)
      - [Features of hand-written signatures in Digital World](#features-of-hand-written-signatures-in-digital-world)
      - [Ensure hardness of forgery](#ensure-hardness-of-forgery)
    - [Hashes, MACs and Authenticated Encryption](#hashes-macs-and-authenticated-encryption)
      - [Hash](#hash)
      - [MACs](#macs)
      - [Authenticated Encryption Modes](#authenticated-encryption-modes)
      - [Summary](#summary)
  - [Access Control](#access-control)
  - [Introduction to Networking](#introduction-to-networking)
  - [Security Protocols](#security-protocols)
  - [Web Systems and Attacks](#web-systems-and-attacks)
  - [Other Common Attacks and Defenses](#other-common-attacks-and-defenses)

# Security and Networks Note

- What is Computer Security?
  - Correctness and Efficient algorithms against an attacker
  - Decide on your assets: Information and Infrastructure
    - Sensitive Data
    - Control Systems
    - Hardware devices
  - How do you safeguard: security goal, estimate impact of attacks, and design mitigations
  - Analyse systems, spot vulnerabilities, build protection

- Information Security
  - Aims
    - **Confidentiality**: Attacker should not retrieve any information
    - **Integrity and Authenticity**: Received data is authentic and the sender is genuine.
    - **Availability**: Data should accessible on demand
  - Potential Attackers
    - **Hackers**: Potentially learning by running known attacks, exploting vulnerabilities
    - **Criminals**: Take control of computers via bugs in softwares. Phishing attacks, Denial of Service(DoS attacks)
    - **Governments**: Extreme computing powers,control on resources(wiretaps)
    - **Business Houses like ISPs**: Spying to sell your data

> - Some Known Attacks
>   - Ransomware
>   - Phishing

## Cryptography

- Cryptography describes how to transfer messages between participants without anyone else being able to read or modify them
- Prerequisite for computer Security
- Start module with explaining th basics of cryptography(enough to understand how TLS works; for more details see cryptography module)
- Before we start with Cryptography, need to look at how to represent data

### Codes Versus Ciphers

- Codes vs. Ciphers
  - A code is any way to represent data.  
    Will use bitstrings (sequence of bits) to represent data.
    Examples: Morese Code, ASCII, Hex Base64
  - A cipher is a code where it is difficult to derive data from code
    - Almost always uses a key
    - Data for a cipher usually called *plain text*. encoding called *cipher text*
    - Function from plain text to cipher text called *encryption*
    - Function from cipher text to plain text called *decryption*

- ASCII  
  ![ASCII.png](./images/ASCII.png)

- Base64  
  ![Base64.png](./images/Base64.png)

- Caesar Cipher
  - The Caesar Cipher replaces each letter of the alphabet with one three to the right
    - a becomes d
    - b becomes e
    - ...
    - z becomes c
  - Use a key
    - **Kerckhoffs' principle**: A cipher should be secure even if the attacker knows everything about it apart from the key
    ![VIGENERE TABLE](./images/VIGENERE%20TABEL.png)

- Frequency Analysis
  - While hard to break by brute force, replacing each letter with another is easy to break using frequency analysis
  - Frequency analysis counts the number of times
    - each symbol occurs
    - each pair of symbols
    - etc.
    and tries to draw conclusions from this

- Summary
  - Code is any binary representation of data; cipher is a code where it is difficult to derive data from code
  - Looked at various codes, including Hex
  - Looked at substitution ciphers, which replace single letters. These are easily breakable

### Symmetric Cryptography

- Overview
  - Will now look at proper encryption schemes
  - Assumption: All participants share common secret key
  - Will consider most important encryption shemes
  - Need some mathematical prerequisites to explain encryption schemes(modular arithmetic)

- Modular Arithmetic
  - Arithmetic modulo n means that you count up to n-1 then loop back to 0
  - i.e., 0, 1, 2, ..., n-1, 0, 1, 2, ...
  - a mod b = r for largest whole number k such that a = b*k + r
  - e.g. 9 mod 4 = 1 because 9 = 2 * 4 + 1

- xor
  - xor(⊕) is binary addition modulo 2;  
    > 0 ⊕ 0 = 0  
    > 1 ⊕ 0 = 1  
    > 0 ⊕ 1 = 1  
    > 1 ⊕ 1 = 0  
  - xor on bitstrings of same length defined by applying xor to corresponding bits
  - Important properties
    - xor is associative and commutative
    - for all bitstrings M, M ⊕ 0 = M
    - for all bitstrings M, M ⊕ M = 0
    > where 0 is a bitstring of all 0's of the appropriate length

- One time Pads
  - Needs a key as long as the message
  - XOR/add the key and the message:  
    (Demonstrated here with strings and addition and subtraction of keys; for bitstrings use xor)
  > Message     : HELLOALICE  
  > Key         : THFLQRZFJK  
  > Cipher text : ALRWERKNLO  
  - Have perfectexryption:  
    You don't learn anything about the plaintext from the ciphertext
  > Theorem:  
  > Given any ciphertext of a certain length, without knowing the key the probability of the ciphertext being the encryption of a plaintext of the same length is the same for all plaintexts of the same length as the ciphertext
  - Problem
    - The key needs to be as long as the message
    - Must use key only once

- Block Ciphers
  - Modern ciphers work on blocks of plain text, not just a single symbol
  - They are made up of a series of permutations and substitutions repeated on each block
  - The key controls the exact nature of the permutations and subsitutions

#### Advanced Encryption Standard(AES)

- AES is a state-of-the art block cipher
- It works on blocks of 128-bits
- It generates *10 round* keys from a signle 128-bit key
- It uses one permutation: ShiftRows and three substitutions SubBytes, MixColumns, AddRoundKey
> A block of 128 bits is represented by a 4x4-matrix where each matrix element is a byte(8 bits), written as  
> ![4x4-matrix](./images/4x4-matrix.png)

![Encryption Process_AES](./images/Encryption%20Process_AES.png)

##### SubBytes: S-box

- SubByte is an operation on bytes using finite field arithmetic
- Use the S-box to transfer the original data to new data

  ![SubBytes](./images/SubBytes.png)

##### ShiftRows

- ShiftRows moves the 
  - 2nd row one byte to the left
  - the 3rd row two bytes
  - and the 4th row 3 bytes

  ![ShiftRows](./images/ShiftRows.png)

##### MixColumn

- MixColumn is substitution of each column such that
  - (a<sub>0</sub>x<sup>3</sup>+a<sub>1</sub>x<sup>2</sup>+a<sub>2</sub>x+a<sub>3</sub>)x(3x<sup>3</sup>+x<sup>2</sup>+x+2) mod (x<sup>4</sup> + 1) = (b<sub>0</sub>x<sup>3</sup>+b<sub>1</sub>x<sup>2</sup>+b<sub>2</sub>x+b<sub>3</sub>)
  - It is matrix mutiplication for the column(cipher key will not be used)**(It is a constant matrix)**

  ![MixColumn](./images/MixColumn.png)

##### AddRoundKey

- AddRoundKey applies ⊕ to the block and the 128-bit round key(which was generated from the main key)

  ![AddRoundKey](./images/AddRoundKey.png)

##### Key Schedule

1. RotWord: Rotates a 32 bit word(last column)
2. SubWord: Substitutes a 32 bit word using the AES S-Box
3. Round Constants: Round Constants are generated using a recursive function(10 round functions! They are constant)
4. AddRoundKey: add round key for each column

##### Security of AES

- No formal proof of security(P = NP?) but best known cryptographic aatack requires 2<sup>126</sup> key guesses - an (irrelevant) improvement of factor 4 compared to 2<sup>128</sup> guesses via brute force attack
- There are side channel attacks(eg via measuring power consumption, execution time)
- Key aspects of security:
  - Shuffling of rows and columns to ensure small change in input causes very big chage in output
  - Require at least one non-linear operation(in the sense of linear algebra) on the data

#### Data Encryption Standard(DES)

- The data Encryption Standard(DES), was the previous standard
- Before it was accepted as a standard the NSA stepped in and added S-boxes and fixed the key length at 56 bits
- S-boxes are a type of substitution

#### Padding

- Block ciphers only work on fixed size blocks.
- If the message isn’t of the right block size we need to pad themessage.
- But receiver needs to tell the difference between the padding and message.

- PKCS5/7
  - If there is 1 byte of space write 01
  - If there are 2 byte of space write 0202
  - If there are 3 byte of space write 030303
  - · · ·
  - If the message goes to the end of the block add a new block of 16161616..
- PKCS 7: 16 byte block, PKCS 5: 8 byte block

#### Block Cipher Modes

- Block Ciphers can be used in a number of modes:
  1. Electronic codebook mode(ECB)
     - Each bloack is encrypted individually
     - Encrypted blocks are assembled in the same order as the plain text blocks
     - if blocks are repeated in the plain text, this is revealed by the cipher text  

      ![ECB](./images/ECB.png)

  2. Cipher Block Chaining Mode(CBC)
     - - each block XOR'd with previous block
       - Start with a random Initialization Vector(IV)
       - helps overcome replay atttack
     - Suppose the plain text is B<sub>1</sub>, B<sub>2</sub>, .., B<sub>n</sub>  
        IV = random number (sent in the clear)  
        C<sub>1</sub> = encrypt(B<sub>1</sub> ⊕ IV)  
        C<sub>2</sub> = encrypt(B<sub>2</sub> ⊕ C<sub>1</sub>)  
        ...  
        C<sub>n</sub> = encrypt(B<sub>n</sub> ⊕ C<sub>n-1</sub>)  

      ![CBC](./images/Cipher%20Block%20Chaining(CBC)%20mode.png)

      - CBC decrypt
        - Receive IV
        - Receive cipher text C1, C2, . . . , Cn
        - Plain text is B1, B2, . . . , Bn, where  
          B1 = decrypt(C1) ⊕ IV  
          B2 = decrypt(C2) ⊕ C1  
          · · ·  
          Bn = decrypt(Cn) ⊕ Cn−1  

      ![CBC_decrypt](./images/Cipher%20Block%20Chaining(CBC)%20mode_decrypt.png)

      - Probabilistic Encryption
        - Probabilistic encryption schemes use random elements to make every encryption different.
        - CBC with a random IV is a good way to make encryption probabilistic.
        - Using CBC and random IVs lets me encrypt the same message, and with the same key, without an attacker realising.  
  
      ![CBC_](./images/CBC.png)

  3. Counter Mode(CTR)
      - Suppose the plain text is B<sub>1</sub>, B<sub>2</sub>, .., B<sub>n</sub>  
      - IV = random number (sent in the clear)  
      - Cipher text: C<sub>1</sub>, C<sub>2</sub>, ..., C<sub>n</sub> where  
        C<sub>1</sub> = B<sub>1</sub> ⊕ encrypt(IV)  
        C<sub>2</sub> = B<sub>2</sub> ⊕ encrypt(IV + 1)  
        ...  
        C<sub>n</sub> = B<sub>n</sub> ⊕ encrypt(IV + n - 1)  

      ![CTR](./images/Counter(CTR)%20mode%20encryption.png)

      ![CTR_Decryption](./images/Counter(CTR)%20mode%20decryption.png)

      - Cipher Texts Can Be Altered
        - AES encryption with a particular key maps any 128-bit blockto a 128-bit block (or 256)
        - AES decrypt also maps any 128-bit block to a 128-bit block.
        - Decrypt can be run on any block (not just encryptions).
      - Known Plain Text Attacks
        - If I know the plaintext I can change CTR encrypted messages
        - eg If I know EncCTR(M1) and I know M1, I can make a ciphertext that decrypts to any message I want, eg M2
        - New ciphertext is Enc<sub>CTR</sub>(M1) ⊕ (M1 ⊕ M2)
        > DecCTR(EncCTR(M1) ⊕ (M1 ⊕ M2)) =  
        > DecCTR(Enc(N||Ctr) ⊕ M1) ⊕ (M1 ⊕ M2) =  
        > Enc(N||Ctr) ⊕ (Enc(N||Ctr) ⊕ M1) ⊕ (M1 ⊕ M2) =  
        > M2?  

### Introduction to Public-Key Cryptography

- Cryptography: four directions
  - Confidentiality
  - Message Integrity
  - Sender Authentication
  - (soft)Sender Undeniability(non-repudiation)

- Symmetric Key Cryptography  
  ![Symmetric Key Cryptography](./images/Symmetric%20Key%20Cryptography.png)
  - Each person has two keys: one public and one Private
  - The keys are asymmetric: Related but not identical
  - Public Key is known to everyone, private key is kept secret  
  ![Public Key Encryption](./images/Public%20Key%20Encryption.png)  
  ![Public Key Infrastructure](./images/Public%20Key%20Infrastructure.png)

### Secure Key Exchange

- MultiRound Solution  
  ![MultiRound Solution](./images/MultiRound%20Solution.png)

- Diffie Hellman Key Exchange
  - Parameters
    - Choose a prime $p$ and a number $g < p$ such that $gcd(g, p − 1) = 1$.
  - Diffie-Hellman Assumption
    - There is no polynomial time algorithm to compute $g^{ab} \mod p$ from $g^a mod p$ and $g^b \mod p$

    ![Diffie-Hellman Assumption](./images/Diffie-Hellman%20Assumption.png)

- Man-in-the-Middle Attack  
  ![Man-in-the-Middle Attack](./images/Man-in-the-Middle%20Attack.png)
  - How to Solve?
    - **Basic Idea**: Authenticating Public Key.
    - **Requirement**: Trusted Third Party: Certification Authority(CA).

### RSA Encryption

- Textbook RSA scheme: Three Algorithms (Gen, Enc, Dec)
  - Gen: on input a security parameter $\lambda$
    - Generate two distinct primes $p$ and $q$ of same bit-size $\lambda$
    - Compute $N = pq$ and $\phi(N) = (p - 1)(q - 1)$
    - Choose at random an integer $e(1<e<\phi(N))$ such that $gcd(e, \phi(N)) = 1$
    - Let $Z_N^*={x | 0<x<N\space and \space gcd(x,N)=1}$
    - Compute $d$such that $e·d ≡ 1(\mod\phi(N))$
    - Public key $PK = (e, N)$. The private key $SK = e, d, N$
  - Enc(PK,m): On input an element $m\in Z_N^*$ and the public key PK = (e,N) compute
    - $c = m^e(\mod N)$
  - Dec(SK, c): On input an element $c\in Z_N^*$ and the private key SK = (e, d, N) compute
    - $m = c^d(\mod N)$

### Digital Signatures

#### Features of hand-written signatures in Digital World

- Hand-written Signatures
  - Function: bind a statement/message to its authors
  - Verification is public. (against a prior authenticated one)
  - Properties
    - **Correctness**: A correct signature should always be verified true
    - **Security**: Hard to forge

#### Ensure hardness of forgery

- Signature Schemes
  - Correctness  
    ![Signature Schemes_Correctness](./images/Signature%20Schemes_Correctness.png)
  - Unforgeability:  
    Must output forgery for a message for which the attacker did not request the signature.  
    ![Signature Schemes_Unforgeability](./images/Signature%20Schemes_Unforgeability.png)
  > - RSA Full Domain Hash
  >   - Public Functions A hash function $H :\{0, 1 \}^*\space\rArr Z_N^*$ 
  >   - Keygen: Run RSA.Keygen. $pk = (e,N), sk = (d, N)$
  >   - Sign: Input: sk, M. Output  
  >     $\sigma = RSA.Dec(sk, H(M)) = H(M)^d \mod N$
  >   - Verify: Input: $sk,M,\sigma$. If $RSA.Enc(pk,\sigma) = H(M)$ output accept, else reject
  >   - If $\sigma^e \mod N = H(M)$, output accept, else reject
  > 
  > - Note
  >   - A hash function takes string of arbitrary length as input and produces a fixed length output. For cryptographic hash functions, given a $z$, it is very expensive to find $x$ such that $H(x) = z$

### Hashes, MACs and Authenticated Encryption

#### Hash

- Hashes
  - A hash of any message is a short string generated from that message
  - The hash of a message is always the same
  - Any small change makes the hash totally different
  - It is very hard to go from the hash to the message
  - It is very unlikely that any two differrent messages have the same hash

- Uses of Hashing
  - Verification of download of message
  - Tying parts of a message together(hash the whole message)
  - Hash the message, then sign the hash(for electronic signatures)
  - Protect passwords
    - Store the hash, not the passwords

- Attacks on hashes
  - Preimage attack: Find a message for a given hash: very hard.
  - Collision attack: Find two messages with the same hash
  - Prefix collision attack: A collision attack where the attacker can pick a prefix for message

#### MACs

- Message Authentication Codes
  - Abbreviated often as "MAC", has nothing to do with MAC in MAC address for networking(MAC = Media Access Control)
  - MACs sometimes used for authentication:
    - Example: Alice and Bank share key k, Alice sends to bank
      $$
        ”Pay \space Bob \space \$10”, MAC_k (”Pay Bob \$10”)
      $$
  - Possible attack on MAC: Add data to a MAC without knowing the key(Length extension attack)

- How can we make a MAC?
  - Block Cipher Modes
    ![BCM](./images/MAC_BlockCipherModes.png)
  - Making a CBC MAC
    ![CBC](./images/MAC_CBC.png)
  - An Inefficient Hash Function
    ![Inefficient Hash Function](./images/MAC_Inefficient%20Hash%20Function.png)

- Broken Hash to Mac
  - If we had a Hash we could try to make a MAC by:
    $$
      MAC_{Key}(M) = H(Key, M)
    $$
  - But this might allow a length extension attack
    ![Broken Hash](./images/MAC_Broken%20Hash.png)

- Cipher Texts Can Be Altered
  - AES encryption with a particular key maps any 128-bit block to a 128-bit block (or 256)
  - AES decrypt also maps any 128-bit block to a 128-bit block
  - Decrypt can be run on any block (not just encryptions)

- Block Mode
  - CBC mode: any change affects all of the rest of the message
  - ECB mode: any change affects only the block
  - CTR mode: any change affects only the bits altered

#### Authenticated Encryption Modes

- Authenticated Encryption Modes
  - Authenticated encryption modes stop this.
  - With Authenticated Encryption you can only form a valid ciphertext if you know the key.
  - Most common way to do this is to add a MAC to the ciphertext.

- CCM mode encryption
  - First calculate an AES CBC-MAC on the data.
  - Then encrypt the message followed by the MAC using the same key and CTR mode.
  - Not rocket science, but proven secure
    - Fully defined as RFC 3610

#### Summary
  - Defined ways of detecting manipulation of ciphertexts
    - **Hashes**: Detect corruption of messages in general. New hashes can be generated by the attacker
    - **MAC (Message Authentication Codes)**: use a key to ensure that message has not been changed
    - **Authenticated Encryption**: provides encryption such that manipulation of cipher texts can be detected. Often uses MACs.

## Access Control

- Introduction
  - Need to ensure that only authorised uses have access to what they need
  - Will discuss ways of achieving this and possible pitfalls

- Model of Access Control  
  ![Model of Access Control](./images/ModelofAccessControl.png)

- Access Control Matrix  
  ![Access Control Matrix](./images/AccessControlMatrix.png)
  > Permission: x: execute, r: read, w: write
  - ACM is a matrix of all principals and objects
  - The matrix entries describe the permissions
  - Problem: maintaining such a matrix can be difficult
  - If the matrix is corrupted, then all control is lost

- The UNIX Access Control List  
  ![Access Control List](./images/The%20UNIX%20Access%20Control.png)

- UNIX File Permissions
  - Permissions:
    - r: read permission
    - w: write permission
    - x: execution permission
    - -: no permissions
  - File Type:
    - -: file
    - d: directory
    - b/c: device file

  ![UNIX File Permissions](./images/UNIX%20File%20Permissions.png)

- Access Control for Directories
  - For directories
    - "r" is read only for directory contents
    - "x" is permission to traverse, e.g. switch to, run
  - No "x": I can't run any commands inside the directory
  - No "y": I can't list the files in the directory

- Access Control for Program
  ```
  -r-sr-xr-x  1 root  wheel 70352 19 Jun  2009 passwd
  ```
  - The "x" permission controls who can run a program in the case of passwd: anyone
  - The "s" permission indicates that the program runs with the permission of its owner

- Different user identifiers
  - Have different user identifiers(uids):
    - real uid (ruid) owner of process
    - effective uid (euid): used for access checks (except filesystem)
    - file system uid (fsuid): used for access checks and ownership of files (usually equal to effective uid)
    - saved user uid (suid): when the euid is changed, the old euid is saved as suid. Unprivileged process may change euid only to ruid or suid.
  - Provides flexibility for granting higher privileges temporarily
    - eg daemons: start as root (to bind to ports < 1024), then set ruid, euid and suid to unprivileged values. Cannot gain root privileges afterwards
    - Process run as privileged user may set euid to unprivileged value, then execute non-privileged operations, and gain root privileges afterwards

- Security issues with granting higher privileges
  - Users can run process with more privileges
  - If there was a mistake in the passwd program we could use it do root only actions.
  - Particular problem: race conditions in code like: `if can_access file then perform_operations on file`
  - **Make sure process have as low a level as possible.**

- Storing Passwords
  - Passwords not stored in clear text
  - Only hashes are stored
  - Further security measure: Store pair (Salt, Hash), where Salt is random bitstring, and Hash the hash of the salt and the password
  - ⇒ Same password for two users gives rise to different entries in the password file
  - Makes cracking passwords much harder

- Windows Password Hashes
  - Windows stores its password hashes in:  
    `system32/config/SAM`
  - This file requires Admin level to read.
  - It is locked and encrypted with a key, based on other key values  
    This adds no real security


## Introduction to Networking

## Security Protocols

## Web Systems and Attacks

## Other Common Attacks and Defenses