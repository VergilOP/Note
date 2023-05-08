# Past Exam Question 1

## Each question will be marked out of 20. The examination will be marked out of 60, which will be rescaled to a mark out of 100

1.  
    1. How does padding work?
        > Padding works by adding extra bits or characters to a message to ensure it reaches a fixed length or block size.   
        This is commonly done in encryption algorithms that operate on fixed-length blocks of data, such as AES. Padding schemes, like PKCS#7, are used to make the message a multiple of the block size, allowing the encryption algorithm to process it properly. When the message is decrypted, the padding is removed to recover the original message.
    2. For full disk encryption would you use AES in CBC-mode or in counter mode? Justify your answer.
        > For full disk encryption, it's generally better to use AES in counter mode (CTR) rather than in CBC mode. This is because counter mode offers parallelization, allowing for faster encryption and decryption of large amounts of data.
    3. Alice and Bob use the Diffie-Hellman key exchange protocol to derive a session key. If this is done over an unencrypted wireless connection, can an active attacker learn the session key? Either describe an attack, or explain why no attack exists. 
        > In a Diffie-Hellman key exchange over an unencrypted wireless connection, an active attacker can perform a man-in-the-middle attack. The attacker intercepts the messages exchanged between Alice and Bob and establishes separate key exchanges with each of them. The attacker then relays the messages between Alice and Bob, but substitutes their own public keys in the process. Alice and Bob will each derive a session key, but these keys will be shared with the attacker, not with each other. As a result, the attacker can decrypt and manipulate the messages exchanged between Alice and Bob.
    4. Assume the account number is contained in the first block of a message. Assume CBC-mode is used for encryption. Is it possible for an active attacker to change the account number? Either describe an attack, or explain why no attack exists.
        > If CBC-mode encryption is used and the account number is contained in the first block of a message, an active attacker can change the account number using a bit-flipping attack. In a bit-flipping attack, the attacker modifies the ciphertext of the initialization vector (IV) XORed with the plaintext block containing the account number. This results in a predictable change in the decrypted plaintext block, effectively allowing the attacker to change the account number. 

2.  
    1. What is a Man-in-the-middle-attack?
        > A Man-in-the-Middle (MITM) attack is a type of cyber attack where an attacker intercepts the communication between two parties without their knowledge. The attacker can eavesdrop on, modify, or inject new messages into the communication, potentially compromising the confidentiality, integrity, or authenticity of the exchanged information.
    2.  A website uses TLS to ensure credit card data is transmitted securely. Is this enough to protect against malware running on the client? Justify your answer.
        > TLS (Transport Layer Security) is designed to provide secure communication between a client and a server by encrypting data and authenticating the server. While TLS effectively protects credit card data during transmission, it does not protect against malware running on the client-side. Malware on the client could potentially capture the credit card information before it is encrypted by TLS, or log keystrokes as the user enters their information.
    3.  Consider the following protocol:
        $$
            A \rightarrow B: A\\ 
            B \rightarrow A: N_A\\
            A \rightarrow B: \{N_A\}_{K_{AB}}, \{Pay Elvis \$5\}_{K_{ab}}
        $$
        where $N_A$ is a nonce and $K_{ab}$ is a symmetric key known only to Alice and Bob. Is this protocol secure? If yes, explain why. If not, give an attack in Alice-Bob notation.
        > The protocol is not secure, as it is vulnerable to a replay attack. An attacker can intercept the message containing ${N_A}{K{AB}}, {Pay Elvis 5}{K{ab}}$ and later send the same message to Bob.  
        > $A -> E(B): A$  
        > $E(B) -> A: N_A$  
        > $A -> E(B): {N_A}_{K_{AB}}, {Pay Elvis 5}_{K_{ab}}$  
        > $E(B) -> B: {N_A}_{K_{AB}}, {Pay Elvis 5}_{K_{ab}}$  
        > $(Attacker replays the message)$  
        > $E(B) -> B: {N_A}_{K_{AB}}, {Pay Elvis 5}_{K_{ab}}$  

    4. Consider the following protocol:
        $$
            A \rightarrow B : N_A, A \\
            B \rightarrow A : \{N_A, N_B, B\}_{pk(A)}\\
            A \rightarrow B : \{M\}_{\#(N_A, N_B)}
        $$
        where $N_A$ and $N_B$ are nonces, and $\#(N_A, N_B)$ is a symmetric key based on the hash of $N_A$ and $N_B$, and $pk(A)$ is the public key of A. Is it possible for the attacker to learn M without knowing the private key of A? If so, give an attack in Alice-Bob Notation. If not, explain why
        > In this protocol, it is not possible for an attacker to learn M without knowing the private key of A. Since $N_A$ and $N_B$ are nonces, they ensure freshness in each communication session. The symmetric key, $\#(N_A, N_B)$, is based on the hash of these nonces, which makes it difficult for the attacker to predict or reproduce without knowing both nonces. Furthermore, the message ${N_A, N_B, B}_{pk(A)}$ is encrypted using A's public key, meaning only A, with their private key, can decrypt it to retrieve $N_B$. Without knowing A's private key, the attacker cannot obtain both nonces, and therefore cannot generate the symmetric key or decrypt the message M.
3. 
    1. What is cross-site scripting?
    2. A website contains the following code which sends a message, user name and password to a server:
        ```html
            1c <form action=”message.php” method=”get”>
            2c <p>Message: <input type=”text” name=”message” /></p>
            3c <p>Username: <input type=”text” name=”user” /></p>
            4c <p>Password: <input type=”text” name=”pass” /></p>
            5c <p><input type=”submit” /></p>
        ```
        and on the server the message.php page processes this data:
        ```php
            1s <?php
            2s $user = $_REQUEST[”user”];
            3s $pass = $_REQUEST[”pass”];
            4s $message = $_REQUEST[”message”];
            5s $result = mysqli_multi_query($con,”UPDATE messages SET
            6s message=’”.$message.”’ WHERE user=’”.$user.”’”);
            7s $row = mysqli_fetch_array($result);
            8s if (!empty($row)) {
            9s echo ”Your message: ”.$message.” has been added”;
            10s }
            11s ?>
        ```
        Describe four security weaknesses in this website, how they might be exploited and rank them in order of severity
    3. Provide fixes for the security weaknesses you have identified. 
    