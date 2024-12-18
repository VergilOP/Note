# Cryptographic  
Q: What is a Hash Function  
A: A hash function is a function that converts an input (or 'message') into a fixed-size string of bytes.  
  
Q: What is a Block cipher mode?  
A: A block cipher mode is a method that applies a block cipher algorithm to plaintext data.   
  
Q: How does padding work?  
A: Padding works by adding extra bits or characters to message to ensure it reaches a fixed length or block size  
  
Q: Explain what is symmetric encryption and asymmetric encryption?  
A: Symmetric encryption uses the same key for encryption and decryption, while asymmetric encryption uses a pair of keys, the public   key for encryption and the private key for decryption.  
  
Q: What is a cipher block chaining (CBC) mode in cryptography and how does it work?  
A: In CBC mode, each block of plaintext is XORed with the previous ciphertext block before being encrypted.  
  
Q: What is a ECB mode in cryptography and how does it work?  
A: Electronic Codebook (ECB) is a mode of operation for a block cipher, each block is encrypted individually, with the characteristic that each possible block of plaintext has a defined corresponding ciphertext value and vice versa.   

Q: What is a CTR mode in cryptography and how does it work?  
A: In CTR mode, a nonce (typically a counter) is encrypted and then XORed with the plaintext to produce the ciphertext. The nonce is incremented for each subsequent block.  

# Access Control  
Q: What is an Access Control List (ACL)?  
A: An access control list is a table that defines the access rights of an individual (such as a user or process) to an object (such as   a file or directory).  
  
# Protocol  
Q: If a website uses HTTPS, does that protect against a keylogger on the client machine? Justify your answer.  
A: No, HTTPS protects data in transit from the client to the server. It does not protect against malware such as keyloggers on the   client machine which can capture keystrokes before they are even encrypted by HTTPS.  
  
Q: What is a digital signature and what are its uses?  
A: A digital signature is a cryptographic tool that allows an entity to prove the authenticity and integrity of a message or document.   It's often used in situations where it's crucial to detect forgery and tampering.  
  
Q: Explain the main functions of the SSL/TLS protocol?  
A: The main function of the SSL/TLS protocol is to provide security and data integrity assurance on the Internet. It uses encryption to   protect communications between two systems against eavesdropping, tampering, and message forgery.  
  
Q: What is a VPN and why is it used?  
A: A VPN, or Virtual Private Network, is a service that creates a direct, secured connection between your device and the website or   service you're accessing. It is used to encrypt your data and hide your online activity from ISPs, hackers, and other third parties.  
  
Q: Four creteria of Mutual Belief in Key  
A: fresh key  
    key exclusively  
    far-end operative  
    once authentication  

# Attack  
Q: What is a Man-in-the-middle-attack?  
A: The Man-in-the-middle attack is when the attack intercepts the communication between two parties, and can change the message before   sending it to the other party, and can even pretend the other party  
  
Q: What is a SQL Injection attack?  
A: SQL Injection is an attack technique where an attacker inserts malicious SQL code into a query, which can then be executed by the   database. This can allow the attacker to view, modify, or delete data in the database.  
  
Q: What is Cross Site Scripting (XSS)? How to defend?  
A: A cross-site scripting attack is a type of network attack in which an attacker injects a malicious script into a web page, and the   script will be executed when other users browse the web page. Common ways to defend against XSS include validating input, encoding   output, and using Content Security Policy (CSP).  
  
Q: What is a CSRF attack and how can a website defend against it?  
A: Cross-Site Request Forgery (CSRF) is an attack that tricks the victim into submitting a malicious request. It uses the identity and   privileges of the victim to perform an undesired function on their behalf. To prevent CSRF attacks, a website can use anti-CSRF tokens,   which are unique to each session and request and validate them with each client submission.  