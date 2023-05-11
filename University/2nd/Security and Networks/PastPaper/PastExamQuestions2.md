# Past Exam Question 2

## Answer ALL questions. Each question will be marked out of 20. The paper will be marked out of 60, which will be rescaled to a mark out of 100.

1. 
    1. What is a Block cipher mode?
        > A block cipher mode is a method that applies a block cipher algorithm to plaintext data. It defines how to use a symmetric key block cipher algorithm for encrypting large amounts of data, typically larger than the block size of the cipher.
    2. Bob gives instructions to buy or sell shares to his broker over the internet. When Bob gives an instructions, he sends two messages. The first message consists of the RSA-encryption with the broker's public key of a 128-bit key which is shared between Bob and the broker. The second message consists of the RSA-encryption with the broker’s public key of the instruction. If an attacker manages to obtain the encrypted messages, is it possible for the attacker to send an instruction to the broker which is different from all previous instructions?
        > In theory, it is possible for an attacker to send a different instruction to the broker from all previous instructions, assuming the broker's system solely relies on RSA encryption for the authenticity and integrity of messages.  
        > An attacker could potentially intercept the encrypted shared key and the instruction messages sent by Bob. Utilising the broker's public key, which is publicly available, the attacker can create a new fraudulent instruction, encrypt it, and send it to the broker along with the intercepted encrypted shared key.
    3. Assume Alice and the bank share a symmetric key. Alice encrypts “Pay Tom 1000 pounds” in AES-counter mode using this key, and signs the encrypted message with El-Gamal using her private key. The bank accepts this message if it can decrypt it, and the signature matches. If the attacker has obtained the encrypted message and the signature, is it possible for the attacker to change the message so that message is the encryption of “Pay Bob 9999 pounds” and moreover create a matching signature which the bank will accept? If this is possible, describe how the attacker can do this. If this is not possible, explain why.
        > Yes, in AES-Counter mode, an attacker with knowledge of the plaintext and corresponding ciphertext can manipulate the content of the message. This is possible because the attacker can compute the XOR of the known plaintext and ciphertext to obtain the keystream, then XOR this keystream with the desired new plaintext to get a valid new ciphertext.  
        > However, this attack doesn't affect the El-Gamal signature. The signature is based on Alice's private key, which the attacker does not possess. Therefore, the attacker cannot create a valid signature matching the altered message. When the bank verifies the signature, it will find it does not match, and thus will not accept the manipulated message.

2. 
    1. What is a replay attack?
        > A replay attack is a network attack where a valid data transmission is maliciously or fraudulently repeated or delayed. 
    2. Is it safe to replace nonces by timestamps in a security protocol? Justify your answer.
        > No. Timestamps are predictable, hence in the Needham-Schroder protocol the attacker could guess the key.
    3. Consider the following protocol:
        $$
            A \rightarrow B : N_A, B  \\
            B \rightarrow A : E_A(N_A), E_A(Sign_B(Pay Elvis \$5), Pay Elvis \$5)
        $$
        Assume different protocol runs produce different payment messages. Is this protocol secure? If yes, explain why. If not, give an attack in Alice-Bob notation.
        > There is an attack.   
        > The attacker keeps the part E_A(Sign_B(Pay Elvis £5, Pay Elvis £5) from the first protocol round   
        > and in the second protocol round replaces the message  E_A(Sign_B(Pay Charlie £10, Pay Elvis £10) with the message  E_A(Sign_B(Pay Elvis £5, Pay Elvis £5).
    4. Consider the following protocol:
        $$
            A \rightarrow B : E_B(N_A, A)   \\
            B \rightarrow A : E_A(N_B, B)   \\
            A \rightarrow B : E_B(N_B)
        $$
        where NA and NB are nonces, and #(NA, NB) is a symmetric key based on the hash of NA and NB. By giving an attack in Alice-Bob notation, show that this protocol does not satisfy key agreement.
        > One possible attack is:
        >     $$
        >       A -> E(B): E_B(N_A, A) \\
        >       E(A) -> B: E_B(N_A', A)\\
        >       B-> A: E_A(N_B, B) \\
        >       A -> B: E_B(N_B)   \\
        >     $$
        > Now A thinks the key is #(N_A, N_B), whereas B thinks the key is #(N_A', N_B). Note that the attacker does not learn the key,   but this was not required.
3. You review a C program that performs a password check:
    ```c
        1 int check_authentication(char *password) {
        2   int authenticated = 0; // 0: not authenticated, else authenticated
        3   char password_buffer[16];
        4   
        5   strcpy(password_buffer, password);
        6   password_buffer[15] = '\0'; // prevent long strings!
        7   if(strlen(password_buffer) > 15)
        8   return 0;
        9   
        10  if(strcmp(password_buffer, "mahgnimrib") == 0)
        11  authenticated = 1;
        12 
        13  return authenticated;
        14 }
    ```
    1. Assume that the program is compiled for x86 in 32-bit mode.
        1. Sketch the state of the stack before line 5 is executed. Clearly indicate where top and bottom of the stack are located. Assume that all variables are aligned at 4-byte boundaries.
            > From top to bottom: Password buffer, authenticated, old EBP, old EIP, *password.
        2. Explain which vulnerability is present in this code?
            > Buffer overflow in line 5 - strcpy does not do any checks and copies until it finds a \0 - character.
        3. Indicate which part of the stack has been changed after the strcpy on line 5 has been executed when the input password is 20 characters long.
            > the password buffer is overwritten. If the \0-character is not counted as one of the 20 characters, then the first byte of authenticated is overwritten as well.
    2. For each of the following exploits, explain how you would craft an input to the function to achieve it. If possible, give a concrete example.
        1. Circumvent the password check. Your input should make the function return 1 without knowing the correct password.
            > The password should consist of 17-20 characters, overwriting the authenticated flag. It should not be longer than that to prevent messing with the EIP and EBP.
        2. Achieve an arbitrary code execution?
            > The return address i.e. the EIP on the stack. Also the attacker needs to place a shellcode on the stack and jump into it by overwriting EIP.
    3. The author of the code intended to prevent this type of vulnerability using the code in lines 6–8. Explain why these checks do not achieve the intended purpose and explain how you would need to change the code instead.
        > Line 6 ensures that the string in password_buffer is at most 15 characters long. Hence line 7 and 10 work. But the buffer overflow has already occurred when line 5 was executed. The fix is to use strncpy and copy at most 15 bytes into the password buffer. This makes a buffer overflow impossible, and lines 7-9 can be deleted.
