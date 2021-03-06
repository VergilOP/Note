# Table of contents

- [Security](#security)
  - [Identify **three** items that could be contained in a `digital certificate` \[3\]](#identify-three-items-that-could-be-contained-in-a-digital-certificate-3)
  - [Explain how `asymmetric encryption` uses the contents of the `digital certificates` to ensure that the message has not been altered during transmission \[6\]](#explain-how-asymmetric-encryption-uses-the-contents-of-the-digital-certificates-to-ensure-that-the-message-has-not-been-altered-during-transmission-6)
    - [Explain how the customer's browser, and the server used to collect the payment will `establish a secure connection` \[6\]](#explain-how-the-customers-browser-and-the-server-used-to-collect-the-payment-will-establish-a-secure-connection-6)
  - [Give the similarities and differences between a `public key` and a `private key` \[4\]](#give-the-similarities-and-differences-between-a-public-key-and-a-private-key-4)
  - [Give the similarities and differences between a `digital certificate` and a `digital signature` \[4\]](#give-the-similarities-and-differences-between-a-digital-certificate-and-a-digital-signature-4)
  - [Give the similarities and differences between `phishing` and `pharming` \[4\]](#give-the-similarities-and-differences-between-phishing-and-pharming-4)
  - [Name **two** types of malware. State what the company should do to help `prevent the effect of the malware` \[4\]](#name-two-types-of-malware-state-what-the-company-should-do-to-help-prevent-the-effect-of-the-malware-4)

Security
--------

### Identify **three** items that could be contained in a `digital certificate` \[3\]
> s20_33_Q8

- a hashing algorithm
- a public key
- serial number
- dates valid

### Explain how `asymmetric encryption` uses the contents of the `digital certificates` to ensure that the message has not been altered during transmission \[6\]
> s20_33_Q8

- ***Sender's message*** is encrypted using ***Receiver's public key*** (provided by ***Receiver's digital certificate***)
- ***Sender's hashing algorithm*** is used on the message to produce the message digest
- The message digest is then encrypted with ***Sender's private key*** to provide a digital signature
- Both the encrypted message and the digital signature are sent
- The message is decrypted with ***Receiver's private key***
- ***Sender's digital signature*** is decrypted with ***Sender's public key*** (provided by the ***Sender's digital certificate***) to obtain the message digest
- ***Sender's hashing algorithm*** (provided by the ***Sender's digital certificate***) recreates the message digest from the decrypted message
- The two message digests are compared, if they are the same then the message should be authentic

#### Explain how the customer's browser, and the server used to collect the payment will `establish a secure connection` \[6\]
> s19_33_Q5

- Browser requests that the server identifies itself
- Server sends a copy of its (Digital) Certificate
- ...containing its public key
- Browser checks the certificate
- ...against a list of trusted Certificate Authorities
- If the browser trusts the certificate
- ...a symmetric session key is created
- ...this is (by the browser) encrypted using the server's public key and sent to the server
- Server decrypts the symmetric session key
- ...using its private key
- Server and browser now encrypt all transmitted data with the session key

### Give the similarities and differences between a `public key` and a `private key` \[4\]
> w20_33_Q6

- **Similarities**
  - Both used in asymmetric
  - ...encryption
  - ...as a pair of keys is required
  - ...one is used to encrypt the data/message, and the other is used to decrypt the data/message
  - Both hashing algorithms

- **Differences**
  - Private key only known to owner of the key pair
  - ...The public key can be distributed to anyone
  - When messages are sent to the owner of a public key, they are encrypted with the owners public key
  - ...so they can only be decrypted by the owner's private key
  - Message digests are encrypted with the private key of the sender to form a digital signature
  - ...messages are encrypted with the public key of the receiver

### Give the similarities and differences between a `digital certificate` and a `digital signature` \[4\]
> w20_33_Q6

- **Similarities**
  - Both used for authentication
  - Both are unique to the owner/subject
  - include/use owner's public key
  - include/make use of hash algorithm

- **Differences**
  - Certificate obtained from issuing authority
  - ...signature created from a message
  - Certificate provides authentication messages that are sent by the owner
  - ...Signature used to authenticate messages that are sent by the owner
  - Certificate remains unchanged whilst it is valid
  - ...new signature created for every message

### Give the similarities and differences between `phishing` and `pharming` \[4\]
> w20_33_Q6

- **Similarities**
  - Both malware/malicious software
  - Both collect personal/sensitive data
  - ...via fake websites
  - ...the data are then used illegally

- **Differences**
  - Phishing uses (malicious) emails...
  - ...that direct users to fake websites
  - Pharming misdirects browser to a bogus website
  - ...by modifying entries on a DNS server
  - ...or by being installed on your computer

### Name **two** types of malware. State what the company should do to help `prevent the effect of the malware` \[4\]
> s19_33_Q5

- **Virus**
  - Have company policies to ensure that anti-virus software is installed regularly updated and run

- **Spyware**
  - Have company policies to ensure that anti-spyware software is installed, regularly updated and run

- **Phishing**
  - Have network policies to ensure that the firewall criteria include SPAM filters, whitelist, blacklist etc
