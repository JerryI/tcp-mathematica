# tcp-mathematica
Implementation of the simple protocol to transfer arbitrary data over TCP between two kernels. 

*No handshaking
*First 4 bytes - the length of the message (unsigned int)
*The rest is payload
*No additional separators
*Fully asynchronous
*Working effectively with byte arrays
*TCP packet size doesn't matter

## Server.wls
It is always listening for data from multiple clients. By the default, when the new data comes, it calls the function `EvalAndReply` for every received message (not TCP packet). The name fully describes what it does. 

To send arbitrary data to particular client use
``
SendAsync[]
``
  
