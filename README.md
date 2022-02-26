# tcp-mathematica
Implementation of the simple protocol to transfer arbitrary data over TCP between two kernels. 

* No handshaking
* First 4 bytes - the length of the message (unsigned int)
* The rest is payload
* No additional separators
* Fully asynchronous
* It works effectively with byte arrays
* TCP packet size doesn't matter

## server.wls
It is always listening for data from multiple clients. By the default, when the new data comes, it calls the function `EvalAndReply` for every received message (not TCP packet). The name fully describes what it does. 

To send arbitrary data to particular client use

    SendAsync[message, socket, Promise-> Handler]

The argument `message` has the attribute `Hold`.
`socket` is the of the connection with the particular client, basically it's `SocketObject[""]//First`.
With the option `Promise` you can specify which function (`Handler`) will be used on upcoming reply from the client. It accepts two arguments: decoded `message` and the id of the socket. However, after the first received message, the handler function will be set back to the default.

## cli.wls
It connects to the server and also listens. The default handler for incomming messages is `Eval` function.
For sending messages the same function used

    SendAsync[message, socket, Promise-> Handler]
    
In this case you have only one connection, therefore `socket = $cli//First`.
