# tcp-mathematica
Implementation of the simple protocol to transfer arbitrary data over TCP between two kernels. 

* No handshaking
* First 4 bytes - the length of the message (unsigned int)
* The rest is payload
* No additional separators
* Fully asynchronous
* It works effectively with byte arrays
* TCP packet size doesn't matter

## Installation
It is performed simply via `PacletInstall`

    PacletInstall["https://github.com/JerryI/tcp-mathematica/blob/main/JTP.paclet?raw=true", ForceVersionInstall -> True]
    <<JTP`

The examples are stored in `test.nb`.

## Server
It is always listening for data from multiple clients. Firstly, you need to create server by

    server = JTPServer[] // JTPServerStart

The host address and the port can be specified by `"host"` and `"port"` options. By the default, it starts to listerning. It evaluates every received message (not TCP packet) and replies. 

One can access to the logs using

    server["log"]["Elements"] // TableForm // Dynamic

## Client
You can use it in the same manner

    client = JTPClient["host"->"", "port"->] // JTPClientStart

There are a few different modes how to work with server:

### 1. Evaluate and fetch the result
The simples model is like `Evaluate` in the notebook interface

    JTPClientEvaluate[client, expression]

The second argument has `Hold` attribute on `èxpression`. After evaluating on the server it will return the result.

### 2. Asynchronous evaluation
The same as the previous, but doen't stop the program and accepts `callback[uuid, result]` function to handle the answer from the server. 

    JTPClientEvaluateAsync[client, expression, Promise -> callback]

### 3. Listening mode
To start contineous listerning mode on the client use

    JTPClientStartListening[cli, Promise -> callback]

If the `callback` option is not specified, then only the evaluation of the incomming answers will be performed. Note that in this mode the methods (1), (2) are disabled.

## Misc
Other notes on everything...
### Local variables
The evaluation of the incomming messages on the client and server either performes inside `Block[]` with defined variable `socket = uuid`, where `uuid` is id of the socket, which sent the message. It can be obtained by

    JTPClientSend[cli, socket] == "0e5eac ... "

And to reply to yourself using low-level functions on the server's side

    JTPClientStartListening[cli, Promise -> Function[{x, y}, CreateWindow[DialogNotebook[y]]]]
    JTPClientSend[cli, reply[socket, "ahaha"]; "ohoho"];

This part is still in development...

## Development
You can create the new paclet using `build.nb`. 