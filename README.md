RPC-Exec - executable plugins using json-rpc
==

RPC-Exec allows implementing pligin as a separated executable in its own language, connected with the main program via json-rpc-1.0 as described
in [http://json-rpc.org/wiki/specification](http://json-rpc.org/wiki/specification). It was started during development of some perl code, for prototyping
new functionality in Haskell. I did not feel like messing with FFI of both them in a single process, so decided to go this way.

Status
--

The tree contains haskell library RPC. It is a primary implementation. It supports running a program
or custom connection from two handles. It can handle requests in both ways, and intended to be able
to run several requests in parallel, thought this is not yet so.
It is still a deep alpha, particularly some very important things still not implemented.

Also contains a perl library RPCX.pm. It is not working, only sketched. I intended it to be able to act as a simple client.
