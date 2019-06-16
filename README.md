Workshop on Session Types using IOHK Typed Protocols Package
============================================================

This material was prepared for a [Monadic Party
2019](https://monadic.party) workshop on Session Types.  It is based on the
work of [IOHK](https://iohk.io) networking team with collaboration with
[Well-Typed](https://well-typed.com) and [Predictable Network
Solutions](http://www.pnsol.com/).

It consist of four parts:

Introduction to [typed-protocols](https://github.com/input-output-hk/ouroboros-network) framework
-------------------------------------------------------------------------------------------------

[Typed-protocols](https://github.com/input-output-hk/ouroboros-network)
package is used at IOHK to develop protocols & network applications.  The next
release of [Cardano crypto-currency](https://www.cardano.org/en/home/) is developed using it.

You will need to clone
[ouroboros-network](https://github.com/input-output-hk/ouroboros-network)
repository.  We will focus on the
[typed-protocols](https://github.com/input-output-hk/ouroboros-network/tree/master/typed-protocols)
cabal project.

Example ping-pong protocol and protocol pipelining
--------------------------------------------------

The [ping pong
protocol](https://github.com/input-output-hk/ouroboros-network/tree/master/typed-protocols/src/Network/TypedProtocol/PingPong).
Instruction to run the ping pong demo are
[here](https://github.com/input-output-hk/ouroboros-network/wiki/Ouroboros-Network-Demo#pingpong-demo).

Simple streaming protocol
-------------------------

A simple streaming protocol is presented in this package, under
`src/Network/Protocol/Stream`.  It contains client and server,
a [CBORG](https://hackage.haskell.org/package/cborg) codec, and a demo
application which streams a file from your file system over a Unix socket.

To run a demo server:

```
cabal new-run demo-stream -- server
```

To stream a single file
```
cabal new-run demo-stream -- client 10 ./some-file
```

Assignment
----------

* Extend the streaming protocol to allow streaming of multiple files.
* Extend the demo to stream multiple files.
* Build a pipelined server, which can do [protocol
  pipelining](https://en.wikipedia.org/wiki/Protocol_pipelining) to hide
  latency.
* Add tests: checkout
  [ping-pong](https://github.com/input-output-hk/ouroboros-network/blob/master/typed-protocols/src/Network/TypedProtocol/PingPong/Tests.hs)
  tests suite, how to write property based tests for a protocol and its codec.

Build instructions
------------------

* The simplest way is to use `cabal new-build` (nix-style local build using
  cabal).  You will need a recent `cabal-2.4.0.0`.

  To build the library:
  ```
  cabal new-build lib:monadic-party2019
  ```

  To build the demo:
  ```
  cabal new-build demo-stream
  ```
