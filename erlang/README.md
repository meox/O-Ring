# Erlang Ring
The Ring exercise in Erlang, without OTP.


## Build
---

Compile the source code: `erlc ring.erl`

Run the script: `./ring N M`
with:
  -  `N`: the number of processes in the ring
  -  `M`: the number of times a message travels in the ring


## Description
---

The ring is built by recursively spawing a new process, until the total number of `N` is reached. 
The starting process (the Erlang shell) will start a timer and inject in one of the process a message consisting of the sole 
number `N*M`; every time the message is received by a process, it sends to the next one the received number `K`, minus 1: `K-1`. 

When the number 1 is reached, the parent process is notified. It will stop the timer, print result and exit.
