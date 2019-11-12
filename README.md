# O-Ring

Ring Erlang exercise implemented in several languages.

## Exercise

Taken from "Programming Erlang", second edition (by Joe Armstrong):

```text
Write a ring benchmark. Create N processes in a ring. Send a message
round the ring M times so that a total of N * M messages get sent. Time
how long this takes for different values of N and M.
Write a similar program in some other programming language you are
familiar with. Compare the results. Write a blog, and publish the results
on the Internet!
```

## Guidelines
To partecipate to the benchmark, you must adopt on the following guidelines:
  *  every implementation shall expose an executable file (bash script or program) named `ring`
  *  every implementation shall agree on the following API when `ring` is invoked: `./ring NumberOfNodes NumberOfTrips` where:
     -  `NumberOfNodes`: the number of processes/threads/nodes in the ring, `N` in the description above
     -  `NumberOfTrips`: the number of times a message is passed in the ring, `M` in the description above
     -  returned value of the script should be `0` if ok
     -  in case of errors, nothing shall be written on stdout (you can write on stderr), and error code shall be different than `0`
  * every implementation shall measure the time interval `T0` used to setup the ring
  * every implementation shall measure the time interval `T1` in milliseconds between the instant the first message is sent and the instant the last one is received 
  * every implementation shall write to stdout a single number, `T`, described above
  * every implementation shall be provided in a separated folder with a significant name
  * every implementation shall provide a `README.md` file with (at least) the following sections:
     -  `Build`: instructions on how to build; after a successful build, a `ring` executable should be present in a build directory
     -  `Description`: a brief description of the implementation
  * every implementation has complete freedom on all other aspects not specified above

## Example Output

```
> ./ring 200000 30
465 2990 200000 30
```

where:

- `465` is the time in ms to setup the ring
- `2990` is the time in ms from the first message to the last one
- `200000` is the number of nodes in the ring
- `30` is the number of trips (how many times we sent a message in the ring)

## Erlang

* Build

```sh
cd erling
erlc ring.erl
```

* Run

```sh
./ring 200000 30
```

## Elixir

* Build

```sh
cd exring
mix escript.build
```

* Run

```sh
./exring 200000 30
```

## Go

* Build

```sh
cd goring
make all
```

* Run

```sh
./ring 200000 30
```

## Haskell

* Build

```sh
cd haskring
make all
```

* Run

```sh
./ring 200000 30
```

## Scala

## Rust

* Run

```sh
cd rustring
cargo run --release  -- 200000 30
```  

## Benchmark
