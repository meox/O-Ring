# Ring

Ring Erlang exercise implemented in several languages.

## Exercise

Taken from "Programming Erlang", second edition:

```text
Write a ring benchmark. Create N processes in a ring. Send a message
round the ring M times so that a total of N * M messages get sent. Time
how long this takes for different values of N and M.
Write a similar program in some other programming language you are
familiar with. Compare the results. Write a blog, and publish the results
on the Internet!
```

## Erlang

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
cd golang
go build -w ring
```

* Run

```sh
./ring 200000 30
```

## Haskell

## Scala

## Benchmark

## Authors

- Federico Bertolucci
- Gian Lorenzo Meocci (`glmeocci @ gmail.com`)
