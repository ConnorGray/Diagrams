# Development of Diagram Maker

This document contains information about how to make development contributions to the
Diagram Maker project.

### Command quick reference

**Build the main `DiagramMaker` paclet artifact:**

```shell
$ cargo make paclet
```

**Run the `diagram-maker` tests:**

```shell
$ cargo test
```

## Artifacts 

The Diagram Maker project has two main "outputs":

* The `DiagramMaker` Wolfram Language paclet, that is released via the [Wolfram Paclet
  Repository](https://resources.wolframcloud.com/PacletRepository).
* The `diagram-maker` Rust crate, which is released via [crates.io](https://crates.io).

## Setup

Developing Diagram Maker requires that the following software be installed:

* The Rust programming language, specifically the `cargo` build tool.
* The Wolfram programming language. The
  [free Wolfram Engine](https://www.wolfram.com/engine/) is sufficient, though an
  installation of Wolfram with a GUI (e.g. Mathematica) will also work.
* Diagram Maker uses the [`cargo-make`](https://crates.io/crates/cargo-make) utility to
  drive many development command-line tasks. `cargo-make` can be installed using `cargo`:

  ```shell
  $ cargo install cargo-make
  ```
