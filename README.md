# mcc

Another MicroC compiler. This used to be a fork of the reference compiler for Columbia's _Programming Languages and Translators_ course, but has since grown to include very important C features such as pointers and structs. The original can be found at https://github.com/cwabbott0/microc-llvm. Students in the class are allowed to write the compiler for their own final semester project in OCaml or Haskell, but since the provided reference compiler is in OCaml, almost no one chooses Haskell. This project is an attempt to remedy that situation and open the possibility of using Haskell to more students. The `compat` branch of the repo has only features that are present in the original repo, whereas master has some additions, such as pointers, structs, bitwise operators and an exponentiation operator that demonstrates how to link llvm intrinsic functions. For comparison, two separate parsers are provided, one recursive descent parser implemented with [Megaparsec](https://hackage.haskell.org/package/megaparsec) and one more classical LR-style parser implemented with [Alex](https://hackage.haskell.org/package/alex) and [Happy](https://hackage.haskell.org/package/happy), Haskell's analogues to the venerable lex and yacc. The test suite checks that they return identical parse trees for identical valid source files, although for files which fail to parse correctly, they do not emit the same error messages.

## Setup

_Note_: As of March 2020, the recommended setup for hacking on `mcc` is to just use [nix](https://nixos.org/nix/download.html). Running `nix-shell --pure` at the root of the project _should_ drop you into a shell that has the right versions of ghc, llvm, clang, and all the libraries that `mcc` depends on. The `default.nix` file included is pinned to a particular nixpkgs commit, so hopefully this will continue to be true in perpetuity. To run the test suite, `nix-shell --pure --run 'cabal new-test'` should do. Using stack to build `mcc` is no longer supported, as I don't use it anymore. The legacy instructions below are included for posterity.

### Manual installation

As an alternative to using `nix`, you can use the following recipe:

1. Assuming that you have `ghc` and `cabal` installed (at least versions 8.8 and 3.2, recommended), install `alex` and `happy`:

```sh
> cabal update
> cabal install alex happy
```

and update `~/.cabal/config` to refer to them. For example:

```
  alex-location: /Users/xxx/.cabal/bin/alex
  happy-location: /Users/xxx/.cabal/bin/happy
```

2. Install `llvm`, which includes `llc`, `clang`, and a host of other compiler tools.
   Go to http://releases.llvm.org/download.html for further information. There are
   also some more [detailed instructions](https://github.com/llvm-hs/llvm-hs#installing-llvm).

For macOS, for example, use `brew install llvm` and add `/usr/local/opt/llvm/bin` to your `$PATH`.

3. Check that

```sh
> llvm-config --libs
-lLLVM-12
```

works. You may need to create the symlinks described at step 5 in the
more [detailed instructions](https://github.com/llvm-hs/llvm-hs#installing-llvm) on how
to install llvm.

Note that unlike the OCaml version of MicroC, this project requires a more cecent version of LLVM.
(These instauctions use LLVM 12.)

4. Pull down the necessary sources and make sure that you have the branches
   that correspond to the version of `llvm` that you are using checked out:

```sh
> mkdir myproject
> cd myproject
> git clone https://github.com/llvm-hs/llvm-hs.git
> git checkout llvm-12
> git clone https://github.com/llvm-hs/llvm-hs-pretty.git
> git checkout llvm-12
> git clone https://github.com/jmorag/mcc.git
> git checkout llvm-12
```

5. Create a `cabal.project` file in `myproject` (to ensure that you are using the right version
   of `llvm-hs`. (Otherwise you risk using the hackage version, which may be
   problematic)

```
packages: ./llvm-hs-pretty
          ./llvm-hs/llvm-hs-pure
          ./llvm-hs/llvm-hs
          ./mcc
```

6. Build and test `mcc`:

```sh
> cd mcc
> cabal build
> cabal test
```

7. You can either install `mcc` to your `~/.cabal/bin` by running `cabal install exe:mcc` or you can `cabal run mcc` from the `mcc` folder:

```sh
❯ cabal run mcc -- --help
Up to date
Usage: mcc [(-a|--ast) | (-s|--sast) | (-l|--llvm) | (-c|--compile) [-o FILE]]
           FILE [--combinator | (-g|--generator)]
  Run the mcc compiler on the given file. Passing no flags will compile the
  file, execute it, and print the output.

Available options:
  -h,--help                Show this help text
  -a,--ast                 Pretty print the ast
  -s,--sast                Pretty print the sast
  -l,--llvm                Pretty print the generated llvm
  -c,--compile             Compile to an executable
  FILE                     Source file
  --combinator             Use the megaparsec parser implementation (default).
  -g,--generator           Use alex and happy to parse.

> cabal run mcc -- tests/pass/test-add1.mc
Up to date
42
```

### Legacy Setup

`mcc` requires the `stack` package manager for Haskell. To install `stack`, see https://docs.haskellstack.org/en/stable/README/. It also requires that `clang` and `llc` binaries be available in your PATH. There are two options to ensure that these will be available:

1. **Use Nix**:
   Using `stack`'s `nix` integration will set up a local environment with the correct versions of `clang` and `llc` installed and added to the PATH. To acquire nix, follow the installation [instructions](https://nixos.org/nix/download.html). Nix is enabled by default, so simply running `stack test` should work if everything is nix and stack are installed correctly.
2. **Manual Installation**:
   To install the `llvm` toolchain, which includes `llc`, `clang`, and a host of other compiler tools, go to http://releases.llvm.org/download.html#6.0.1. For macOS, the maintainers of the Haskell-LLVM bindings provide a brew tap, so running `brew install llvm-hs/llvm/llvm-6.0` and adding `/usr/local/opt/llvm/bin` to your `$PATH` should be sufficient. They also provide more [detailed instructions](https://github.com/llvm-hs/llvm-hs#installing-llvm) on how to install llvm on other platforms. Note that unlike the OCaml version of MicroC, this project requires LLVM 6, not LLVM 3.4. To use stack commands without nix integration, pass the `--no-nix` flag to them.

### Running and testing the compiler

Once everything is installed, to run the compiler's test suite, navigate to the project home directory and run `stack test`. To build compiler executable, run `stack build mcc`. Remember to add `--no-nix` to these commands if you would like to disable `nix` integration.

## Questions, contributions

Please don't hesitate to open an issue or pull request if you don't understand something in here or you see something that could be improved! Once most of the functionality is complete, I'm planning on releasing a detailed walkthrough on the whole compilation process in a series of posts, but until then, documentation is very sparse.
