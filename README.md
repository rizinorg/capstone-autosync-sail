This is the repo of the generator tool capstone-autosync-sail, a tool that ingests the https://github.com/riscv/sail-riscv executable model of the RISCV instruction set and generates most of a disassembler for the architecture.

# What is Sail ?

Sail is a Domain Specific Language, a specialized programming language made for a narrow, singular purpose and not designed for general development of arbitrary programs (although it might end up, nonetheless, suitable for general development anyway). SQL and regular expressions are often-cited examples of DSLs. 

Sail’s specialized domain is describing Instruction Set Architectures in an executable form.

# Why Sail ?

Describing ISAs like RISCV and x86 (among others) in machine-readable executable formats has many rationales:

1- ISAs are increasingly too complex for natural language. Even just describing the sequential semantics of an instruction - how it intreprets its addressing mode(s), how it fetches operands, how it computes a result, what registers/memory and/or any other architectural state it modifies or clobbers or saves or restores - can get unmanagebly tedious and confusing in natural language. 

Add to that the fact that ISAs often specify addtional semantics on top of simple sequential execution of instruction streams: traps, faults, interrupts, privilege levels, ABI conventions, memory models, etc…, and it becomes obvious that describing an ISA is itself a computationally complex task that requires a suitable formal language.

2- Formal models enable automatic generation of ISA artifacts from a single source of truth. It’s clear, for instance, that assemblers and disassemblers are related, they “contain the same information”, in a certain sense. It follows that it’s wasteful and error-prone to write them by hand: it constitutes a duplication of information. 

A well-defined executable model of the ISA could in principle contain the core information needed to generate both. An assembler-generator would ingest this model and generate an assembler, and a disassembler-generator would ingest the same model and generate a disassembler. This is commonly called the [Model-Driven Engineering](https://en.wikipedia.org/wiki/Model-driven_engineering) development methodology. Sail enables the application of MDE to the development of ISA artifacts.

# Why is Sail a new language?

The fact that Sail is an entirely new language and not a library, a framework or an internal DSL in an existing one deserves an explanation. Being a new language enables Sail to optimize its syntax and semantics for its domain, at the cost of larger development effort for the Sail compiler team.

For instance, the following are 2 features that Sail provides specifically to syntax-sugar ISA development:

 1- Dependent typing: Sail can track values on the type level, such that it's possible to define the number 2 as a type, and derive a new type (e.g. 4) from it using type-level arithmetics. Dependent typing is an advanced feature not present in the vast majority of mainstream languages, even functional ones. Sail uses this advanced type system to ensure additional type safety and consistency checking for architectural parameters (e.g. register file width) at compile time.

2- Bidirectional mappings: Sail mappings are functions that can be run in both direction. Their type signature is a hint of their nature: while functions are typed A→ B, meaning they derive values of type B from values of type A, mappings are typed A↔B, meaning they can go “back and forth” from values of any type to values of the other. Mappings can be a very concise way of specifying bidirectional computations, such as describing how to obtain a structured object representing an instruction from the binary encoding of the instruction, how to parse the binary form of the instruction, and at the same time also how to serialize the structured object back to its binary encoding.

# How to run

The `end2end-smoke-test` workflow file in the CI directory (.github) setups the generator’s environment from scratch on a new machine. It should be viewed as the authoritative guide on how to run the generator on a new machine, even if it gets out of sync of this README. (In such case, please update the README to reflect the file.)

The steps are reproduced below with explanations: 

## 1- Download and install Ocaml toolchain

Ocaml has an installation guide [here](https://ocaml.org/install#linux_mac_bsd), but in a nutshell, the following commands are enough on most *nix systems (any Linux, MacOS, any BSD):

```bash
sudo apt-get install bubblewrap
bash -c "sh <(curl -fsSL [https://opam.ocaml.org/install.sh](https://opam.ocaml.org/install.sh))"
opam init
opam install dune
```

And this is indeed what the workflow file does, with the exception that it pipes values 

to serve as interactive inputs to the first 2 commands because can’t require user inputs.

- The `bubblewrap` package is a low-level interface for creating and managing sandboxed environments, `opam` requires it by default on Linux systems to run arbitrary code safely
- The second command downloads Ocaml and installs it,
- The third command initializes `opam`, Ocaml’s package downloader/installer and toolchain manager (manages the installation of different Ocaml compilers on the same machine), and
- The fourth downloads and installs `dune`, Ocaml’s package manager.

> P.S. It would be correct to say that exactly one out of {`opam`, `dune`} is actually needed, but alas, that’s a problem for the Ocaml language community to sort out, not this tool.
> 

The following are optional development dependencies that aren’t strictly needed for running the generator, but might be useful if you want to modify the generator:

```bash
opam install ocaml-lsp-server odoc ocamlformat utop
```

## 2- Install generator dependencies

The generator tool itself has almost no dependencies except Libsail, the Sail compiler exposed as a library. It also requires installing Sail itself as a shell command because the Sail standard library must be downloaded, and requires the system-level command line tool `z3`, the popular SMT solver from Microsoft Research, exposed as a cmd tool.

In commands:

```bash
git clone https://github.com/rizinorg/capstone-autosync-sail.git
cd capstone-autosync-sail && source ~/.bash_profile
export OPAMCONFIRMLEVEL=yes
opam update

opam install sail=0.18
sudo apt-get install z3
opam install . --deps-only
```

It’s very important to fix the exact Sail version, as above, because Sail is in heavy development and every version breaks its predecessor’s APIs. The version is determined by what the maintainers of the `sail-riscv` model choose.  

## 3- Fetch the sail-riscv model

Because Sail is a large language and this tool is *not* a general purpose Sail→C compiler, only a subset of Sail’s features is actually supported. More generally, the tool also makes some specific assumptions about its input code, for example that the logic to stringify instructions is a mapping (and not, e.g., a function). Taken together, those 2 facts imply that the input sail-riscv model could break the tool if it violates those assumptions or uses more Sail features than what the tool supports.

Therefore, `capstone-autosync-sail` versions its input. In `sail.hash.txt`, there is a git sha hash that states the latest sail-riscv version that the tool ran successfully on. It’s generally preferable for this version to not lag the sail-riscv model latest commit on master by too much (~1 month worth of commits are the limits), and it’s a bug in `capstone-autosync-sail` if it crashes on the latest master commit. 

To get the input model, run the following in the parent directory of the tool:

```bash
git clone https://github.com/riscv/sail-riscv.git
cd sail-riscv
git reset --hard $(cat sail.hash.txt)
```

## 4- Run the generator on the input

Finally, run the following command from the generator’s directory to obtain the generated C code:

```bash
source ~/.bash_profile && dune exec --profile release -- riscv_disasm_from_sail -f conf/sail-files-paths.txt
```

## 5- Or just copy riscv_disasm

The outputs of the generator for the model version specified by sail.hash.txt is also kept in this repo, this is a quality-of-life feature for 2 reasons: 

1- If a commit to `capstone-autosync-sail` changed the generated code, the diff to the generated code is accompanied by the diff to the generator’s logic as an explanation 

2- Users can simply copy the generated code instead of going through steps 1..4 above

As a downside, if the generated C code is used elsewhere, such as in a RISCV module in https://github.com/capstone-engine/capstone disassembler framework (the primary purpose and raison d'être of this tool), this means that the handwritten code in riscv_disasm (all the files not having “.gen” in their names) MUST NOT be modified in that site of use, only here in this repo.

# Updating the input model version

To update the version of sail-riscv, you have to update conf/sail-files-paths.txt and conf/hash.txt. To update the first, run :

```bash
tools/riscv-ls.sh <path-to-riscv-model>
```

This will (ab)use the makefile in the sail-riscv repo to print an ordered list of all files that comprise the model. This is also what should be done if the sail-riscv directory downloaded happens to not be the direct filesystem-sibling of the capstone-autosync-sail directory, as the paths in the committed conf/sail-files-paths.txt assume this relationship. You can override the committed capstone-autosync-sail paths by running the above command on any valid path to the sail-riscv model directory. (Please don't commit the resulting conf/sail-files.paths.txt)

Updating the hash must be done manually as of now, query the sail-riscv repo version (e.g. using git log) and paste the version in sail.hash.txt.