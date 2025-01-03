(lang dune 3.15)

(name capstone-autosync-sail)

(generate_opam_files true)

(source
 (github rizinorg/capstone-autosync-sail))

(authors "Mostafa M. Kamal (moste00)")

(maintainers "Mostafa M. Kamal (moste00)")

(license BSD-3-Clause)

(documentation https://url/to/documentation)

(package
 (name capstone-autosync-sail)
 (synopsis "A tool that generates a capstone disassembler module for RISC-V ISA from the Sail ISA description for RISC-V")
 (description "The tool depends on the Sail ISA description language compiler (which is an Ocaml library) to ingest the Sail model of RISC-V and generate a Capstone-compliant disassembler")
 (depends ocaml dune (libsail (= 0.18)))
 (tags
  (RISCV Disassembly Assembly Capstone "Reverse-Engineering" Rizin Sail ISA)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
