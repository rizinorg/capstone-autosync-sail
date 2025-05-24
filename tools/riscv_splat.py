#!/usr/bin/env python3

import sys

SPLAT_MARKER = "//> RISCV instruction"
END_SPLAT_MARKER = "};"

def print_usage():
    print("Usage: riscv_splat <src-file> --into <dst-file>")
    print("(OR")
    print("    riscv_splat --into <dst-file> <src-file>")
    print(")")

def parse_args(args):
    if len(args) != 4:
        print_usage()
        sys.exit(1)

    _, a1, a2, a3 = args
    # dst always follows a "--into"

    # means dst is a2
    if a1 == "--into":
        return a3, a2
    # means dst is a3
    if a2 == "--into":
        return a1, a3

    # bad state
    print_usage()
    sys.exit(1)

src, dst = parse_args(sys.argv)

content = []
num_matches = 0
skip_till_end_marker = False
with open(dst, "r") as dst_file:
    for line in dst_file:
        if not skip_till_end_marker:
            content.append(line)
            # if the line matches the splat marker modulo case and space
            if line.strip().lower() == SPLAT_MARKER.lower():
                num_matches += 1
                # splat the src file after it
                with open(src, "r") as src_file:
                    content += src_file.readlines() 
                skip_till_end_marker = True
        else:
            skip_till_end_marker = not(line.strip().lower() == END_SPLAT_MARKER.lower())


with open(dst, "w") as dst_file:
    dst_file.writelines(content)           

if num_matches == 0:
    print("Error: Splat marker not found in the given destination file, destination file unchanged")
else:
    if num_matches > 1:
        print("Warning: Splat marker found several times in the given destination file")
        print("Destination file contains multiple copies of the source file")