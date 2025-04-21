#!/usr/bin/env python3

import sys
import re
from glob import glob

PAT_SPACES = r"\s+"
PAT_HEX_NUM = r"(?P<Num>0x[0-9a-fA-F]+)"
PAT_STR = r'(?P<Str>".*")'

def print_usage():
    print("Usage: riscv_gen_csr_name_map <path-to-model-directory>")

def parse_args(args):
    if len(args) != 1:
        print_usage()
        sys.exit(1)
    
    return args[0]

model_dir = parse_args(sys.argv[1:])

pattern = re.compile(r"csr_name_map ="+ PAT_SPACES + PAT_HEX_NUM + PAT_SPACES + r"<->" + PAT_SPACES + PAT_STR + PAT_SPACES)  
# for some reason, the model repeats itself and defines several clauses for the same address with the same value
seen = set()
for filename in glob("*.sail", root_dir = model_dir):
    with open(model_dir + filename) as file:
        for line in file:
             result = pattern.search(line)
             if result and result.groupdict()["Num"].lower() not in seen:
                print(f"""case {result.groupdict()["Num"].lower()}:
                        SStream_concat(ss, {result.groupdict()["Str"]});
                        return;\n""")
                seen.add(result.groupdict()["Num"].lower())

print(
"""
default: 
    hex_bits_12(csr, ss, ctx);
    return;\n
""")