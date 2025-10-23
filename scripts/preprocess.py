#!/usr/bin/python3

import os
import re
import sys

from tempfile import NamedTemporaryFile

prefix = "__preprocessed_"

translation = [
    ("_Atomic bool", "atomic_bool"),
    ("_Atomic _Bool", "atomic_bool"),
    ("_Atomic char", "atomic_char"),
    ("_Atomic signed char", "atomic_schar"),
    ("_Atomic unsigned char", "atomic_uchar"),
    ("_Atomic short", "atomic_short"),
    ("_Atomic unsigned short", "atomic_ushort"),
    ("_Atomic int", "atomic_int"),
    ("_Atomic unsigned int", "atomic_uint"),
    ("_Atomic long", "atomic_long"),
    ("_Atomic unsigned long", "atomic_ulong"),
    ("_Atomic long long", "atomic_llong"),
    ("_Atomic unsigned long long", "atomic_ullong"),
    ("_Atomic short unsigned int", "atomic_char16_t"),
    ("_Atomic unsigned int", "atomic_char32_t"),
    ("_Atomic long int", "atomic_wchar_t"),
    ("_Atomic signed char", "atomic_int_least8_t"),
    ("_Atomic unsigned char", "atomic_uint_least8_t"),
    ("_Atomic short int", "atomic_int_least16_t"),
    ("_Atomic short unsigned int", "atomic_uint_least16_t"),
    ("_Atomic int", "atomic_int_least32_t"),
    ("_Atomic unsigned int", "atomic_uint_least32_t"),
    ("_Atomic long long int", "atomic_int_least64_t"),
    ("_Atomic long long unsigned int", "atomic_uint_least64_t"),
    ("_Atomic signed char", "atomic_int_fast8_t"),
    ("_Atomic unsigned char", "atomic_uint_fast8_t"),
    ("_Atomic int", "atomic_int_fast16_t"),
    ("_Atomic unsigned int", "atomic_uint_fast16_t"),
    ("_Atomic int", "atomic_int_fast32_t"),
    ("_Atomic unsigned int", "atomic_uint_fast32_t"),
    ("_Atomic long long int", "atomic_int_fast64_t"),
    ("_Atomic long long unsigned int", "atomic_uint_fast64_t"),
    ("_Atomic int", "atomic_intptr_t"),
    ("_Atomic unsigned int", "atomic_uintptr_t"),
    ("_Atomic unsigned int", "atomic_size_t"),
    ("_Atomic int", "atomic_ptrdiff_t"),
    ("_Atomic long long int", "atomic_intmax_t"),
    ("_Atomic long long unsigned int", "atomic_uintmax_t"),
    ("__float128", "float"),
]


# Frama-C does not support variable length arrays at non-final fields. We replace them by
# arrays of constant size 2**32.
#
# We need to be careful to do not modify array accesses.
translation_re = [
    (
        r" *(?:unsigned|signed){0,1}\s*(?:char|int|short|long|long long|float|double|void|struct)[^;=\)\(]*\s*\[0U{0,1}\] *;",
        r"0U{0,1}",
        f"{(2**32)-1}",
    ),
]


def preprocess(orig_path, lines):
    res = []

    for line in lines:
        if "typedef _Atomic" in line:
            print("[Preprocessor] Replacing atomic type: ", line, end="")
            new_line = line.replace("_Atomic", "")
        else:
            new_line = line

            for original, new in translation:
                new_line = new_line.replace(original, new)

            for line_pattern, pattern, target in translation_re:
                for m in re.findall(line_pattern, new_line):
                    print(
                        "[Preprocessor] Replacing VLA with fixed sized array: "
                        + new_line,
                        end="",
                    )
                    new_line = re.sub(pattern, target, new_line)
                    print("  ==> ", new_line, end="")

        res.append(new_line)

    _, filename = os.path.split(orig_path)
    filename = prefix + filename
    tmp_file = NamedTemporaryFile(
        prefix=filename, suffix=".c", delete=False, dir="/tmp"
    )

    with open(tmp_file.name, "w") as f:
        f.write("".join(res))

    return tmp_file.name


if __name__ == "__main__":
    if len(sys.argv) != 2:
        exit(1)

    res = preprocess(sys.argv[1])

    if res is None:
        print("No change")
    else:
        print(res)
