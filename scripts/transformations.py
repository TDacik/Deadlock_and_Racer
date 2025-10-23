"""Program transformations for under- and over-approximating analysis."""

import os

from pathlib import Path
from subprocess import run
from tempfile import NamedTemporaryFile


def transformation(config, name, source_file, options, header=""):
    """
    Generic transformation function.
    """
    config.log(f"Running {name} transformation")

    path = Path(source_file)
    suffix = f".{name}.c"

    print(path.stem)
    tmp_file = NamedTemporaryFile(
        prefix=path.stem, suffix=suffix, delete=False, dir="/tmp"
    )

    cmd = [
        config.framac_path,
        f'-cpp-extra-args="-include'
        + os.path.join(config.models_path, "__racerf_atomic_typedefs.h")
        + '"',
        *options,
        source_file,
        "-then",
        "-print",
        "-ocode",
        tmp_file.name,
    ]

    config.log(f"Running preprocessing with command: " + " ".join(cmd))

    _ = run(cmd)

    with open(tmp_file.name, "r+") as f:
        content = f.read()
        f.seek(0, 0)
        f.write("int tmp_0;\n")  # TODO: why those are needed?
        f.write("int tmp_60;\n")  #
        f.write(header)
        f.write(content)

    return tmp_file.name


def transform_under_approx(config, source_file, options):
    additional_options = [
        "-ulevel=2",
        "-cc-inline",
    ]
    return transformation(
        config, "under-approximation", source_file, options + additional_options
    )


def transform_over_approx(config, source_file, options):
    additional_options = [
        "-cc-preprocess",
    ]
    header = "__thread int nondet;\n" + "int get_nondet();\n"
    return transformation(
        config, "over-approximation", source_file, options + additional_options, header
    )
