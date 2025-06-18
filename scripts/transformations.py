"""Program transformations for under- and over-approximating analysis."""

import os
from subprocess import run


def transformation(config, name, source_file, options, header=""):
    """
    Generic transformation function.

    TODO: don't generate the file in the working directory.
    """
    config.log(f"Running {name} transformation")

    dirpath, filename = os.path.split(source_file)
    newname = "__" + name + "_" + filename
    path = os.path.join(dirpath, newname)

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
        path,
    ]

    config.log(f"Running preprocessing with command: " + " ".join(cmd))

    _ = run(cmd)

    with open(path, "r+") as f:
        content = f.read()
        f.seek(0, 0)
        f.write("int tmp_0;\n")  # TODO: why those are needed?
        f.write("int tmp_60;\n")  #
        f.write(header)
        f.write(content)

    return path


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
