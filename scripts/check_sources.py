"""Checks for unsupported features."""

from preprocess import preprocess


def check(content):
    for line in content:
        if not "extern" in line:
            if "sem_wait" in line:
                print("unsupported: semaphore")
                exit(10)


def check_sources(source_files):
    acc = []
    for fname in source_files:
        with open(fname, "r", encoding="latin1") as f:
            lines = f.readlines()
        check(lines)
        acc.append(preprocess(fname, lines))
    return acc


def check_and_get_sources(cmdline):
    """Splits command line into lists of source files and options."""
    source_files = []
    options = []
    for arg in cmdline:
        if arg.endswith(".i") or arg.endswith(".c"):
            source_files.append(arg)
        else:
            options.append(arg)
    return check_sources(source_files), options, source_files
