#!/usr/bin/python3

"""
Wrapper script for running RacerF.

"""

import os
import sys
import argparse

from subprocess import run, PIPE, STDOUT

from check_sources import check_and_get_sources
from transformations import transform_under_approx, transform_over_approx


def remove_if_exists(path):
    try:
        os.remove(path)
    except FileNotFoundError:
        pass

class Runner:
    def __init__(self, svcomp_mode=False, debug=False):
        self.svcomp_mode = svcomp_mode
        self.debug = debug
        if svcomp_mode:
            assert False # TODO
            return
        
        self.framac_path = "frama-c"
        try:
            p = run(["frama-c", "-print-share-path"], capture_output=True)
            path = p.stdout.decode()
            self.libc_path = os.path.join(path, "libc")
        except Exception as e:
            print("Cannot find Frama-C library")
            raise e
        try:
            p = run(["frama-c", "-print-share-path"], capture_output=True)
            path = p.stdout.decode()
            self.models_path = os.path.join(path, "cc/models/")
        except Exception as e:
            print("Cannot find RacerF models")
            raise e

    def __str__(self):
        return f"Binary: {self.framac_path}\nLib: {self.libc_path}\nModels: {self.models_path}"
   
    def log(self, msg):
        color = "\033[96m"
        white = "\033[0m"
        if self.debug:
            print(color + msg + white, file=sys.stderr)

    def version(self):
        cmd = [self.framac_path, "-racer-version"]
        process = run(cmd)
        return

    def run_racer_once(self, source_file, options):
        silent_kernel = [
            "-eva-verbose=0",
            "-eva-mlevel=1",
            "-kernel-verbose=1",
            "-eva-warn-key=*=inactive",
            "-kernel-warn-key=*=inactive",
        ]

        cmd = [
            self.framac_path,
            f'-cpp-extra-args="-include'
            + os.path.join(self.models_path, "__racerf_atomic_typedefs.h")
            + '"',
            # Needs to be added directly, not with '-include'!
            os.path.join(self.libc_path, "stdlib.h"),
            "-racer",
            "-cc-profile",
            "-cc-stats",
            "-racer-witness-path=witness.graphml",
            *silent_kernel,
            *options,
            source_file,
        ]

        self.log("Running Frama-C with command: " + " ".join(cmd))
        sys.stdout.flush()

        process = run(cmd, stdout=PIPE, stderr=STDOUT, universal_newlines=True)
        return process

    def run_over_approx(self, source_file, options, orig_source_file):
        source_file_over = transform_over_approx(self, source_file, options)
        options = options + ["-cc-thread-approx=over", "-cc-orig-sources", orig_source_file]
        return self.run_racer_once(source_file_over, options)

    def run_under_approx(self, source_file, options, orig_source_file):
        source_file_under = transform_under_approx(self, source_file, options)
        options = options + ["-cc-thread-approx=under", "-cc-orig-sources", orig_source_file]
        return self.run_racer_once(source_file_under, options)

    def run_racer(self, source_file, options, args, orig_source_file):
        if args.over_approx:
            res = self.run_over_approx(source_file, options, orig_source_file)
            print(res.stdout)
            return res.returncode
        elif args.under_approx:
            res = self.run_under_approx(source_file, optionsm, orig_source_file)
            print(res.stdout)
            return res.returncode
        else: # Run both
            res1 = self.run_over_approx(source_file, options, orig_source_file)
            print(res1.stdout) # TODO: stderr?
            if "[racer] Data race " not in str(res1.stdout):
                return res1.returncode

            self.log("Over-approximation was inconclusive\n")
            # Remove possible witness from over-approx
            remove_if_exists("witness.graphml")

            res2 = self.run_under_approx(source_file, options, orig_source_file)
            print(res2.stdout)

            if "[racer] Data race " in res2.stdout:
                return res2.returncode

            # We need to print this to match our short-sighted defininition of Benchexec's tool info
            print("[SV-wrapper] The result may be imprecise")
            return 0

    def setup(self):
        """
        We need to create symlink to _opam in /tmp to deal with absolute paths".
        """
        if not self.sv_comp_mode:
            return

        if not os.path.exists(OPAM_SYMLINK):
            try:
                opam_path = os.path.abspath(OPAM_PATH)
                os.symlink(OPAM_PATH, OPAM_SYMLINK)
            except Exception:
                print("Initialization of _opam symlink failed")
                raise Exception

def parse_args():
    args = sys.argv[1:]
    is_script_arg = lambda x: x.startswith("--")

    # We silently assume that valueas are passed as --opt=value
    script_args = list(filter(is_script_arg, args))
    racerf_args = list(filter(lambda x: not is_script_arg(x), args))

    if not any(arg.startswith("-machdep") for arg in racerf_args):
        racerf_args.append("-machdep=gcc_x86_32")

    parser = argparse.ArgumentParser()
    parser.add_argument("--version", action="store_true")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--sv-comp", action="store_true", default=True)
    parser.add_argument("--under-approx", action="store_true")
    parser.add_argument("--over-approx", action="store_true")
    return parser.parse_args(script_args), racerf_args

def main():
    args, racerf_args = parse_args()
    runner = Runner(sv_comp_mode=args.sv_comp, debug=args.debug)
    runner.log(str(runner))
    runner.setup()
    if args.version:
        runner.version()
    else:
        sources, options, orig_sources = check_and_get_sources(racerf_args)
        if len(sources) != 1:
            print(sources)
            print("Currently, only one source file can be analysed by this wrapper")
            exit(1)
        res = runner.run_racer(sources[0], options, args, orig_sources[0])
        exit(res)

if __name__ == "__main__":
    main()
