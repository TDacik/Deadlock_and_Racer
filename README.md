# RacerF & DeadlockF

*RacerF* and *DeadlockF* are static analysers for detection of data races and deadlocks in multi-threaded C programs with mutexes. Both tools are implemented as plugins of the [Frama-C](http://frama-c.com/) platform.

## Installation

The recent versions of the plugins are compatible with [Frama-C 29.0 (Copper)](https://frama-c.com/fc-versions/copper.html). 

For installation, `opam` package manager and OCaml >= 5.1 is necessary. After cloning this repository, run `opam install .`

## Usage

The simplest way to run the plugins is:
``` 
frama-c -racer source_file1.c source_file2.c ...
frama-c -deadlock source_file1.c source_file2.c ...
```

Several commandline options can be specified. The options related to a specific plugin are prefixed `-racer` and `-deadlock`, respectively, the general options are prefixed by `-cc`.
Run `frama-c [racer|deadlock]-h to see more details.

### Running via wrapper script
The script *racerf.py* can be used to run RacerF using a combination of under- and over-approximating strategy as follows:
``` 
./scripts/racerf.py -machdep=[gcc_x86_32|gcc_x86_64] [--under|--over] [--debug] [frama-c-options] source_file.c
```
The options `--under` and `--over` run just under- and --over approximation respectively. 
By default, the combined strategy is used.

Currently only single source file can be analysed using the wrapper. 
Moreover, machine model of Frama-C needs to be one of `gcc_x86_32` or `gcc_x86_64`.




## Related papers

* Tomáš Dacík and Tomáš Vojnar. [RacerF: Lightweight Static Data Race Detection for C Code](https://arxiv.org/pdf/2502.04905). ECOOP 2025.

* Tomáš Dacík and Tomáš Vojnar. [RacerF: Data Race Detection with Frama-C (Competition Contribution)](https://arxiv.org/pdf/2502.20052). SV-COMP 2025.

* Tomáš Dacík. [Static Analysis in the Frama-C Environment Focused on Deadlock Detection](https://www.fit.vut.cz/study/thesis/22928/.en). Bachelor's Thesis. Brno University of Technoloy. 2020. Supervised by Tomáš Vojnar.

## Contact
If you have any questions, do not hesitate to contact the tool/method authors:
* [**Tomáš Dacík**](https://www.fit.vut.cz/person/idacik/) <[idacik@fit.vut.cz](mailto:idacik@fit.vut.cz)>
* [**Tomáš Vojnar**](https://www.muni.cz/lide/134390-tomas-vojnar) <[vojnar@fi.muni.cz](mailto:vojnar@fi.muni.cz)>

## License
The plugin is available under MIT license.
