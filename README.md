Ada 2020 - Generators/Coroutines prototype
==========================================

This repository hosts a prototype for generators/coroutines support in Ada.
This prototype includes:

* a thin Ada binding to PCL (Portable Coroutine Library): see the `pcl`
  directory.
* a wrapper around it to integrate nicely with the GNAT runtime: see the
  `coroutine` directory.
* a library leveraging this to provide generators capabilities: see the
  `generators` directory.

Requirements
------------

In order to use this prototype, one has first to install PCL (Portable
Coroutine Library): <http://www.xmailserver.org/libpcl.html> and make the
library and its headers available to the toolchain.

Then, all the `*.gpr` files must be made available to GPRbuild adding all the
directories to the `GPR_PROJECT_PATH` environment variables.

Usage
-----

This is only a prototype so there is no documentation yet! That being said, if
you want to use this prototype, take a loot at the `coroutines/tests` and
`generators/tests` subdirectories: in particular all the `.adb` source files.

If you want to run the testsuite, go to the `test` directory and then build the
testcases:

```sh
$ gprbuild -Ptests
```

Then, execute the `run.py` scripts: the testsuite results will be displayed on
The standard output.
