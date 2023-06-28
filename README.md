# Kaleidoscope

This is a starter program to get used to LLVM. 
Credit to the LLVM documentation website and Toby Ho for the helpful guides on LLVM.
 
LLVM tutorial documentation on kaleidoscope[^1]: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html

Toby Ho's tutorial on kaleidoscope: https://www.youtube.com/playlist?list=PLSq9OFrD2Q3ChEc_ejnBcO5u9JeT0ufkg

## Requirements

This code is compiled with the most recent stable version of llvm and clang as of 26th June 2023, that is:
- llvm >= 15.0.7
- clang >= 15.0.7

## How to run

```bash
$ clang++-15 -g my_lang.cpp `llvm-config-15 --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toy.out
$ ./toy.out
```

[^1]: The tutoral seems to be updated using the lastest developing version. So there are libaries that is in the tutorial/current version, but is not in older versions of llvm. For learning llvm with older builds. I recommend checking code listings of older snapshots. This is the one that I have used (version 15.0.7): https://github.com/llvm/llvm-project/tree/8dfdcc7b7bf66834a761bd8de445840ef68e4d1a/llvm/examples/Kaleidoscope