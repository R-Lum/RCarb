## Release summary

Follow up submission to account for the issue raised by Uwe Ligges.

> ##Example with file output
> write_InputTemplate(file = "~/Desktop/Input.csv")

The example was already wrapped in by a `\dontrun{}` comment and with it not
really different from the example given in, e.g., `utils::write.table()`
Anyway, we changed it to:

`#write_InputTemplate(file = "[YOUR PATH]")`

(including the `#`). The suggestion `tempdir()` was not used to avoid confusion; some
unexperienced users may think that `tempdir()` is part of the needed call. 

## Reverse dependency checks

This package has not yet external dependencies.

## Test environments
* local macOS Mojava 10.14.1, Xcode 10.0.1, R-devel and R-release (patched)
* on AppVeyor CI
  * i386-w64-mingw32/i386 (32-bit), R-devel
  * x86_64_w64-mingw32/64 (64-bit), R-devel
  * x86_64_w64-mingw32/64 (64-bit), R-release
  * i386-w64-mingw32/i386 (32-bit), R-stable
* on Travis CI
  * Ubuntu 14.04.5 LTS, oldrel
  * Ubuntu 14.04.5 LTS, release
  * Ubuntu 14.04.5 LTS, devel
  * macOS Sierra 10.13, R-release, Apple LLVM version 9.1.0 (clang-902.0.39.2)
