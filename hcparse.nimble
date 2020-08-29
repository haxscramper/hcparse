# Package

version       = "0.1.0"
author        = "haxscramper"
description   = "High-level nim wrapper for C/C++ parsing"
license       = "Apache-2.0"
srcDir        = "src"



# Dependencies

requires "nim >= 1.2.6"
requires "hnimast", "hmisc >= 0.4.0", "hpprint", "nimtraits"

# import shell

# task c2nim_build, "Generate base file libclang wrapper":
