from times import Time
{.deadCodeElim: on.}
{.push callconv: cdecl.}
import opaque_impls

when defined(windows):
  const
    libclang = "libclang.dll"
elif defined(macosx):
  const
    libclang = "libclang.dylib"
else:
  const
    libclang = "libclang.so"


type
  CXString* {.pure, bycopy.} = object
    data*: pointer # `const void *`
    private_flags*: cuint # `unsigned int`

type
  CXStringSet* {.pure, bycopy.} = object
    strings*: ptr[CXString] # `CXString *`
    count*: cuint # `unsigned int`

proc clang_getCString*(
  string: CXString, # `CXString`
): ptr[cstring] {.
    cdecl,
    importc: "clang_getCString",
    dynlib: libclang
  .}

proc clang_disposeString*(
  string: CXString, # `CXString`
): void {.
    cdecl,
    importc: "clang_disposeString",
    dynlib: libclang
  .}

proc clang_disposeStringSet*(
  set: ptr[CXStringSet], # `CXStringSet *`
): void {.
    cdecl,
    importc: "clang_disposeStringSet",
    dynlib: libclang
  .}

