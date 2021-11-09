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


