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
  CXErrorCode* {.size: sizeof(cint).} = enum
    CXError_Success = 0
    CXError_Failure = 1
    CXError_Crashed = 2
    CXError_InvalidArguments = 3
    CXError_ASTReadError = 4

