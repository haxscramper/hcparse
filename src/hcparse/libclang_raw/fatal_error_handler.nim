from times import Time
{.deadCodeElim: on.}
{.push callconv: cdecl.}
import opaque_impls

when defined(windows):
  const
    libclang* = "libclang.dll"
elif defined(macosx):
  const
    libclang* = "libclang.dylib"
else:
  const
    libclang* = "libclang.so"


proc clang_install_aborting_llvm_fatal_error_handler*(
): void {.
    cdecl,
    importc: "clang_install_aborting_llvm_fatal_error_handler",
    dynlib: libclang
  .}

proc clang_uninstall_llvm_fatal_error_handler*(
): void {.
    cdecl,
    importc: "clang_uninstall_llvm_fatal_error_handler",
    dynlib: libclang
  .}

