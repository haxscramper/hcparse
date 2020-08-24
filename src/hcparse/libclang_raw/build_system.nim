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


proc clang_getBuildSessionTimestamp*(
): culonglong {.
    cdecl,
    importc: "clang_getBuildSessionTimestamp",
    dynlib: libclang
  .}

type CXVirtualFileOverlay* = distinct ptr[CXVirtualFileOverlayImpl] # CXVirtualFileOverlay

proc clang_VirtualFileOverlay_create*(
  options: cuint, # `unsigned int`
): CXVirtualFileOverlay {.
    cdecl,
    importc: "clang_VirtualFileOverlay_create",
    dynlib: libclang
  .}

proc clang_VirtualFileOverlay_addFileMapping*(
  arg_0: CXVirtualFileOverlay, # `CXVirtualFileOverlay`
  virtualPath: ptr[cstring], # `const char *`
  realPath: ptr[cstring], # `const char *`
): CXErrorCode {.
    cdecl,
    importc: "clang_VirtualFileOverlay_addFileMapping",
    dynlib: libclang
  .}

proc clang_VirtualFileOverlay_setCaseSensitivity*(
  arg_0: CXVirtualFileOverlay, # `CXVirtualFileOverlay`
  caseSensitive: cint, # `int`
): CXErrorCode {.
    cdecl,
    importc: "clang_VirtualFileOverlay_setCaseSensitivity",
    dynlib: libclang
  .}

proc clang_VirtualFileOverlay_writeToBuffer*(
  arg_0: CXVirtualFileOverlay, # `CXVirtualFileOverlay`
  options: cuint, # `unsigned int`
  out_buffer_ptr: ptr[ptr[cstring]], # `char **`
  out_buffer_size: ptr[cuint], # `unsigned int *`
): CXErrorCode {.
    cdecl,
    importc: "clang_VirtualFileOverlay_writeToBuffer",
    dynlib: libclang
  .}

proc clang_free*(
  buffer: pointer, # `void *`
): void {.
    cdecl,
    importc: "clang_free",
    dynlib: libclang
  .}

proc clang_VirtualFileOverlay_dispose*(
  arg_0: CXVirtualFileOverlay, # `CXVirtualFileOverlay`
): void {.
    cdecl,
    importc: "clang_VirtualFileOverlay_dispose",
    dynlib: libclang
  .}

type CXModuleMapDescriptor* = distinct ptr[CXModuleMapDescriptorImpl] # CXModuleMapDescriptor

proc clang_ModuleMapDescriptor_create*(
  options: cuint, # `unsigned int`
): CXModuleMapDescriptor {.
    cdecl,
    importc: "clang_ModuleMapDescriptor_create",
    dynlib: libclang
  .}

proc clang_ModuleMapDescriptor_setFrameworkModuleName*(
  arg_0: CXModuleMapDescriptor, # `CXModuleMapDescriptor`
  name: ptr[cstring], # `const char *`
): CXErrorCode {.
    cdecl,
    importc: "clang_ModuleMapDescriptor_setFrameworkModuleName",
    dynlib: libclang
  .}

proc clang_ModuleMapDescriptor_setUmbrellaHeader*(
  arg_0: CXModuleMapDescriptor, # `CXModuleMapDescriptor`
  name: ptr[cstring], # `const char *`
): CXErrorCode {.
    cdecl,
    importc: "clang_ModuleMapDescriptor_setUmbrellaHeader",
    dynlib: libclang
  .}

proc clang_ModuleMapDescriptor_writeToBuffer*(
  arg_0: CXModuleMapDescriptor, # `CXModuleMapDescriptor`
  options: cuint, # `unsigned int`
  out_buffer_ptr: ptr[ptr[cstring]], # `char **`
  out_buffer_size: ptr[cuint], # `unsigned int *`
): CXErrorCode {.
    cdecl,
    importc: "clang_ModuleMapDescriptor_writeToBuffer",
    dynlib: libclang
  .}

proc clang_ModuleMapDescriptor_dispose*(
  arg_0: CXModuleMapDescriptor, # `CXModuleMapDescriptor`
): void {.
    cdecl,
    importc: "clang_ModuleMapDescriptor_dispose",
    dynlib: libclang
  .}

