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


type CXCompilationDatabase* = distinct pointer # CXCompilationDatabase

type CXCompileCommands* = distinct pointer # CXCompileCommands

type CXCompileCommand* = distinct pointer # CXCompileCommand

type
  CXCompilationDatabase_Error* {.pure, size: sizeof(cint).} = enum
    CXCompilationDatabase_NoError = 0
    CXCompilationDatabase_CanNotLoadDatabase = 1

proc clang_CompilationDatabase_fromDirectory*(
  buildDir: ptr[cstring], # `const char *`
  errorCode: ptr[CXCompilationDatabase_Error], # `CXCompilationDatabase_Error *`
): CXCompilationDatabase {.
    cdecl,
    importc: "clang_CompilationDatabase_fromDirectory",
    dynlib: libclang
  .}

proc clang_CompilationDatabase_dispose*(
  arg_0: CXCompilationDatabase, # `CXCompilationDatabase`
): void {.
    cdecl,
    importc: "clang_CompilationDatabase_dispose",
    dynlib: libclang
  .}

proc clang_CompilationDatabase_getCompileCommands*(
  arg_0: CXCompilationDatabase, # `CXCompilationDatabase`
  completeFileName: ptr[cstring], # `const char *`
): CXCompileCommands {.
    cdecl,
    importc: "clang_CompilationDatabase_getCompileCommands",
    dynlib: libclang
  .}

proc clang_CompilationDatabase_getAllCompileCommands*(
  arg_0: CXCompilationDatabase, # `CXCompilationDatabase`
): CXCompileCommands {.
    cdecl,
    importc: "clang_CompilationDatabase_getAllCompileCommands",
    dynlib: libclang
  .}

proc clang_CompileCommands_dispose*(
  arg_0: CXCompileCommands, # `CXCompileCommands`
): void {.
    cdecl,
    importc: "clang_CompileCommands_dispose",
    dynlib: libclang
  .}

proc clang_CompileCommands_getSize*(
  arg_0: CXCompileCommands, # `CXCompileCommands`
): cuint {.
    cdecl,
    importc: "clang_CompileCommands_getSize",
    dynlib: libclang
  .}

proc clang_CompileCommands_getCommand*(
  arg_0: CXCompileCommands, # `CXCompileCommands`
  i: cuint, # `unsigned int`
): CXCompileCommand {.
    cdecl,
    importc: "clang_CompileCommands_getCommand",
    dynlib: libclang
  .}

proc clang_CompileCommand_getDirectory*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
): CXString {.
    cdecl,
    importc: "clang_CompileCommand_getDirectory",
    dynlib: libclang
  .}

proc clang_CompileCommand_getFilename*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
): CXString {.
    cdecl,
    importc: "clang_CompileCommand_getFilename",
    dynlib: libclang
  .}

proc clang_CompileCommand_getNumArgs*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
): cuint {.
    cdecl,
    importc: "clang_CompileCommand_getNumArgs",
    dynlib: libclang
  .}

proc clang_CompileCommand_getArg*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
  i: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_CompileCommand_getArg",
    dynlib: libclang
  .}

proc clang_CompileCommand_getNumMappedSources*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
): cuint {.
    cdecl,
    importc: "clang_CompileCommand_getNumMappedSources",
    dynlib: libclang
  .}

proc clang_CompileCommand_getMappedSourcePath*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
  i: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_CompileCommand_getMappedSourcePath",
    dynlib: libclang
  .}

proc clang_CompileCommand_getMappedSourceContent*(
  arg_0: CXCompileCommand, # `CXCompileCommand`
  i: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_CompileCommand_getMappedSourceContent",
    dynlib: libclang
  .}

