
{.deadCodeElim: on.}
{.push, callconv: cdecl.}
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
  CXVirtualFileOverlayImpl* = object
  CXModuleMapDescriptorImpl* = object
  time_t* = clong
proc install_aborting_llvm_fatal_error_handler*(): void {.cdecl, dynlib: libclang,
    importc: "clang_install_aborting_llvm_fatal_error_handler".}
  ##  Installs error handler that prints error message to stderr and calls abort().
  ##  Replaces currently installed error handler (if any).
proc uninstall_llvm_fatal_error_handler*(): void {.cdecl, dynlib: libclang,
    importc: "clang_uninstall_llvm_fatal_error_handler".}
  ##  Removes currently installed error handler (if any).
  ##  If no error handler is intalled, the default strategy is to print error
  ##  message to stderr and call exit(1).
type
  CXErrorCode* = enum           ##  Error codes returned by libclang routines.
                   ##  Zero (
                   ##  is the only error code indicating success.  Other
                   ##  error codes, including not yet assigned non-zero values, indicate errors.
    ecSuccess = 0,              ##  No error.
    ecFailure = 1, ##  A generic error code, no further details are available.
                ##  Errors of this kind can get their own specific error codes in future
                ##  libclang versions.
    ecCrashed = 2,              ##  libclang crashed while performing the requested operation.
    ecInvalidArguments = 3, ##  The function detected that the arguments violate the function
                         ##  contract.
    ecASTReadError = 4          ##  An AST deserialization error has occurred.
type
  CXString* {.pure, bycopy.} = object
    data*: pointer
    private_flags*: cuint

type
  CXStringSet* {.pure, bycopy.} = object
    strings*: ptr[CXString]
    count*: cuint

proc getCString*(cxstring: CXString): cstring {.cdecl, dynlib: libclang,
    importc: "clang_getCString".}
  ##  Retrieve the character data associated with the given string.
proc disposeString*(cxstring: CXString): void {.cdecl, dynlib: libclang,
    importc: "clang_disposeString".}
  ##  Free the given string.
proc disposeStringSet*(cxset: ptr[CXStringSet]): void {.cdecl, dynlib: libclang,
    importc: "clang_disposeStringSet".}
  ##  Free the given string set.
type
  CXCompilationDatabase* = distinct pointer
type
  CXCompileCommands* = distinct pointer
type
  CXCompileCommand* = distinct pointer
type
  CXCompilationDatabase_Error* = enum ##  Error codes for Compilation Database
    cdeNoError = 0, cdeCanNotLoadDatabase = 1
proc compilationDatabase_fromDirectory*(buildDir: cstring; errorCode: ptr[
    CXCompilationDatabase_Error]): CXCompilationDatabase {.cdecl, dynlib: libclang,
    importc: "clang_CompilationDatabase_fromDirectory".}
  ##  Creates a compilation database from the database found in directory
  ##  buildDir. For example, CMake can output a compile_commands.json which can
  ##  be used to build the database.
  ##  It must be freed by
proc dispose*(argCXCompilationDatabase: CXCompilationDatabase): void {.cdecl,
    dynlib: libclang, importc: "clang_CompilationDatabase_dispose".}
  ##  Free the given compilation database
proc getCompileCommands*(argCXCompilationDatabase: CXCompilationDatabase;
                        completeFileName: cstring): CXCompileCommands {.cdecl,
    dynlib: libclang, importc: "clang_CompilationDatabase_getCompileCommands".}
  ##  Find the compile commands used for a file. The compile commands
  ##  must be freed by
proc getAllCompileCommands*(argCXCompilationDatabase: CXCompilationDatabase): CXCompileCommands {.
    cdecl, dynlib: libclang,
    importc: "clang_CompilationDatabase_getAllCompileCommands".}
  ##  Get all the compile commands in the given compilation database.
proc dispose*(argCXCompileCommands: CXCompileCommands): void {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommands_dispose".}
  ##  Free the given CompileCommands
proc getSize*(argCXCompileCommands: CXCompileCommands): cuint {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommands_getSize".}
  ##  Get the number of CompileCommand we have for a file
proc getCommand*(argCXCompileCommands: CXCompileCommands; i: cuint): CXCompileCommand {.
    cdecl, dynlib: libclang, importc: "clang_CompileCommands_getCommand".}
  ##  Get the I'th CompileCommand for a file
  ##  Note : 0
  ## <
  ## = i
  ## <
  ##  clang_CompileCommands_getSize(CXCompileCommands)
proc getDirectory*(argCXCompileCommand: CXCompileCommand): CXString {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommand_getDirectory".}
  ##  Get the working directory where the CompileCommand was executed from
proc getFilename*(argCXCompileCommand: CXCompileCommand): CXString {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommand_getFilename".}
  ##  Get the filename associated with the CompileCommand.
proc getNumArgs*(argCXCompileCommand: CXCompileCommand): cuint {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommand_getNumArgs".}
  ##  Get the number of arguments in the compiler invocation.
proc getArg*(argCXCompileCommand: CXCompileCommand; i: cuint): CXString {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommand_getArg".}
  ##  Get the I'th argument value in the compiler invocations
  ##  Invariant :
  ##   - argument 0 is the compiler executable
proc getNumMappedSources*(argCXCompileCommand: CXCompileCommand): cuint {.cdecl,
    dynlib: libclang, importc: "clang_CompileCommand_getNumMappedSources".}
  ##  Get the number of source mappings for the compiler invocation.
proc getMappedSourcePath*(argCXCompileCommand: CXCompileCommand; i: cuint): CXString {.
    cdecl, dynlib: libclang, importc: "clang_CompileCommand_getMappedSourcePath".}
  ##  Get the I'th mapped source path for the compiler invocation.
proc getMappedSourceContent*(argCXCompileCommand: CXCompileCommand; i: cuint): CXString {.
    cdecl, dynlib: libclang, importc: "clang_CompileCommand_getMappedSourceContent".}
  ##  Get the I'th mapped source content for the compiler invocation.
type
  CXIndex* = distinct pointer
type
  CXTargetInfoImpl* {.pure, bycopy.} = object

type
  CXTargetInfo* = distinct ptr[CXTargetInfoImpl]
type
  CXTranslationUnitImpl* {.pure, bycopy.} = object

type
  CXTranslationUnit* = distinct ptr[CXTranslationUnitImpl]
type
  CXClientData* = distinct pointer
type
  CXUnsavedFile* {.pure, bycopy.} = object
    filename*: cstring
    contents*: cstring
    length*: culong

type
  CXAvailabilityKind* = enum ##  Describes the availability of a particular entity, which indicates
                          ##  whether the use of this entity will result in a warning or error due to
                          ##  it being deprecated or unavailable.
    akAvailable,              ##  The entity is available.
    akDeprecated, ##  The entity is available, but has been deprecated (and its use is
                 ##  not recommended).
    akNotAvailable,           ##  The entity is not available; any use of it will be an error.
    akNotAccessible ##  The entity is available, but not accessible; any use of it will be
                   ##  an error.
type
  CXVersion* {.pure, bycopy.} = object
    major*: cint
    minor*: cint
    subminor*: cint

type
  CXCursor_ExceptionSpecificationKind* = enum ##  Describes the exception specification of a cursor.
                                           ##  A negative value indicates that the cursor is not a function declaration.
    ceskNone,                 ##  The cursor has no exception specification.
    ceskDynamicNone,          ##  The cursor has exception specification throw()
    ceskDynamic,              ##  The cursor has exception specification throw(T1, T2)
    ceskMSAny,                ##  The cursor has exception specification throw(...).
    ceskBasicNoexcept,        ##  The cursor has exception specification basic noexcept.
    ceskComputedNoexcept,     ##  The cursor has exception specification computed noexcept.
    ceskUnevaluated,          ##  The exception specification has not yet been evaluated.
    ceskUninstantiated,       ##  The exception specification has not yet been instantiated.
    ceskUnparsed,             ##  The exception specification has not been parsed yet.
    ceskNoThrow               ##  The cursor has a __declspec(nothrow) exception specification.
proc createIndex*(excludeDeclarationsFromPCH: cint; displayDiagnostics: cint): CXIndex {.
    cdecl, dynlib: libclang, importc: "clang_createIndex".}
  ##  Provides a shared context for creating translation units.
  ##  It provides two options:
  ##  - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local"
  ##  declarations (when loading any new translation units). A "local" declaration
  ##  is one that belongs in the translation unit itself and not in a precompiled
  ##  header that was used by the translation unit. If zero, all declarations
  ##  will be enumerated.
  ##  Here is an example:
  ## Error: cannot render: rnCodeBlock
  ##  This process of creating the 'pch', loading it separately, and using it (via
  ##  -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks
  ##  (which gives the indexer the same performance benefit as the compiler).
proc disposeIndex*(index: CXIndex): void {.cdecl, dynlib: libclang,
                                       importc: "clang_disposeIndex".}
  ##  Destroy the given index.
  ##  The index must not be destroyed until all of the translation units created
  ##  within that index have been destroyed.
type
  CXGlobalOptFlags* = enum
    gofNone = 0,                ##  Used to indicate that no special CXIndex options are needed.
    gofThreadBackgroundPriorityForIndexing = 1, ##  Used to indicate that threads that libclang creates for indexing
                                             ##  purposes should use background priority.
                                             ##  Affects #clang_indexSourceFile, #clang_indexTranslationUnit,
                                             ##  #clang_parseTranslationUnit, #clang_saveTranslationUnit.
    gofThreadBackgroundPriorityForEditing = 2, ##  Used to indicate that threads that libclang creates for editing
                                            ##  purposes should use background priority.
                                            ##  Affects #clang_reparseTranslationUnit, #clang_codeCompleteAt,
                                            ##  #clang_annotateTokens
    gofThreadBackgroundPriorityForAll = 3 ##  Used to indicate that all threads that libclang creates should use
                                       ##  background priority.
proc setGlobalOptions*(argCXIndex: CXIndex; options: cuint): void {.cdecl,
    dynlib: libclang, importc: "clang_CXIndex_setGlobalOptions".}
  ##  Sets general options associated with a CXIndex.
  ##  For example:
  ## Error: cannot render: rnCodeBlock
  ## **options**
  ##  A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
proc getGlobalOptions*(argCXIndex: CXIndex): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXIndex_getGlobalOptions".}
  ##  Gets the general options associated with a CXIndex.
  ## **
  ##  A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that
  ##  are associated with the given CXIndex object.
proc setInvocationEmissionPathOption*(argCXIndex: CXIndex; path: cstring): void {.
    cdecl, dynlib: libclang,
    importc: "clang_CXIndex_setInvocationEmissionPathOption".}
  ##  Sets the invocation emission path option in a CXIndex.
  ##  The invocation emission path specifies a path which will contain log
  ##  files for certain libclang invocations. A null value (default) implies that
  ##  libclang invocations are not logged..
type
  CXFile* = distinct pointer
proc getFileName*(sFile: CXFile): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getFileName".}
  ##  Retrieve the complete file and path name of the given file.
proc getFileTime*(sFile: CXFile): time_t {.cdecl, dynlib: libclang,
                                       importc: "clang_getFileTime".}
  ##  Retrieve the last modification time of the given file.
type
  CXFileUniqueID* {.pure, bycopy.} = object
    data*: array[3, culonglong]

proc getFileUniqueID*(file: CXFile; outID: ptr[CXFileUniqueID]): cint {.cdecl,
    dynlib: libclang, importc: "clang_getFileUniqueID".}
  ##  Retrieve the unique ID for the given
  ## **file**
  ##  the file to get the ID for.
  ## **outID**
  ##  stores the returned CXFileUniqueID.
  ## **
  ##  If there was a failure getting the unique ID, returns non-zero,
  ##  otherwise returns 0.
proc isFileMultipleIncludeGuarded*(tu: CXTranslationUnit; file: CXFile): cuint {.
    cdecl, dynlib: libclang, importc: "clang_isFileMultipleIncludeGuarded".}
  ##  Determine whether the given header is guarded against
  ##  multiple inclusions, either with the conventional
  ## #
  ## ifndef/
  ## #
  ## define/
  ## #
  ## endif macro guards or with
  ## #
  ## pragma once.
proc getFile*(tu: CXTranslationUnit; file_name: cstring): CXFile {.cdecl,
    dynlib: libclang, importc: "clang_getFile".}
  ##  Retrieve a file handle within the given translation unit.
  ## **tu**
  ##  the translation unit
  ## **file_name**
  ##  the name of the file.
  ## **
  ##  the file handle for the named file in the translation unit
  ##  or a NULL file handle if the file was not a part of this translation unit.
proc getFileContents*(tu: CXTranslationUnit; file: CXFile; size: ptr[cint]): cstring {.
    cdecl, dynlib: libclang, importc: "clang_getFileContents".}
proc isEqual*(file1: CXFile; file2: CXFile): cint {.cdecl, dynlib: libclang,
    importc: "clang_File_isEqual".}
  ##  Returns non-zero if the
  ##  and
  ##  point to the same file,
  ##  or they are both NULL.
proc tryGetRealPathName*(file: CXFile): CXString {.cdecl, dynlib: libclang,
    importc: "clang_File_tryGetRealPathName".}
  ##  Returns the real path name of
  ##  An empty string may be returned. Use
  ##  in that case.
type
  CXSourceLocation* {.pure, bycopy.} = object
    ptr_data*: array[2, pointer]
    int_data*: cuint

type
  CXSourceRange* {.pure, bycopy.} = object
    ptr_data*: array[2, pointer]
    begin_int_data*: cuint
    end_int_data*: cuint

proc getNullLocation*(): CXSourceLocation {.cdecl, dynlib: libclang,
    importc: "clang_getNullLocation".}
  ##  Retrieve a NULL (invalid) source location.
proc equalLocations*(loc1: CXSourceLocation; loc2: CXSourceLocation): cuint {.cdecl,
    dynlib: libclang, importc: "clang_equalLocations".}
  ##  Determine whether two source locations, which must refer into
  ##  the same translation unit, refer to exactly the same point in the source
  ##  code.
  ## **
  ##  non-zero if the source locations refer to the same location, zero
  ##  if they refer to different locations.
proc getLocation*(tu: CXTranslationUnit; file: CXFile; line: cuint; column: cuint): CXSourceLocation {.
    cdecl, dynlib: libclang, importc: "clang_getLocation".}
  ##  Retrieves the source location associated with a given file/line/column
  ##  in a particular translation unit.
proc getLocationForOffset*(tu: CXTranslationUnit; file: CXFile; offset: cuint): CXSourceLocation {.
    cdecl, dynlib: libclang, importc: "clang_getLocationForOffset".}
  ##  Retrieves the source location associated with a given character offset
  ##  in a particular translation unit.
proc location_isInSystemHeader*(location: CXSourceLocation): cint {.cdecl,
    dynlib: libclang, importc: "clang_Location_isInSystemHeader".}
  ##  Returns non-zero if the given source location is in a system header.
proc location_isFromMainFile*(location: CXSourceLocation): cint {.cdecl,
    dynlib: libclang, importc: "clang_Location_isFromMainFile".}
  ##  Returns non-zero if the given source location is in the main file of
  ##  the corresponding translation unit.
proc getNullRange*(): CXSourceRange {.cdecl, dynlib: libclang,
                                   importc: "clang_getNullRange".}
  ##  Retrieve a NULL (invalid) source range.
proc getRange*(cxbegin: CXSourceLocation; cxend: CXSourceLocation): CXSourceRange {.
    cdecl, dynlib: libclang, importc: "clang_getRange".}
  ##  Retrieve a source range given the beginning and ending source
  ##  locations.
proc equalRanges*(range1: CXSourceRange; range2: CXSourceRange): cuint {.cdecl,
    dynlib: libclang, importc: "clang_equalRanges".}
  ##  Determine whether two ranges are equivalent.
  ## **
  ##  non-zero if the ranges are the same, zero if they differ.
proc range_isNull*(cxrange: CXSourceRange): cint {.cdecl, dynlib: libclang,
    importc: "clang_Range_isNull".}
  ##  Returns non-zero if
  ##  is null.
proc getExpansionLocation*(location: CXSourceLocation; file: ptr[CXFile];
                          line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]): void {.
    cdecl, dynlib: libclang, importc: "clang_getExpansionLocation".}
  ##  Retrieve the file, line, column, and offset represented by
  ##  the given source location.
  ##  If the location refers into a macro expansion, retrieves the
  ##  location of the macro expansion.
  ## **location**
  ##  the location within a source file that will be decomposed
  ##  into its parts.
  ## **file**
  ##  [out] if non-NULL, will be set to the file to which the given
  ##  source location points.
  ## **line**
  ##  [out] if non-NULL, will be set to the line to which the given
  ##  source location points.
  ## **column**
  ##  [out] if non-NULL, will be set to the column to which the given
  ##  source location points.
  ## **offset**
  ##  [out] if non-NULL, will be set to the offset into the
  ##  buffer to which the given source location points.
proc getPresumedLocation*(location: CXSourceLocation; filename: ptr[CXString];
                         line: ptr[cuint]; column: ptr[cuint]): void {.cdecl,
    dynlib: libclang, importc: "clang_getPresumedLocation".}
  ##  Retrieve the file, line and column represented by the given source
  ##  location, as specified in a # line directive.
  ##  Example: given the following source code in a file somefile.c
  ## Error: cannot render: rnCodeBlock
  ##  the location information returned by this function would be
  ##  File: dummy.c Line: 124 Column: 12
  ##  whereas clang_getExpansionLocation would have returned
  ##  File: somefile.c Line: 3 Column: 12
  ## **location**
  ##  the location within a source file that will be decomposed
  ##  into its parts.
  ## **filename**
  ##  [out] if non-NULL, will be set to the filename of the
  ##  source location. Note that filenames returned will be for "virtual" files,
  ##  which don't necessarily exist on the machine running clang - e.g. when
  ##  parsing preprocessed output obtained from a different environment. If
  ##  a non-NULL value is passed in, remember to dispose of the returned value
  ##  using
  ##  once you've finished with it. For an invalid
  ##  source location, an empty string is returned.
  ## **line**
  ##  [out] if non-NULL, will be set to the line number of the
  ##  source location. For an invalid source location, zero is returned.
  ## **column**
  ##  [out] if non-NULL, will be set to the column number of the
  ##  source location. For an invalid source location, zero is returned.
proc getInstantiationLocation*(location: CXSourceLocation; file: ptr[CXFile];
                              line: ptr[cuint]; column: ptr[cuint];
                              offset: ptr[cuint]): void {.cdecl, dynlib: libclang,
    importc: "clang_getInstantiationLocation".}
  ##  Legacy API to retrieve the file, line, column, and offset represented
  ##  by the given source location.
  ##  This interface has been replaced by the newer interface
  ##  #clang_getExpansionLocation(). See that interface's documentation for
  ##  details.
proc getSpellingLocation*(location: CXSourceLocation; file: ptr[CXFile];
                         line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]): void {.
    cdecl, dynlib: libclang, importc: "clang_getSpellingLocation".}
  ##  Retrieve the file, line, column, and offset represented by
  ##  the given source location.
  ##  If the location refers into a macro instantiation, return where the
  ##  location was originally spelled in the source file.
  ## **location**
  ##  the location within a source file that will be decomposed
  ##  into its parts.
  ## **file**
  ##  [out] if non-NULL, will be set to the file to which the given
  ##  source location points.
  ## **line**
  ##  [out] if non-NULL, will be set to the line to which the given
  ##  source location points.
  ## **column**
  ##  [out] if non-NULL, will be set to the column to which the given
  ##  source location points.
  ## **offset**
  ##  [out] if non-NULL, will be set to the offset into the
  ##  buffer to which the given source location points.
proc getFileLocation*(location: CXSourceLocation; file: ptr[CXFile];
                     line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]): void {.
    cdecl, dynlib: libclang, importc: "clang_getFileLocation".}
  ##  Retrieve the file, line, column, and offset represented by
  ##  the given source location.
  ##  If the location refers into a macro expansion, return where the macro was
  ##  expanded or where the macro argument was written, if the location points at
  ##  a macro argument.
  ## **location**
  ##  the location within a source file that will be decomposed
  ##  into its parts.
  ## **file**
  ##  [out] if non-NULL, will be set to the file to which the given
  ##  source location points.
  ## **line**
  ##  [out] if non-NULL, will be set to the line to which the given
  ##  source location points.
  ## **column**
  ##  [out] if non-NULL, will be set to the column to which the given
  ##  source location points.
  ## **offset**
  ##  [out] if non-NULL, will be set to the offset into the
  ##  buffer to which the given source location points.
proc getRangeStart*(cxrange: CXSourceRange): CXSourceLocation {.cdecl,
    dynlib: libclang, importc: "clang_getRangeStart".}
  ##  Retrieve a source location representing the first character within a
  ##  source range.
proc getRangeEnd*(cxrange: CXSourceRange): CXSourceLocation {.cdecl,
    dynlib: libclang, importc: "clang_getRangeEnd".}
  ##  Retrieve a source location representing the last character within a
  ##  source range.
type
  CXSourceRangeList* {.pure, bycopy.} = object
    count*: cuint
    ranges*: ptr[CXSourceRange]

proc getSkippedRanges*(tu: CXTranslationUnit; file: CXFile): ptr[CXSourceRangeList] {.
    cdecl, dynlib: libclang, importc: "clang_getSkippedRanges".}
  ##  Retrieve all ranges that were skipped by the preprocessor.
  ##  The preprocessor will skip lines when they are surrounded by an
  ##  if/ifdef/ifndef directive whose condition does not evaluate to true.
proc getAllSkippedRanges*(tu: CXTranslationUnit): ptr[CXSourceRangeList] {.cdecl,
    dynlib: libclang, importc: "clang_getAllSkippedRanges".}
  ##  Retrieve all ranges from all files that were skipped by the
  ##  preprocessor.
  ##  The preprocessor will skip lines when they are surrounded by an
  ##  if/ifdef/ifndef directive whose condition does not evaluate to true.
proc disposeSourceRangeList*(ranges: ptr[CXSourceRangeList]): void {.cdecl,
    dynlib: libclang, importc: "clang_disposeSourceRangeList".}
  ##  Destroy the given
type
  CXDiagnosticSeverity* = enum  ##  Describes the severity of a particular diagnostic.
    dsIgnored = 0, ##  A diagnostic that has been suppressed, e.g., by a command-line
                ##  option.
    dsNote = 1, ##  This diagnostic is a note that should be attached to the
             ##  previous (non-note) diagnostic.
    dsWarning = 2, ##  This diagnostic indicates suspicious code that may not be
                ##  wrong.
    dsError = 3,                ##  This diagnostic indicates that the code is ill-formed.
    dsFatal = 4 ##  This diagnostic indicates that the code is ill-formed such
             ##  that future parser recovery is unlikely to produce useful
             ##  results.
type
  CXDiagnostic* = distinct pointer
type
  CXDiagnosticSet* = distinct pointer
proc getNumDiagnosticsInSet*(diags: CXDiagnosticSet): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getNumDiagnosticsInSet".}
  ##  Determine the number of diagnostics in a CXDiagnosticSet.
proc getDiagnosticInSet*(diags: CXDiagnosticSet; index: cuint): CXDiagnostic {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticInSet".}
  ##  Retrieve a diagnostic associated with the given CXDiagnosticSet.
  ## **Diags**
  ##  the CXDiagnosticSet to query.
  ## **Index**
  ##  the zero-based diagnostic number to retrieve.
  ## **
  ##  the requested diagnostic. This diagnostic must be freed
  ##  via a call to
type
  CXLoadDiag_Error* = enum      ##  Describes the kind of error that occurred (if any) in a call to
    ldeNone = 0,                ##  Indicates that no error occurred.
    ldeUnknown = 1, ##  Indicates that an unknown error occurred while attempting to
                 ##  deserialize diagnostics.
    ldeCannotLoad = 2, ##  Indicates that the file containing the serialized diagnostics
                    ##  could not be opened.
    ldeInvalidFile = 3 ##  Indicates that the serialized diagnostics file is invalid or
                    ##  corrupt.
proc loadDiagnostics*(file: cstring; error: ptr[CXLoadDiag_Error];
                     errorString: ptr[CXString]): CXDiagnosticSet {.cdecl,
    dynlib: libclang, importc: "clang_loadDiagnostics".}
  ##  Deserialize a set of diagnostics from a Clang diagnostics bitcode
  ##  file.
  ## **file**
  ##  The name of the file to deserialize.
  ## **error**
  ##  A pointer to a enum value recording if there was a problem
  ##         deserializing the diagnostics.
  ## **errorString**
  ##  A pointer to a CXString for recording the error string
  ##         if the file was not successfully loaded.
  ## **
  ##  A loaded CXDiagnosticSet if successful, and NULL otherwise.  These
  ##  diagnostics should be released using clang_disposeDiagnosticSet().
proc disposeDiagnosticSet*(diags: CXDiagnosticSet): void {.cdecl, dynlib: libclang,
    importc: "clang_disposeDiagnosticSet".}
  ##  Release a CXDiagnosticSet and all of its contained diagnostics.
proc getChildDiagnostics*(d: CXDiagnostic): CXDiagnosticSet {.cdecl,
    dynlib: libclang, importc: "clang_getChildDiagnostics".}
  ##  Retrieve the child diagnostics of a CXDiagnostic.
  ##  This CXDiagnosticSet does not need to be released by
  ##  clang_disposeDiagnosticSet.
proc getNumDiagnostics*(unit: CXTranslationUnit): cuint {.cdecl, dynlib: libclang,
    importc: "clang_getNumDiagnostics".}
  ##  Determine the number of diagnostics produced for the given
  ##  translation unit.
proc getDiagnostic*(unit: CXTranslationUnit; index: cuint): CXDiagnostic {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnostic".}
  ##  Retrieve a diagnostic associated with the given translation unit.
  ## **Unit**
  ##  the translation unit to query.
  ## **Index**
  ##  the zero-based diagnostic number to retrieve.
  ## **
  ##  the requested diagnostic. This diagnostic must be freed
  ##  via a call to
proc getDiagnosticSetFromTU*(unit: CXTranslationUnit): CXDiagnosticSet {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticSetFromTU".}
  ##  Retrieve the complete set of diagnostics associated with a
  ##         translation unit.
  ## **Unit**
  ##  the translation unit to query.
proc disposeDiagnostic*(diagnostic: CXDiagnostic): void {.cdecl, dynlib: libclang,
    importc: "clang_disposeDiagnostic".}
  ##  Destroy a diagnostic.
type
  CXDiagnosticDisplayOptions* = enum ##  Options to control the display of diagnostics.
                                  ##  The values in this enum are meant to be combined to customize the
                                  ##  behavior of
    ddoDisplaySourceLocation = 1, ##  Display the source-location information where the
                               ##  diagnostic was located.
                               ##  When set, diagnostics will be prefixed by the file, line, and
                               ##  (optionally) column to which the diagnostic refers. For example,
                               ## Error: cannot render: rnCodeBlock
                               ##  This option corresponds to the clang flag
    ddoDisplayColumn = 2, ##  If displaying the source-location information of the
                       ##  diagnostic, also include the column number.
                       ##  This option corresponds to the clang flag
    ddoDisplaySourceRanges = 4, ##  If displaying the source-location information of the
                             ##  diagnostic, also include information about source ranges in a
                             ##  machine-parsable format.
                             ##  This option corresponds to the clang flag
    ddoDisplayOption = 8, ##  Display the option name associated with this diagnostic, if any.
                       ##  The option name displayed (e.g., -Wconversion) will be placed in brackets
                       ##  after the diagnostic text. This option corresponds to the clang flag
    ddoDisplayCategoryId = 16, ##  Display the category number associated with this diagnostic, if any.
                            ##  The category number is displayed within brackets after the diagnostic text.
                            ##  This option corresponds to the clang flag
    ddoDisplayCategoryName = 32 ##  Display the category name associated with this diagnostic, if any.
                             ##  The category name is displayed within brackets after the diagnostic text.
                             ##  This option corresponds to the clang flag
proc formatDiagnostic*(diagnostic: CXDiagnostic; options: cuint): CXString {.cdecl,
    dynlib: libclang, importc: "clang_formatDiagnostic".}
  ##  Format the given diagnostic in a manner that is suitable for display.
  ##  This routine will format the given diagnostic to a string, rendering
  ##  the diagnostic according to the various options given. The
  ##  function returns the set of
  ##  options that most closely mimics the behavior of the clang compiler.
  ## **Diagnostic**
  ##  The diagnostic to print.
  ## **Options**
  ##  A set of options that control the diagnostic display,
  ##  created by combining
  ##  values.
  ## **
  ##  A new string containing for formatted diagnostic.
proc defaultDiagnosticDisplayOptions*(): cuint {.cdecl, dynlib: libclang,
    importc: "clang_defaultDiagnosticDisplayOptions".}
  ##  Retrieve the set of display options most similar to the
  ##  default behavior of the clang compiler.
  ## **
  ##  A set of display options suitable for use with
proc getDiagnosticSeverity*(argCXDiagnostic: CXDiagnostic): CXDiagnosticSeverity {.
    cdecl, dynlib: libclang, importc: "clang_getDiagnosticSeverity".}
  ##  Determine the severity of the given diagnostic.
proc getDiagnosticLocation*(argCXDiagnostic: CXDiagnostic): CXSourceLocation {.
    cdecl, dynlib: libclang, importc: "clang_getDiagnosticLocation".}
  ##  Retrieve the source location of the given diagnostic.
  ##  This location is where Clang would print the caret ('^') when
  ##  displaying the diagnostic on the command line.
proc getDiagnosticSpelling*(argCXDiagnostic: CXDiagnostic): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticSpelling".}
  ##  Retrieve the text of the given diagnostic.
proc getDiagnosticOption*(diag: CXDiagnostic; disable: ptr[CXString]): CXString {.
    cdecl, dynlib: libclang, importc: "clang_getDiagnosticOption".}
  ##  Retrieve the name of the command-line option that enabled this
  ##  diagnostic.
  ## **Diag**
  ##  The diagnostic to be queried.
  ## **Disable**
  ##  If non-NULL, will be set to the option that disables this
  ##  diagnostic (if any).
  ## **
  ##  A string that contains the command-line option used to enable this
  ##  warning, such as "-Wconversion" or "-pedantic".
proc getDiagnosticCategory*(argCXDiagnostic: CXDiagnostic): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticCategory".}
  ##  Retrieve the category number for this diagnostic.
  ##  Diagnostics can be categorized into groups along with other, related
  ##  diagnostics (e.g., diagnostics under the same warning flag). This routine
  ##  retrieves the category number for the given diagnostic.
  ## **
  ##  The number of the category that contains this diagnostic, or zero
  ##  if this diagnostic is uncategorized.
proc getDiagnosticCategoryName*(category: cuint): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getDiagnosticCategoryName".}
  ##  Retrieve the name of a particular diagnostic category.  This
  ##   is now deprecated.  Use clang_getDiagnosticCategoryText()
  ##   instead.
  ## **Category**
  ##  A diagnostic category number, as returned by
  ## **
  ##  The name of the given diagnostic category.
proc getDiagnosticCategoryText*(argCXDiagnostic: CXDiagnostic): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticCategoryText".}
  ##  Retrieve the diagnostic category text for a given diagnostic.
  ## **
  ##  The text of the given diagnostic category.
proc getDiagnosticNumRanges*(argCXDiagnostic: CXDiagnostic): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticNumRanges".}
  ##  Determine the number of source ranges associated with the given
  ##  diagnostic.
proc getDiagnosticRange*(diagnostic: CXDiagnostic; cxrange: cuint): CXSourceRange {.
    cdecl, dynlib: libclang, importc: "clang_getDiagnosticRange".}
  ##  Retrieve a source range associated with the diagnostic.
  ##  A diagnostic's source ranges highlight important elements in the source
  ##  code. On the command line, Clang displays source ranges by
  ##  underlining them with '~' characters.
  ## **Diagnostic**
  ##  the diagnostic whose range is being extracted.
  ## **Range**
  ##  the zero-based index specifying which range to
  ## **
  ##  the requested source range.
proc getDiagnosticNumFixIts*(diagnostic: CXDiagnostic): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticNumFixIts".}
  ##  Determine the number of fix-it hints associated with the
  ##  given diagnostic.
proc getDiagnosticFixIt*(diagnostic: CXDiagnostic; fixIt: cuint;
                        replacementRange: ptr[CXSourceRange]): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticFixIt".}
  ##  Retrieve the replacement information for a given fix-it.
  ##  Fix-its are described in terms of a source range whose contents
  ##  should be replaced by a string. This approach generalizes over
  ##  three kinds of operations: removal of source code (the range covers
  ##  the code to be removed and the replacement string is empty),
  ##  replacement of source code (the range covers the code to be
  ##  replaced and the replacement string provides the new code), and
  ##  insertion (both the start and end of the range point at the
  ##  insertion location, and the replacement string provides the text to
  ##  insert).
  ## **Diagnostic**
  ##  The diagnostic whose fix-its are being queried.
  ## **FixIt**
  ##  The zero-based index of the fix-it.
  ## **ReplacementRange**
  ##  The source range whose contents will be
  ##  replaced with the returned replacement string. Note that source
  ##  ranges are half-open ranges [a, b), so the source code should be
  ##  replaced from a and up to (but not including) b.
  ## **
  ##  A string containing text that should be replace the source
  ##  code indicated by the
proc getTranslationUnitSpelling*(cTUnit: CXTranslationUnit): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getTranslationUnitSpelling".}
  ##  Get the original translation unit source file name.
proc createTranslationUnitFromSourceFile*(cIdx: CXIndex; source_filename: cstring;
    num_clang_command_line_args: cint; clang_command_line_args: cstringArray;
    num_unsaved_files: cuint; unsaved_files: ptr[CXUnsavedFile]): CXTranslationUnit {.
    cdecl, dynlib: libclang, importc: "clang_createTranslationUnitFromSourceFile".}
  ##  Return the CXTranslationUnit for a given source file and the provided
  ##  command line arguments one would pass to the compiler.
  ##  Note: The 'source_filename' argument is optional.  If the caller provides a
  ##  NULL pointer, the name of the source file is expected to reside in the
  ##  specified command line arguments.
  ##  Note: When encountered in 'clang_command_line_args', the following options
  ##  are ignored:
  ##    '-c'
  ##    '-emit-ast'
  ##    '-fsyntax-only'
  ##    '-o
  ## <
  ## output file>'  (both '-o' and '
  ## <
  ## output file>' are ignored)
  ## **CIdx**
  ##  The index object with which the translation unit will be
  ##  associated.
  ## **source_filename**
  ##  The name of the source file to load, or NULL if the
  ##  source file is included in
  ## **num_clang_command_line_args**
  ##  The number of command-line arguments in
  ## **clang_command_line_args**
  ##  The command-line arguments that would be
  ##  passed to the
  ##  executable if it were being invoked out-of-process.
  ##  These command-line options will be parsed and will affect how the translation
  ##  unit is parsed. Note that the following options are ignored: '-c',
  ##  '-emit-ast', '-fsyntax-only' (which is the default), and '-o
  ## <
  ## output file>'.
  ## **num_unsaved_files**
  ##  the number of unsaved file entries in
  ## **unsaved_files**
  ##  the files that have not yet been saved to disk
  ##  but may be required for code completion, including the contents of
  ##  those files.  The contents and name of these files (as specified by
  ##  CXUnsavedFile) are copied when necessary, so the client only needs to
  ##  guarantee their validity until the call to this function returns.
proc createTranslationUnit*(cIdx: CXIndex; ast_filename: cstring): CXTranslationUnit {.
    cdecl, dynlib: libclang, importc: "clang_createTranslationUnit".}
  ##  Same as
  ##  but returns
  ##  the
  ##  instead of an error code.  In case of an error this
  ##  routine returns a
  ##  without further detailed
  ##  error codes.
proc createTranslationUnit2*(cIdx: CXIndex; ast_filename: cstring;
                            out_TU: ptr[CXTranslationUnit]): CXErrorCode {.cdecl,
    dynlib: libclang, importc: "clang_createTranslationUnit2".}
  ##  Create a translation unit from an AST file (
  ## **out_TU**
  ##  A non-NULL pointer to store the created
  ## **
  ##  Zero on success, otherwise returns an error code.
type
  CXTranslationUnit_Flags* = enum ##  Flags that control the creation of translation units.
                               ##  The enumerators in this enumeration type are meant to be bitwise
                               ##  ORed together to specify which options should be used when
                               ##  constructing the translation unit.
    tufNone = 0, ##  Used to indicate that no special translation-unit options are
              ##  needed.
    tufDetailedPreprocessingRecord = 1, ##  Used to indicate that the parser should construct a "detailed"
                                     ##  preprocessing record, including all macro definitions and instantiations.
                                     ##  Constructing a detailed preprocessing record requires more memory
                                     ##  and time to parse, since the information contained in the record
                                     ##  is usually not retained. However, it can be useful for
                                     ##  applications that require more detailed information about the
                                     ##  behavior of the preprocessor.
    tufIncomplete = 2, ##  Used to indicate that the translation unit is incomplete.
                    ##  When a translation unit is considered "incomplete", semantic
                    ##  analysis that is typically performed at the end of the
                    ##  translation unit will be suppressed. For example, this suppresses
                    ##  the completion of tentative declarations in C and of
                    ##  instantiation of implicitly-instantiation function templates in
                    ##  C++. This option is typically used when parsing a header with the
                    ##  intent of producing a precompiled header.
    tufPrecompiledPreamble = 4, ##  Used to indicate that the translation unit should be built with an
                             ##  implicit precompiled header for the preamble.
                             ##  An implicit precompiled header is used as an optimization when a
                             ##  particular translation unit is likely to be reparsed many times
                             ##  when the sources aren't changing that often. In this case, an
                             ##  implicit precompiled header will be built containing all of the
                             ##  initial includes at the top of the main file (what we refer to as
                             ##  the "preamble" of the file). In subsequent parses, if the
                             ##  preamble or the files in it have not changed,
                             ##  will re-use the implicit
                             ##  precompiled header to improve parsing performance.
    tufCacheCompletionResults = 8, ##  Used to indicate that the translation unit should cache some
                                ##  code-completion results with each reparse of the source file.
                                ##  Caching of code-completion results is a performance optimization that
                                ##  introduces some overhead to reparsing but improves the performance of
                                ##  code-completion operations.
    tufForSerialization = 16, ##  Used to indicate that the translation unit will be serialized with
                           ##  This option is typically used when parsing a header with the intent of
                           ##  producing a precompiled header.
    tufChainedPCH = 32, ##  DEPRECATED: Enabled chained precompiled preambles in C++.
                        ##  Note: this is a *temporary* option that is available only while
                        ##  we are testing C++ precompiled preamble support. It is deprecated.
    tufSkipFunctionBodies = 64, ##  Used to indicate that function/method bodies should be skipped while
                             ##  parsing.
                             ##  This option can be used to search for declarations/definitions while
                             ##  ignoring the usages.
    tufIncludeBriefCommentsInCodeCompletion = 128, ##  Used to indicate that brief documentation comments should be
                                                ##  included into the set of code completions returned from this translation
                                                ##  unit.
    tufCreatePreambleOnFirstParse = 256, ##  Used to indicate that the precompiled preamble should be created on
                                      ##  the first parse. Otherwise it will be created on the first reparse. This
                                      ##  trades runtime on the first parse (serializing the preamble takes time) for
                                      ##  reduced runtime on the second parse (can now reuse the preamble).
    tufKeepGoing = 512, ##  Do not stop processing when fatal errors are encountered.
                     ##  When fatal errors are encountered while parsing a translation unit,
                     ##  semantic analysis is typically stopped early when compiling code. A common
                     ##  source for fatal errors are unresolvable include files. For the
                     ##  purposes of an IDE, this is undesirable behavior and as much information
                     ##  as possible should be reported. Use this flag to enable this behavior.
    tufSingleFileParse = 1024,  ##  Sets the preprocessor in a mode for parsing a single file only.
    tufLimitSkipFunctionBodiesToPreamble = 2048, ##  Used in combination with CXTranslationUnit_SkipFunctionBodies to
                                              ##  constrain the skipping of function bodies to the preamble.
                                              ##  The function bodies of the main file are not skipped.
    tufIncludeAttributedTypes = 4096, ##  Used to indicate that attributed types should be included in CXType.
    tufVisitImplicitAttributes = 8192, ##  Used to indicate that implicit attributes should be visited.
    tufIgnoreNonErrorsFromIncludedFiles = 16384, ##  Used to indicate that non-errors from included files should be ignored.
                                              ##  If set, clang_getDiagnosticSetFromTU() will not report e.g. warnings from
                                              ##  included files anymore. This speeds up clang_getDiagnosticSetFromTU() for
                                              ##  the case where these warnings are not of interest, as for an IDE for
                                              ##  example, which typically shows only the diagnostics in the main file.
    tufRetainExcludedConditionalBlocks = 32768 ##  Tells the preprocessor not to skip excluded conditional blocks.
proc defaultEditingTranslationUnitOptions*(): cuint {.cdecl, dynlib: libclang,
    importc: "clang_defaultEditingTranslationUnitOptions".}
  ##  Returns the set of flags that is suitable for parsing a translation
  ##  unit that is being edited.
  ##  The set of flags returned provide options for
  ##  to indicate that the translation unit is likely to be reparsed many times,
  ##  either explicitly (via
  ##  or implicitly
  ##  (e.g., by code completion (
  ##  The returned flag
  ##  set contains an unspecified set of optimizations (e.g., the precompiled
  ##  preamble) geared toward improving the performance of these routines. The
  ##  set of optimizations enabled may change from one version to the next.
proc parseTranslationUnit*(cIdx: CXIndex; source_filename: cstring;
                          command_line_args: cstringArray;
                          num_command_line_args: cint;
                          unsaved_files: ptr[CXUnsavedFile];
                          num_unsaved_files: cuint; options: cuint): CXTranslationUnit {.
    cdecl, dynlib: libclang, importc: "clang_parseTranslationUnit".}
  ##  Same as
  ##  but returns
  ##  the
  ##  instead of an error code.  In case of an error this
  ##  routine returns a
  ##  without further detailed
  ##  error codes.
proc parseTranslationUnit2*(cIdx: CXIndex; source_filename: cstring;
                           command_line_args: cstringArray;
                           num_command_line_args: cint;
                           unsaved_files: ptr[CXUnsavedFile];
                           num_unsaved_files: cuint; options: cuint;
                           out_TU: ptr[CXTranslationUnit]): CXErrorCode {.cdecl,
    dynlib: libclang, importc: "clang_parseTranslationUnit2".}
  ##  Parse the given source file and the translation unit corresponding
  ##  to that file.
  ##  This routine is the main entry point for the Clang C API, providing the
  ##  ability to parse a source file into a translation unit that can then be
  ##  queried by other functions in the API. This routine accepts a set of
  ##  command-line arguments so that the compilation can be configured in the same
  ##  way that the compiler is configured on the command line.
  ## **CIdx**
  ##  The index object with which the translation unit will be
  ##  associated.
  ## **source_filename**
  ##  The name of the source file to load, or NULL if the
  ##  source file is included in
  ## **command_line_args**
  ##  The command-line arguments that would be
  ##  passed to the
  ##  executable if it were being invoked out-of-process.
  ##  These command-line options will be parsed and will affect how the translation
  ##  unit is parsed. Note that the following options are ignored: '-c',
  ##  '-emit-ast', '-fsyntax-only' (which is the default), and '-o
  ## <
  ## output file>'.
  ## **num_command_line_args**
  ##  The number of command-line arguments in
  ## **unsaved_files**
  ##  the files that have not yet been saved to disk
  ##  but may be required for parsing, including the contents of
  ##  those files.  The contents and name of these files (as specified by
  ##  CXUnsavedFile) are copied when necessary, so the client only needs to
  ##  guarantee their validity until the call to this function returns.
  ## **num_unsaved_files**
  ##  the number of unsaved file entries in
  ## **options**
  ##  A bitmask of options that affects how the translation unit
  ##  is managed but not its compilation. This should be a bitwise OR of the
  ##  CXTranslationUnit_XXX flags.
  ## **out_TU**
  ##  A non-NULL pointer to store the created
  ##  describing the parsed code and containing any
  ##  diagnostics produced by the compiler.
  ## **
  ##  Zero on success, otherwise returns an error code.
proc parseTranslationUnit2FullArgv*(cIdx: CXIndex; source_filename: cstring;
                                   command_line_args: cstringArray;
                                   num_command_line_args: cint;
                                   unsaved_files: ptr[CXUnsavedFile];
                                   num_unsaved_files: cuint; options: cuint;
                                   out_TU: ptr[CXTranslationUnit]): CXErrorCode {.
    cdecl, dynlib: libclang, importc: "clang_parseTranslationUnit2FullArgv".}
  ##  Same as clang_parseTranslationUnit2 but requires a full command line
  ##  for
  ##  including argv[0]. This is useful if the standard
  ##  library paths are relative to the binary.
type
  CXSaveTranslationUnit_Flags* = enum ##  Flags that control how translation units are saved.
                                   ##  The enumerators in this enumeration type are meant to be bitwise
                                   ##  ORed together to specify which options should be used when
                                   ##  saving the translation unit.
    stufNone = 0                ##  Used to indicate that no special saving options are needed.
proc defaultSaveOptions*(tU: CXTranslationUnit): cuint {.cdecl, dynlib: libclang,
    importc: "clang_defaultSaveOptions".}
  ##  Returns the set of flags that is suitable for saving a translation
  ##  unit.
  ##  The set of flags returned provide options for
  ##  by default. The returned flag
  ##  set contains an unspecified set of options that save translation units with
  ##  the most commonly-requested data.
type
  CXSaveError* = enum           ##  Describes the kind of error that occurred (if any) in a call to
    seNone = 0,                 ##  Indicates that no error occurred while saving a translation unit.
    seUnknown = 1, ##  Indicates that an unknown error occurred while attempting to save
                ##  the file.
                ##  This error typically indicates that file I/O failed when attempting to
                ##  write the file.
    seTranslationErrors = 2, ##  Indicates that errors during translation prevented this attempt
                          ##  to save the translation unit.
                          ##  Errors that prevent the translation unit from being saved can be
                          ##  extracted using
                          ##  and
    seInvalidTU = 3 ##  Indicates that the translation unit to be saved was somehow
                 ##  invalid (e.g., NULL).
proc saveTranslationUnit*(tU: CXTranslationUnit; fileName: cstring; options: cuint): cint {.
    cdecl, dynlib: libclang, importc: "clang_saveTranslationUnit".}
  ##  Saves a translation unit into a serialized representation of
  ##  that translation unit on disk.
  ##  Any translation unit that was parsed without error can be saved
  ##  into a file. The translation unit can then be deserialized into a
  ##  new
  ##  with
  ##  or,
  ##  if it is an incomplete translation unit that corresponds to a
  ##  header, used as a precompiled header when parsing other translation
  ##  units.
  ## **TU**
  ##  The translation unit to save.
  ## **FileName**
  ##  The file to which the translation unit will be saved.
  ## **options**
  ##  A bitmask of options that affects how the translation unit
  ##  is saved. This should be a bitwise OR of the
  ##  CXSaveTranslationUnit_XXX flags.
  ## **
  ##  A value that will match one of the enumerators of the CXSaveError
  ##  enumeration. Zero (CXSaveError_None) indicates that the translation unit was
  ##  saved successfully, while a non-zero value indicates that a problem occurred.
proc suspendTranslationUnit*(argCXTranslationUnit: CXTranslationUnit): cuint {.
    cdecl, dynlib: libclang, importc: "clang_suspendTranslationUnit".}
  ##  Suspend a translation unit in order to free memory associated with it.
  ##  A suspended translation unit uses significantly less memory but on the other
  ##  side does not support any other calls than
  ##  to resume it or
  ##  to dispose it completely.
proc disposeTranslationUnit*(argCXTranslationUnit: CXTranslationUnit): void {.cdecl,
    dynlib: libclang, importc: "clang_disposeTranslationUnit".}
  ##  Destroy the specified CXTranslationUnit object.
type
  CXReparse_Flags* = enum ##  Flags that control the reparsing of translation units.
                       ##  The enumerators in this enumeration type are meant to be bitwise
                       ##  ORed together to specify which options should be used when
                       ##  reparsing the translation unit.
    rfNone = 0                  ##  Used to indicate that no special reparsing options are needed.
proc defaultReparseOptions*(tU: CXTranslationUnit): cuint {.cdecl, dynlib: libclang,
    importc: "clang_defaultReparseOptions".}
  ##  Returns the set of flags that is suitable for reparsing a translation
  ##  unit.
  ##  The set of flags returned provide options for
  ##  by default. The returned flag
  ##  set contains an unspecified set of optimizations geared toward common uses
  ##  of reparsing. The set of optimizations enabled may change from one version
  ##  to the next.
proc reparseTranslationUnit*(tU: CXTranslationUnit; num_unsaved_files: cuint;
                            unsaved_files: ptr[CXUnsavedFile]; options: cuint): cint {.
    cdecl, dynlib: libclang, importc: "clang_reparseTranslationUnit".}
  ##  Reparse the source files that produced this translation unit.
  ##  This routine can be used to re-parse the source files that originally
  ##  created the given translation unit, for example because those source files
  ##  have changed (either on disk or as passed via
  ##  The
  ##  source code will be reparsed with the same command-line options as it
  ##  was originally parsed.
  ##  Reparsing a translation unit invalidates all cursors and source locations
  ##  that refer into that translation unit. This makes reparsing a translation
  ##  unit semantically equivalent to destroying the translation unit and then
  ##  creating a new translation unit with the same command-line arguments.
  ##  However, it may be more efficient to reparse a translation
  ##  unit using this routine.
  ## **TU**
  ##  The translation unit whose contents will be re-parsed. The
  ##  translation unit must originally have been built with
  ## **num_unsaved_files**
  ##  The number of unsaved file entries in
  ## **unsaved_files**
  ##  The files that have not yet been saved to disk
  ##  but may be required for parsing, including the contents of
  ##  those files.  The contents and name of these files (as specified by
  ##  CXUnsavedFile) are copied when necessary, so the client only needs to
  ##  guarantee their validity until the call to this function returns.
  ## **options**
  ##  A bitset of options composed of the flags in CXReparse_Flags.
  ##  The function
  ##  produces a default set of
  ##  options recommended for most uses, based on the translation unit.
  ## **
  ##  0 if the sources could be reparsed.  A non-zero error code will be
  ##  returned if reparsing was impossible, such that the translation unit is
  ##  invalid. In such cases, the only valid call for
  ##  is
  ##   The error codes returned by this
  ##  routine are described by the
  ##  enum.
type
  CXTUResourceUsageKind* = enum ##  Categorizes how memory is being used by a translation unit.
                             ## **SKipped enum values**
                             ## - CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN = CXTUResourceUsage_AST
                             ## - CXTUResourceUsage_MEMORY_IN_BYTES_END = CXTUResourceUsage_Preprocessor_HeaderSearch
                             ## - CXTUResourceUsage_First = CXTUResourceUsage_AST
                             ## - CXTUResourceUsage_Last = CXTUResourceUsage_Preprocessor_HeaderSearch
    turukAST = 1, turukIdentifiers = 2, turukSelectors = 3,
    turukGlobalCompletionResults = 4, turukSourceManagerContentCache = 5,
    turukAST_SideTables = 6, turukSourceManager_Membuffer_Malloc = 7,
    turukSourceManager_Membuffer_MMap = 8,
    turukExternalASTSource_Membuffer_Malloc = 9,
    turukExternalASTSource_Membuffer_MMap = 10, turukPreprocessor = 11,
    turukPreprocessingRecord = 12, turukSourceManager_DataStructures = 13,
    turukPreprocessor_HeaderSearch = 14
proc getTUResourceUsageName*(kind: CXTUResourceUsageKind): cstring {.cdecl,
    dynlib: libclang, importc: "clang_getTUResourceUsageName".}
  ##  Returns the human-readable null-terminated C string that represents
  ##   the name of the memory category.  This string should never be freed.
type
  CXTUResourceUsageEntry* {.pure, bycopy.} = object
    kind*: CXTUResourceUsageKind
    amount*: culong

type
  CXTUResourceUsage* {.pure, bycopy.} = object
    data*: pointer
    numEntries*: cuint
    entries*: ptr[CXTUResourceUsageEntry]

proc getCXTUResourceUsage*(tU: CXTranslationUnit): CXTUResourceUsage {.cdecl,
    dynlib: libclang, importc: "clang_getCXTUResourceUsage".}
  ##  Return the memory usage of a translation unit.  This object
  ##   should be released with clang_disposeCXTUResourceUsage().
proc disposeCXTUResourceUsage*(usage: CXTUResourceUsage): void {.cdecl,
    dynlib: libclang, importc: "clang_disposeCXTUResourceUsage".}
proc getTranslationUnitTargetInfo*(cTUnit: CXTranslationUnit): CXTargetInfo {.cdecl,
    dynlib: libclang, importc: "clang_getTranslationUnitTargetInfo".}
  ##  Get target information for this translation unit.
  ##  The CXTargetInfo object cannot outlive the CXTranslationUnit object.
proc dispose*(info: CXTargetInfo): void {.cdecl, dynlib: libclang,
                                      importc: "clang_TargetInfo_dispose".}
  ##  Destroy the CXTargetInfo object.
proc getTriple*(info: CXTargetInfo): CXString {.cdecl, dynlib: libclang,
    importc: "clang_TargetInfo_getTriple".}
  ##  Get the normalized target triple as a string.
  ##  Returns the empty string in case of any error.
proc getPointerWidth*(info: CXTargetInfo): cint {.cdecl, dynlib: libclang,
    importc: "clang_TargetInfo_getPointerWidth".}
  ##  Get the pointer width of the target in bits.
  ##  Returns -1 in case of error.
type
  CXCursorKind* = enum ##  Describes the kind of entity that a cursor refers to.
    ckUnexposedDecl = 1, ##  A declaration whose specific kind is not exposed via this
                      ##  interface.
                      ##  Unexposed declarations have the same operations as any other kind
                      ##  of declaration; one can extract their location information,
                      ##  spelling, find their definitions, etc. However, the specific kind
                      ##  of the declaration is not reported.
    ckStructDecl = 2,           ##  A C or C++ struct.
    ckUnionDecl = 3,            ##  A C or C++ union.
    ckClassDecl = 4,            ##  A C++ class.
    ckEnumDecl = 5,             ##  An enumeration.
    ckFieldDecl = 6, ##  A field (in C) or non-static data member (in C++) in a
                  ##  struct, union, or C++ class.
    ckEnumConstantDecl = 7,     ##  An enumerator constant.
    ckFunctionDecl = 8,         ##  A function.
    ckVarDecl = 9,              ##  A variable.
    ckParmDecl = 10,            ##  A function or method parameter.
    ckObjCInterfaceDecl = 11,   ##  An Objective-C
                           ## @
                           ## interface.
    ckObjCCategoryDecl = 12,    ##  An Objective-C
                          ## @
                          ## interface for a category.
    ckObjCProtocolDecl = 13,    ##  An Objective-C
                          ## @
                          ## protocol declaration.
    ckObjCPropertyDecl = 14,    ##  An Objective-C
                          ## @
                          ## property declaration.
    ckObjCIvarDecl = 15,        ##  An Objective-C instance variable.
    ckObjCInstanceMethodDecl = 16, ##  An Objective-C instance method.
    ckObjCClassMethodDecl = 17, ##  An Objective-C class method.
    ckObjCImplementationDecl = 18, ##  An Objective-C
                                ## @
                                ## implementation.
    ckObjCCategoryImplDecl = 19, ##  An Objective-C
                              ## @
                              ## implementation for a category.
    ckTypedefDecl = 20,         ##  A typedef.
    ckMethod = 21,           ##  A C++ class method.
    ckNamespace = 22,           ##  A C++ namespace.
    ckLinkageSpec = 23,         ##  A linkage specification, e.g. 'extern "C"'.
    ckConstructor = 24,         ##  A C++ constructor.
    ckDestructor = 25,          ##  A C++ destructor.
    ckConversionFunction = 26,  ##  A C++ conversion function.
    ckTemplateTypeParameter = 27, ##  A C++ template type parameter.
    ckNonTypeTemplateParameter = 28, ##  A C++ non-type template parameter.
    ckTemplateTemplateParameter = 29, ##  A C++ template template parameter.
    ckFunctionTemplate = 30,    ##  A C++ function template.
    ckClassTemplate = 31,       ##  A C++ class template.
    ckClassTemplatePartialSpecialization = 32, ##  A C++ class template partial specialization.
    ckNamespaceAlias = 33,      ##  A C++ namespace alias declaration.
    ckUsingDirective = 34,      ##  A C++ using directive.
    ckUsingDeclaration = 35,    ##  A C++ using declaration.
    ckTypeAliasDecl = 36,       ##  A C++ alias declaration
    ckObjCSynthesizeDecl = 37,  ##  An Objective-C
                            ## @
                            ## synthesize definition.
    ckObjCDynamicDecl = 38,     ##  An Objective-C
                         ## @
                         ## dynamic definition.
    ckAccessSpecifier = 39,  ##  An access specifier.
    ckFirstRef = 40, ckObjCProtocolRef = 41, ckObjCClassRef = 42, ckTypeRef = 43, ##  A reference to a type declaration.
                                                                      ##  A type reference occurs anywhere where a type is named but not
                                                                      ##  declared. For example, given:
                                                                      ## Error: cannot render: rnCodeBlock
                                                                      ##  The typedef is a declaration of size_type (CXCursor_TypedefDecl),
                                                                      ##  while the type of the variable "size" is referenced. The cursor
                                                                      ##  referenced by the type of size is the typedef for size_type.
    ckBaseSpecifier = 44, ckTemplateRef = 45, ##  A reference to a class template, function template, template
                                           ##  template parameter, or class template partial specialization.
    ckNamespaceRef = 46,        ##  A reference to a namespace or namespace alias.
    ckMemberRef = 47, ##  A reference to a member of a struct, union, or class that occurs in
                   ##  some non-expression context, e.g., a designated initializer.
    ckLabelRef = 48, ##  A reference to a labeled statement.
                  ##  This cursor kind is used to describe the jump to "start_over" in the
                  ##  goto statement in the following example:
                  ## Error: cannot render: rnCodeBlock
                  ##  A label reference cursor refers to a label statement.
    ckOverloadedDeclRef = 49, ##  A reference to a set of overloaded functions or function templates
                           ##  that has not yet been resolved to a specific function or function template.
                           ##  An overloaded declaration reference cursor occurs in C++ templates where
                           ##  a dependent name refers to a function. For example:
                           ## Error: cannot render: rnCodeBlock
                           ##  Here, the identifier "swap" is associated with an overloaded declaration
                           ##  reference. In the template definition, "swap" refers to either of the two
                           ##  "swap" functions declared above, so both results will be available. At
                           ##  instantiation time, "swap" may also refer to other functions found via
                           ##  argument-dependent lookup (e.g., the "swap" function at the end of the
                           ##  example).
                           ##  The functions
                           ##  and
                           ##  can be used to retrieve the definitions
                           ##  referenced by this cursor.
    ckVariableRef = 50, ##  A reference to a variable that occurs in some non-expression
                     ##  context, e.g., a C++ lambda capture list.
    ckFirstInvalid = 70, ckNoDeclFound = 71, ckNotImplemented = 72, ckInvalidCode = 73,
    ckFirstExpr = 100, ckDeclRefExpr = 101, ##  An expression that refers to some value declaration, such
                                      ##  as a function, variable, or enumerator.
    ckMemberRefExpr = 102, ##  An expression that refers to a member of a struct, union,
                        ##  class, Objective-C class, etc.
    ckCallExpr = 103,           ##  An expression that calls a function.
    ckObjCMessageExpr = 104, ##  An expression that sends a message to an Objective-C
                          ##    object or class.
    ckBlockExpr = 105,          ##  An expression that represents a block literal.
    ckIntegerLiteral = 106,     ##  An integer literal.
    ckFloatingLiteral = 107,    ##  A floating point number literal.
    ckImaginaryLiteral = 108,   ##  An imaginary number literal.
    ckStringLiteral = 109,      ##  A string literal.
    ckCharacterLiteral = 110,   ##  A character literal.
    ckParenExpr = 111,          ##  A parenthesized expression, e.g. "(1)".
                    ##  This AST node is only formed if full location information is requested.
    ckUnaryOperator = 112, ##  This represents the unary-expression's (except sizeof and
                        ##  alignof).
    ckArraySubscriptExpr = 113, ##  [C99 6.5.2.1] Array Subscripting.
    ckBinaryOperator = 114, ##  A builtin binary operation expression such as "x + y" or
                         ##  "x
                         ## <
                         ## = y".
    ckCompoundAssignOperator = 115, ##  Compound assignment such as "+=".
    ckConditionalOperator = 116, ##  The ?: ternary operator.
    ckCStyleCastExpr = 117, ##  An explicit cast in C (C99 6.5.4) or a C-style cast in C++
                         ##  (C++ [expr.cast]), which uses the syntax (Type)expr.
                         ##  For example: (int)f.
    ckCompoundLiteralExpr = 118, ##  [C99 6.5.2.5]
    ckInitListExpr = 119,       ##  Describes an C or C++ initializer list.
    ckAddrLabelExpr = 120,      ##  The GNU address of label extension, representing
                        ## &
                        ## &label
                        ## .
    ckStmtExpr = 121,           ##  This is the GNU Statement Expression extension: ({int X=4; X;})
    ckGenericSelectionExpr = 122, ##  Represents a C11 generic selection.
    ckGNUNullExpr = 123, ##  Implements the GNU __null extension, which is a name for a null
                      ##  pointer constant that has integral type (e.g., int or long) and is the same
                      ##  size and alignment as a pointer.
                      ##  The __null extension is typically only used by system headers, which define
                      ##  NULL as __null in C++ rather than using 0 (which is an integer that may not
                      ##  match the size of a pointer).
    ckStaticCastExpr = 124,  ##  C++'s static_cast
                            ## <
                            ## > expression.
    ckDynamicCastExpr = 125, ##  C++'s dynamic_cast
                             ## <
                             ## > expression.
    ckReinterpretCastExpr = 126, ##  C++'s reinterpret_cast
                                 ## <
                                 ## > expression.
    ckConstCastExpr = 127,   ##  C++'s const_cast
                           ## <
                           ## > expression.
    ckFunctionalCastExpr = 128, ##  Represents an explicit C++ type conversion that uses "functional"
                                ##  notion (C++ [expr.type.conv]).
                                ##  Example:
                                ## Error: cannot render: rnCodeBlock
    ckTypeidExpr = 129,      ##  A C++ typeid expression (C++ [expr.typeid]).
    ckBoolLiteralExpr = 130, ##  [C++ 2.13.5] C++ Boolean Literal.
    ckNullPtrLiteralExpr = 131, ##  [C++0x 2.14.7] C++ Pointer Literal.
    ckThisExpr = 132,        ##  Represents the "this" expression in C++
    ckThrowExpr = 133, ##  [C++ 15] C++ Throw Expression.
                       ##  This handles 'throw' and 'throw' assignment-expression. When
                       ##  assignment-expression isn't present, Op will be null.
    ckNewExpr = 134, ##  A new expression for memory allocation and constructor calls, e.g:
                     ##  "new CXXNewExpr(foo)".
    ckDeleteExpr = 135, ##  A delete expression for memory deallocation and destructor calls,
                        ##  e.g. "delete[] pArray".
    ckUnaryExpr = 136,          ##  A unary expression. (noexcept, sizeof, or other traits)
    ckObjCStringLiteral = 137,  ##  An Objective-C string literal i.e.
                            ## "
                            ## foo".
    ckObjCEncodeExpr = 138,     ##  An Objective-C
                         ## @
                         ## encode expression.
    ckObjCSelectorExpr = 139,   ##  An Objective-C
                           ## @
                           ## selector expression.
    ckObjCProtocolExpr = 140,   ##  An Objective-C
                           ## @
                           ## protocol expression.
    ckObjCBridgedCastExpr = 141, ##  An Objective-C "bridged" cast expression, which casts between
                              ##  Objective-C pointers and C pointers, transferring ownership in the process.
                              ## Error: cannot render: rnCodeBlock
    ckPackExpansionExpr = 142, ##  Represents a C++0x pack expansion that produces a sequence of
                            ##  expressions.
                            ##  A pack expansion expression contains a pattern (which itself is an
                            ##  expression) followed by an ellipsis. For example:
                            ## Error: cannot render: rnCodeBlock
    ckSizeOfPackExpr = 143, ##  Represents an expression that computes the length of a parameter
                         ##  pack.
                         ## Error: cannot render: rnCodeBlock
    ckLambdaExpr = 144, ckObjCBoolLiteralExpr = 145, ##  Objective-c Boolean Literal.
    ckObjCSelfExpr = 146,       ##  Represents the "self" expression in an Objective-C method.
    ckOMPArraySectionExpr = 147, ##  OpenMP 4.0 [2.4, Array Section].
    ckObjCAvailabilityCheckExpr = 148, ##  Represents an
                                    ## (...) check.
    ckFixedPointLiteral = 149,  ##  Fixed point literal
    ckFirstStmt = 200, ckLabelStmt = 201, ##  A labelled statement in a function.
                                    ##  This cursor kind is used to describe the "start_over:" label statement in
                                    ##  the following example:
                                    ## Error: cannot render: rnCodeBlock
    ckCompoundStmt = 202, ##  A group of statements like { stmt stmt }.
                       ##  This cursor kind is used to describe compound statements, e.g. function
                       ##  bodies.
    ckCaseStmt = 203,           ##  A case statement.
    ckDefaultStmt = 204,        ##  A default statement.
    ckIfStmt = 205,             ##  An if statement
    ckSwitchStmt = 206,         ##  A switch statement.
    ckWhileStmt = 207,          ##  A while statement.
    ckDoStmt = 208,             ##  A do statement.
    ckForStmt = 209,            ##  A for statement.
    ckGotoStmt = 210,           ##  A goto statement.
    ckIndirectGotoStmt = 211,   ##  An indirect goto statement.
    ckContinueStmt = 212,       ##  A continue statement.
    ckBreakStmt = 213,          ##  A break statement.
    ckReturnStmt = 214,         ##  A return statement.
    ckGCCAsmStmt = 215,         ##  A GCC inline assembly statement extension.
    ckObjCAtTryStmt = 216,      ##  Objective-C's overall
                        ## @
                        ## try-
                        ## @
                        ## catch-
                        ## @
                        ## finally statement.
    ckObjCAtCatchStmt = 217,    ##  Objective-C's
                          ## @
                          ## catch statement.
    ckObjCAtFinallyStmt = 218,  ##  Objective-C's
                            ## @
                            ## finally statement.
    ckObjCAtThrowStmt = 219,    ##  Objective-C's
                          ## @
                          ## throw statement.
    ckObjCAtSynchronizedStmt = 220, ##  Objective-C's
                                 ## @
                                 ## synchronized statement.
    ckObjCAutoreleasePoolStmt = 221, ##  Objective-C's autorelease pool statement.
    ckObjCForCollectionStmt = 222, ##  Objective-C's collection statement.
    ckCatchStmt = 223,       ##  C++'s catch statement.
    ckTryStmt = 224,         ##  C++'s try statement.
    ckForRangeStmt = 225,    ##  C++'s for (* : *) statement.
    ckSEHTryStmt = 226,         ##  Windows Structured Exception Handling's try statement.
    ckSEHExceptStmt = 227,      ##  Windows Structured Exception Handling's except statement.
    ckSEHFinallyStmt = 228,     ##  Windows Structured Exception Handling's finally statement.
    ckMSAsmStmt = 229,          ##  A MS inline assembly statement extension.
    ckNullStmt = 230,           ##  The null statement ";": C99 6.8.3p3.
                   ##  This cursor kind is used to describe the null statement.
    ckDeclStmt = 231, ##  Adaptor class for mixing declarations with statements and
                   ##  expressions.
    ckOMPParallelDirective = 232, ##  OpenMP parallel directive.
    ckOMPSimdDirective = 233,   ##  OpenMP SIMD directive.
    ckOMPForDirective = 234,    ##  OpenMP for directive.
    ckOMPSectionsDirective = 235, ##  OpenMP sections directive.
    ckOMPSectionDirective = 236, ##  OpenMP section directive.
    ckOMPSingleDirective = 237, ##  OpenMP single directive.
    ckOMPParallelForDirective = 238, ##  OpenMP parallel for directive.
    ckOMPParallelSectionsDirective = 239, ##  OpenMP parallel sections directive.
    ckOMPTaskDirective = 240,   ##  OpenMP task directive.
    ckOMPMasterDirective = 241, ##  OpenMP master directive.
    ckOMPCriticalDirective = 242, ##  OpenMP critical directive.
    ckOMPTaskyieldDirective = 243, ##  OpenMP taskyield directive.
    ckOMPBarrierDirective = 244, ##  OpenMP barrier directive.
    ckOMPTaskwaitDirective = 245, ##  OpenMP taskwait directive.
    ckOMPFlushDirective = 246,  ##  OpenMP flush directive.
    ckSEHLeaveStmt = 247,       ##  Windows Structured Exception Handling's leave statement.
    ckOMPOrderedDirective = 248, ##  OpenMP ordered directive.
    ckOMPAtomicDirective = 249, ##  OpenMP atomic directive.
    ckOMPForSimdDirective = 250, ##  OpenMP for SIMD directive.
    ckOMPParallelForSimdDirective = 251, ##  OpenMP parallel for SIMD directive.
    ckOMPTargetDirective = 252, ##  OpenMP target directive.
    ckOMPTeamsDirective = 253,  ##  OpenMP teams directive.
    ckOMPTaskgroupDirective = 254, ##  OpenMP taskgroup directive.
    ckOMPCancellationPointDirective = 255, ##  OpenMP cancellation point directive.
    ckOMPCancelDirective = 256, ##  OpenMP cancel directive.
    ckOMPTargetDataDirective = 257, ##  OpenMP target data directive.
    ckOMPTaskLoopDirective = 258, ##  OpenMP taskloop directive.
    ckOMPTaskLoopSimdDirective = 259, ##  OpenMP taskloop simd directive.
    ckOMPDistributeDirective = 260, ##  OpenMP distribute directive.
    ckOMPTargetEnterDataDirective = 261, ##  OpenMP target enter data directive.
    ckOMPTargetExitDataDirective = 262, ##  OpenMP target exit data directive.
    ckOMPTargetParallelDirective = 263, ##  OpenMP target parallel directive.
    ckOMPTargetParallelForDirective = 264, ##  OpenMP target parallel for directive.
    ckOMPTargetUpdateDirective = 265, ##  OpenMP target update directive.
    ckOMPDistributeParallelForDirective = 266, ##  OpenMP distribute parallel for directive.
    ckOMPDistributeParallelForSimdDirective = 267, ##  OpenMP distribute parallel for simd directive.
    ckOMPDistributeSimdDirective = 268, ##  OpenMP distribute simd directive.
    ckOMPTargetParallelForSimdDirective = 269, ##  OpenMP target parallel for simd directive.
    ckOMPTargetSimdDirective = 270, ##  OpenMP target simd directive.
    ckOMPTeamsDistributeDirective = 271, ##  OpenMP teams distribute directive.
    ckOMPTeamsDistributeSimdDirective = 272, ##  OpenMP teams distribute simd directive.
    ckOMPTeamsDistributeParallelForSimdDirective = 273, ##  OpenMP teams distribute parallel for simd directive.
    ckOMPTeamsDistributeParallelForDirective = 274, ##  OpenMP teams distribute parallel for directive.
    ckOMPTargetTeamsDirective = 275, ##  OpenMP target teams directive.
    ckOMPTargetTeamsDistributeDirective = 276, ##  OpenMP target teams distribute directive.
    ckOMPTargetTeamsDistributeParallelForDirective = 277, ##  OpenMP target teams distribute parallel for directive.
    ckOMPTargetTeamsDistributeParallelForSimdDirective = 278, ##  OpenMP target teams distribute parallel for simd directive.
    ckOMPTargetTeamsDistributeSimdDirective = 279, ##  OpenMP target teams distribute simd directive.
    ckBuiltinBitCastExpr = 280, ##  C++2a std::bit_cast expression.
    ckOMPMasterTaskLoopDirective = 281, ##  OpenMP master taskloop directive.
    ckOMPParallelMasterTaskLoopDirective = 282, ##  OpenMP parallel master taskloop directive.
    ckOMPMasterTaskLoopSimdDirective = 283, ##  OpenMP master taskloop simd directive.
    ckOMPParallelMasterTaskLoopSimdDirective = 284, ##  OpenMP parallel master taskloop simd directive.
    ckOMPParallelMasterDirective = 285, ##  OpenMP parallel master directive.
    ckTranslationUnit = 300, ##  Cursor that represents the translation unit itself.
                          ##  The translation unit cursor exists primarily to act as the root
                          ##  cursor for traversing the contents of a translation unit.
    ckFirstAttr = 400, ckIBActionAttr = 401, ckIBOutletAttr = 402,
    ckIBOutletCollectionAttr = 403, ckFinalAttr = 404, ckOverrideAttr = 405,
    ckAnnotateAttr = 406, ckAsmLabelAttr = 407, ckPackedAttr = 408, ckPureAttr = 409,
    ckConstAttr = 410, ckNoDuplicateAttr = 411, ckCUDAConstantAttr = 412,
    ckCUDADeviceAttr = 413, ckCUDAGlobalAttr = 414, ckCUDAHostAttr = 415,
    ckCUDASharedAttr = 416, ckVisibilityAttr = 417, ckDLLExport = 418, ckDLLImport = 419,
    ckNSReturnsRetained = 420, ckNSReturnsNotRetained = 421,
    ckNSReturnsAutoreleased = 422, ckNSConsumesSelf = 423, ckNSConsumed = 424,
    ckObjCException = 425, ckObjCNSObject = 426, ckObjCIndependentClass = 427,
    ckObjCPreciseLifetime = 428, ckObjCReturnsInnerPointer = 429,
    ckObjCRequiresSuper = 430, ckObjCRootClass = 431,
    ckObjCSubclassingRestricted = 432, ckObjCExplicitProtocolImpl = 433,
    ckObjCDesignatedInitializer = 434, ckObjCRuntimeVisible = 435,
    ckObjCBoxable = 436, ckFlagEnum = 437, ckConvergentAttr = 438,
    ckWarnUnusedAttr = 439, ckWarnUnusedResultAttr = 440, ckAlignedAttr = 441,
    ckPreprocessingDirective = 500, ckMacroDefinition = 501, ckMacroExpansion = 502,
    ckInclusionDirective = 503, ckModuleImportDecl = 600, ##  A module import declaration.
    ckTypeAliasTemplateDecl = 601, ckStaticAssert = 602, ##  A static_assert or _Static_assert node
    ckFriendDecl = 603,         ##  a friend declaration.
    ckOverloadCandidate = 700   ##  A code completion overload candidate.

const
  ckFirstDecl*          = ckUnexposedDecl
  ckLastDecl*           = ckAccessSpecifier
  ckObjCSuperClassRef*  = ckFirstRef
  ckLastRef*            = ckVariableRef
  ckInvalidFile*        = ckFirstInvalid
  ckLastInvalid*        = ckInvalidCode
  ckUnexposedExpr*      = ckFirstExpr
  ckLastExpr*           = ckFixedPointLiteral
  ckUnexposedStmt*      = ckFirstStmt
  ckAsmStmt*            = ckGCCAsmStmt
  ckLastStmt*           = ckOMPParallelMasterDirective
  ckUnexposedAttr*      = ckFirstAttr
  ckLastAttr*           = ckAlignedAttr
  ckMacroInstantiation* = ckMacroExpansion
  ckFirstPreprocessing* = ckPreprocessingDirective
  ckLastPreprocessing*  = ckInclusionDirective
  ckFirstExtraDecl*     = ckModuleImportDecl
  ckLastExtraDecl*      = ckFriendDecl


type
  CXCursor* {.pure, bycopy.} = object
    kind*: CXCursorKind
    xdata*: cint
    data*: array[3, pointer]

proc getNullCursor*(): CXCursor {.cdecl, dynlib: libclang,
                               importc: "clang_getNullCursor".}
  ##  Retrieve the NULL cursor, which represents no entity.
proc getTranslationUnitCursor*(argCXTranslationUnit: CXTranslationUnit): CXCursor {.
    cdecl, dynlib: libclang, importc: "clang_getTranslationUnitCursor".}
  ##  Retrieve the cursor that represents the given translation unit.
  ##  The translation unit cursor can be used to start traversing the
  ##  various declarations within the given translation unit.
proc equalCursors*(argCXCursor: CXCursor; argCXCursor1: CXCursor): cuint {.cdecl,
    dynlib: libclang, importc: "clang_equalCursors".}
  ##  Determine whether two cursors are equivalent.
proc isNull*(cursor: CXCursor): cint {.cdecl, dynlib: libclang,
                                   importc: "clang_Cursor_isNull".}
  ##  Returns non-zero if
  ##  is null.
proc hashCursor*(argCXCursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_hashCursor".}
  ##  Compute a hash value for the given cursor.
proc getCursorKind*(argCXCursor: CXCursor): CXCursorKind {.cdecl, dynlib: libclang,
    importc: "clang_getCursorKind".}
  ##  Retrieve the kind of the given cursor.
proc isDeclaration*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isDeclaration".}
  ##  Determine whether the given cursor kind represents a declaration.
proc isInvalidDeclaration*(argCXCursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isInvalidDeclaration".}
  ##  Determine whether the given declaration is invalid.
  ##  A declaration is invalid if it could not be parsed successfully.
  ## **
  ##  non-zero if the cursor represents a declaration and it is
  ##  invalid, otherwise NULL.
proc isReference*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isReference".}
  ##  Determine whether the given cursor kind represents a simple
  ##  reference.
  ##  Note that other kinds of cursors (such as expressions) can also refer to
  ##  other cursors. Use clang_getCursorReferenced() to determine whether a
  ##  particular cursor refers to another entity.
proc isExpression*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isExpression".}
  ##  Determine whether the given cursor kind represents an expression.
proc isStatement*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isStatement".}
  ##  Determine whether the given cursor kind represents a statement.
proc isAttribute*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isAttribute".}
  ##  Determine whether the given cursor kind represents an attribute.
proc hasAttrs*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
                                 importc: "clang_Cursor_hasAttrs".}
  ##  Determine whether the given cursor has any attributes.
proc isInvalid*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isInvalid".}
  ##  Determine whether the given cursor kind represents an invalid
  ##  cursor.
proc isTranslationUnit*(argCXCursorKind: CXCursorKind): cuint {.cdecl,
    dynlib: libclang, importc: "clang_isTranslationUnit".}
  ##  Determine whether the given cursor kind represents a translation
  ##  unit.
proc isPreprocessing*(argCXCursorKind: CXCursorKind): cuint {.cdecl,
    dynlib: libclang, importc: "clang_isPreprocessing".}
  ## *
  ##  Determine whether the given cursor represents a preprocessing
  ##  element, such as a preprocessor directive or macro instantiation.
proc isUnexposed*(argCXCursorKind: CXCursorKind): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isUnexposed".}
  ## *
  ##  Determine whether the given cursor represents a currently
  ##   unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
type
  CXLinkageKind* = enum         ##  Describe the linkage of the entity referred to by a cursor.
    lkInvalid, ##  This value indicates that no linkage information is available
              ##  for a provided CXCursor.
    lkNoLinkage, ##  This is the linkage for variables, parameters, and so on that
                ##   have automatic storage.  This covers normal (non-extern) local variables.
    lkInternal,               ##  This is the linkage for static variables and static functions.
    lkUniqueExternal, ##  This is the linkage for entities with external linkage that live
                     ##  in C++ anonymous namespaces.
    lkExternal                ##  This is the linkage for entities with true, external linkage.
proc getCursorLinkage*(cursor: CXCursor): CXLinkageKind {.cdecl, dynlib: libclang,
    importc: "clang_getCursorLinkage".}
  ##  Determine the linkage of the entity referred to by a given cursor.
type
  CXVisibilityKind* = enum
    vkInvalid, ##  This value indicates that no visibility information is available
              ##  for a provided CXCursor.
    vkHidden,                 ##  Symbol not seen by the linker.
    vkProtected,              ##  Symbol seen by the linker but resolves to a symbol inside this object.
    vkDefault                 ##  Symbol seen by the linker and acts like a normal symbol.
proc getCursorVisibility*(cursor: CXCursor): CXVisibilityKind {.cdecl,
    dynlib: libclang, importc: "clang_getCursorVisibility".}
  ##  Describe the visibility of the entity referred to by a cursor.
  ##  This returns the default visibility if not explicitly specified by
  ##  a visibility attribute. The default visibility may be changed by
  ##  commandline arguments.
  ## **cursor**
  ##  The cursor to query.
  ## **
  ##  The visibility of the cursor.
proc getCursorAvailability*(cursor: CXCursor): CXAvailabilityKind {.cdecl,
    dynlib: libclang, importc: "clang_getCursorAvailability".}
  ##  Determine the availability of the entity that this cursor refers to,
  ##  taking the current target platform into account.
  ## **cursor**
  ##  The cursor to query.
  ## **
  ##  The availability of the cursor.
type
  CXPlatformAvailability* {.pure, bycopy.} = object
    platform*: CXString
    introduced*: CXVersion
    deprecated*: CXVersion
    obsoleted*: CXVersion
    unavailable*: cint
    message*: CXString

proc getCursorPlatformAvailability*(cursor: CXCursor; always_deprecated: ptr[cint];
                                   deprecated_message: ptr[CXString];
                                   always_unavailable: ptr[cint];
                                   unavailable_message: ptr[CXString];
                                   availability: ptr[CXPlatformAvailability];
                                   availability_size: cint): cint {.cdecl,
    dynlib: libclang, importc: "clang_getCursorPlatformAvailability".}
  ##  Determine the availability of the entity that this cursor refers to
  ##  on any platforms for which availability information is known.
  ## **cursor**
  ##  The cursor to query.
  ## **always_deprecated**
  ##  If non-NULL, will be set to indicate whether the
  ##  entity is deprecated on all platforms.
  ## **deprecated_message**
  ##  If non-NULL, will be set to the message text
  ##  provided along with the unconditional deprecation of this entity. The client
  ##  is responsible for deallocating this string.
  ## **always_unavailable**
  ##  If non-NULL, will be set to indicate whether the
  ##  entity is unavailable on all platforms.
  ## **unavailable_message**
  ##  If non-NULL, will be set to the message text
  ##  provided along with the unconditional unavailability of this entity. The
  ##  client is responsible for deallocating this string.
  ## **availability**
  ##  If non-NULL, an array of CXPlatformAvailability instances
  ##  that will be populated with platform availability information, up to either
  ##  the number of platforms for which availability information is available (as
  ##  returned by this function) or
  ##  whichever is smaller.
  ## **availability_size**
  ##  The number of elements available in the
  ##  array.
  ## **
  ##  The number of platforms (N) for which availability information is
  ##  available (which is unrelated to
  ##  Note that the client is responsible for calling
  ##  to free each of the
  ##  platform-availability structures returned. There are
  ##  availability_size) such structures.
proc disposeCXPlatformAvailability*(availability: ptr[CXPlatformAvailability]): void {.
    cdecl, dynlib: libclang, importc: "clang_disposeCXPlatformAvailability".}
  ##  Free the memory associated with a
  ##  structure.
type
  CXLanguageKind* = enum        ##  Describe the "language" of the entity referred to by a cursor.
    lakInvalid = 0, lakC, lakObjC, lakCPlusPlus
proc getCursorLanguage*(cursor: CXCursor): CXLanguageKind {.cdecl, dynlib: libclang,
    importc: "clang_getCursorLanguage".}
  ##  Determine the "language" of the entity referred to by a given cursor.
type
  CXTLSKind* = enum ##  Describe the "thread-local storage (TLS) kind" of the declaration
                 ##  referred to by a cursor.
    tlskNone = 0, tlskDynamic, tlskStatic
proc getCursorTLSKind*(cursor: CXCursor): CXTLSKind {.cdecl, dynlib: libclang,
    importc: "clang_getCursorTLSKind".}
  ##  Determine the "thread-local storage (TLS) kind" of the declaration
  ##  referred to by a cursor.
proc getTranslationUnit*(argCXCursor: CXCursor): CXTranslationUnit {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getTranslationUnit".}
  ##  Returns the translation unit that a cursor originated from.
type
  CXCursorSetImpl* {.pure, bycopy.} = object

type
  CXCursorSet* = distinct ptr[CXCursorSetImpl]
proc createCXCursorSet*(): CXCursorSet {.cdecl, dynlib: libclang,
                                      importc: "clang_createCXCursorSet".}
  ##  Creates an empty CXCursorSet.
proc disposeCXCursorSet*(cset: CXCursorSet): void {.cdecl, dynlib: libclang,
    importc: "clang_disposeCXCursorSet".}
  ##  Disposes a CXCursorSet and releases its associated memory.
proc contains*(cset: CXCursorSet; cursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXCursorSet_contains".}
  ##  Queries a CXCursorSet to see if it contains a specific CXCursor.
  ## **
  ##  non-zero if the set contains the specified cursor.
proc insert*(cset: CXCursorSet; cursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXCursorSet_insert".}
  ##  Inserts a CXCursor into a CXCursorSet.
  ## **
  ##  zero if the CXCursor was already in the set, and non-zero otherwise.
proc getCursorSemanticParent*(cursor: CXCursor): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getCursorSemanticParent".}
  ##  Determine the semantic parent of the given cursor.
  ##  The semantic parent of a cursor is the cursor that semantically contains
  ##  the given
  ##  For many declarations, the lexical and semantic parents
  ##  are equivalent (the lexical parent is returned by
  ##  They diverge when declarations or
  ##  definitions are provided out-of-line. For example:
  ## Error: cannot render: rnCodeBlock
  ##  In the out-of-line definition of
  ##  the semantic parent is
  ##  the class
  ##  of which this function is a member. The lexical parent is
  ##  the place where the declaration actually occurs in the source code; in this
  ##  case, the definition occurs in the translation unit. In general, the
  ##  lexical parent for a given entity can change without affecting the semantics
  ##  of the program, and the lexical parent of different declarations of the
  ##  same entity may be different. Changing the semantic parent of a declaration,
  ##  on the other hand, can have a major impact on semantics, and redeclarations
  ##  of a particular entity should all have the same semantic context.
  ##  In the example above, both declarations of
  ##  have
  ##  as their
  ##  semantic context, while the lexical context of the first
  ##  is
  ##  and the lexical context of the second
  ##  is the translation unit.
  ##  For global declarations, the semantic parent is the translation unit.
proc getCursorLexicalParent*(cursor: CXCursor): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getCursorLexicalParent".}
  ##  Determine the lexical parent of the given cursor.
  ##  The lexical parent of a cursor is the cursor in which the given
  ##  was actually written. For many declarations, the lexical and semantic parents
  ##  are equivalent (the semantic parent is returned by
  ##  They diverge when declarations or
  ##  definitions are provided out-of-line. For example:
  ## Error: cannot render: rnCodeBlock
  ##  In the out-of-line definition of
  ##  the semantic parent is
  ##  the class
  ##  of which this function is a member. The lexical parent is
  ##  the place where the declaration actually occurs in the source code; in this
  ##  case, the definition occurs in the translation unit. In general, the
  ##  lexical parent for a given entity can change without affecting the semantics
  ##  of the program, and the lexical parent of different declarations of the
  ##  same entity may be different. Changing the semantic parent of a declaration,
  ##  on the other hand, can have a major impact on semantics, and redeclarations
  ##  of a particular entity should all have the same semantic context.
  ##  In the example above, both declarations of
  ##  have
  ##  as their
  ##  semantic context, while the lexical context of the first
  ##  is
  ##  and the lexical context of the second
  ##  is the translation unit.
  ##  For declarations written in the global scope, the lexical parent is
  ##  the translation unit.
proc getOverriddenCursors*(cursor: CXCursor; overridden: ptr[ptr[CXCursor]];
                          num_overridden: ptr[cuint]): void {.cdecl,
    dynlib: libclang, importc: "clang_getOverriddenCursors".}
  ##  Determine the set of methods that are overridden by the given
  ##  method.
  ##  In both Objective-C and C++, a method (aka virtual member function,
  ##  in C++) can override a virtual method in a base class. For
  ##  Objective-C, a method is said to override any method in the class's
  ##  base class, its protocols, or its categories' protocols, that has the same
  ##  selector and is of the same kind (class or instance).
  ##  If no such method exists, the search continues to the class's superclass,
  ##  its protocols, and its categories, and so on. A method from an Objective-C
  ##  implementation is considered to override the same methods as its
  ##  corresponding method in the interface.
  ##  For C++, a virtual member function overrides any virtual member
  ##  function with the same signature that occurs in its base
  ##  classes. With multiple inheritance, a virtual member function can
  ##  override several virtual member functions coming from different
  ##  base classes.
  ##  In all cases, this function determines the immediate overridden
  ##  method, rather than all of the overridden methods. For example, if
  ##  a method is originally declared in a class A, then overridden in B
  ##  (which in inherits from A) and also in C (which inherited from B),
  ##  then the only overridden method returned from this function when
  ##  invoked on C's method will be B's method. The client may then
  ##  invoke this function again, given the previously-found overridden
  ##  methods, to map out the complete method-override set.
  ## **cursor**
  ##  A cursor representing an Objective-C or C++
  ##  method. This routine will compute the set of methods that this
  ##  method overrides.
  ## **overridden**
  ##  A pointer whose pointee will be replaced with a
  ##  pointer to an array of cursors, representing the set of overridden
  ##  methods. If there are no overridden methods, the pointee will be
  ##  set to NULL. The pointee must be freed via a call to
  ## **num_overridden**
  ##  A pointer to the number of overridden
  ##  functions, will be set to the number of overridden functions in the
  ##  array pointed to by
proc disposeOverriddenCursors*(overridden: ptr[CXCursor]): void {.cdecl,
    dynlib: libclang, importc: "clang_disposeOverriddenCursors".}
  ##  Free the set of overridden cursors returned by
proc getIncludedFile*(cursor: CXCursor): CXFile {.cdecl, dynlib: libclang,
    importc: "clang_getIncludedFile".}
  ##  Retrieve the file that is included by the given inclusion directive
  ##  cursor.
proc getCursor*(argCXTranslationUnit: CXTranslationUnit;
               argCXSourceLocation: CXSourceLocation): CXCursor {.cdecl,
    dynlib: libclang, importc: "clang_getCursor".}
  ##  Map a source location to the cursor that describes the entity at that
  ##  location in the source code.
  ##  clang_getCursor() maps an arbitrary source location within a translation
  ##  unit down to the most specific cursor that describes the entity at that
  ##  location. For example, given an expression
  ##  + y, invoking
  ##  clang_getCursor() with a source location pointing to "x" will return the
  ##  cursor for "x"; similarly for "y". If the cursor points anywhere between
  ##  "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor()
  ##  will return a cursor referring to the "+" expression.
  ## **
  ##  a cursor representing the entity at the given source location, or
  ##  a NULL cursor if no such entity can be found.
proc getCursorLocation*(argCXCursor: CXCursor): CXSourceLocation {.cdecl,
    dynlib: libclang, importc: "clang_getCursorLocation".}
  ##  Retrieve the physical location of the source constructor referenced
  ##  by the given cursor.
  ##  The location of a declaration is typically the location of the name of that
  ##  declaration, where the name of that declaration would occur if it is
  ##  unnamed, or some keyword that introduces that particular declaration.
  ##  The location of a reference is where that reference occurs within the
  ##  source code.
proc getCursorExtent*(argCXCursor: CXCursor): CXSourceRange {.cdecl,
    dynlib: libclang, importc: "clang_getCursorExtent".}
  ##  Retrieve the physical extent of the source construct referenced by
  ##  the given cursor.
  ##  The extent of a cursor starts with the file/line/column pointing at the
  ##  first character within the source construct that the cursor refers to and
  ##  ends with the last character within that source construct. For a
  ##  declaration, the extent covers the declaration itself. For a reference,
  ##  the extent covers the location of the reference (e.g., where the referenced
  ##  entity was actually used).
type
  CXTypeKind* = enum            ##  Describes the kind of type
                  ## **SKipped enum values**
                  ## - CXType_FirstBuiltin = CXType_Void
                  ## - CXType_LastBuiltin = CXType_ULongAccum
    tkInvalid = 0,              ##  Represents an invalid type (e.g., where no type is available).
    tkUnexposed = 1, ##  A type whose specific kind is not exposed via this
                  ##  interface.
    tkVoid = 2, tkBool = 3, tkChar_U = 4, tkUChar = 5, tkChar16 = 6, tkChar32 = 7, tkUShort = 8,
    tkUInt = 9, tkULong = 10, tkULongLong = 11, tkUInt128 = 12, tkChar_S = 13, tkSChar = 14,
    tkWChar = 15, tkShort = 16, tkInt = 17, tkLong = 18, tkLongLong = 19, tkInt128 = 20,
    tkFloat = 21, tkDouble = 22, tkLongDouble = 23, tkNullPtr = 24, tkOverload = 25,
    tkDependent = 26, tkObjCId = 27, tkObjCClass = 28, tkObjCSel = 29, tkFloat128 = 30,
    tkHalf = 31, tkFloat16 = 32, tkShortAccum = 33, tkAccum = 34, tkLongAccum = 35,
    tkUShortAccum = 36, tkUAccum = 37, tkULongAccum = 38, tkComplex = 100, tkPointer = 101,
    tkBlockPointer = 102, tkLValueReference = 103, tkRValueReference = 104,
    tkRecord = 105, tkEnum = 106, tkTypedef = 107, tkObjCInterface = 108,
    tkObjCObjectPointer = 109, tkFunctionNoProto = 110, tkFunctionProto = 111,
    tkConstantArray = 112, tkVector = 113, tkIncompleteArray = 114,
    tkVariableArray = 115, tkDependentSizedArray = 116, tkMemberPointer = 117,
    tkAuto = 118, tkElaborated = 119, ##  Represents a type that was referred to using an elaborated type keyword.
                                ##  E.g., struct S, or via a qualified name, e.g., N::M::type, or both.
    tkPipe = 120, tkOCLImage1dRO = 121, tkOCLImage1dArrayRO = 122,
    tkOCLImage1dBufferRO = 123, tkOCLImage2dRO = 124, tkOCLImage2dArrayRO = 125,
    tkOCLImage2dDepthRO = 126, tkOCLImage2dArrayDepthRO = 127,
    tkOCLImage2dMSAARO = 128, tkOCLImage2dArrayMSAARO = 129,
    tkOCLImage2dMSAADepthRO = 130, tkOCLImage2dArrayMSAADepthRO = 131,
    tkOCLImage3dRO = 132, tkOCLImage1dWO = 133, tkOCLImage1dArrayWO = 134,
    tkOCLImage1dBufferWO = 135, tkOCLImage2dWO = 136, tkOCLImage2dArrayWO = 137,
    tkOCLImage2dDepthWO = 138, tkOCLImage2dArrayDepthWO = 139,
    tkOCLImage2dMSAAWO = 140, tkOCLImage2dArrayMSAAWO = 141,
    tkOCLImage2dMSAADepthWO = 142, tkOCLImage2dArrayMSAADepthWO = 143,
    tkOCLImage3dWO = 144, tkOCLImage1dRW = 145, tkOCLImage1dArrayRW = 146,
    tkOCLImage1dBufferRW = 147, tkOCLImage2dRW = 148, tkOCLImage2dArrayRW = 149,
    tkOCLImage2dDepthRW = 150, tkOCLImage2dArrayDepthRW = 151,
    tkOCLImage2dMSAARW = 152, tkOCLImage2dArrayMSAARW = 153,
    tkOCLImage2dMSAADepthRW = 154, tkOCLImage2dArrayMSAADepthRW = 155,
    tkOCLImage3dRW = 156, tkOCLSampler = 157, tkOCLEvent = 158, tkOCLQueue = 159,
    tkOCLReserveID = 160, tkObjCObject = 161, tkObjCTypeParam = 162, tkAttributed = 163,
    tkOCLIntelSubgroupAVCMcePayload = 164, tkOCLIntelSubgroupAVCImePayload = 165,
    tkOCLIntelSubgroupAVCRefPayload = 166, tkOCLIntelSubgroupAVCSicPayload = 167,
    tkOCLIntelSubgroupAVCMceResult = 168, tkOCLIntelSubgroupAVCImeResult = 169,
    tkOCLIntelSubgroupAVCRefResult = 170, tkOCLIntelSubgroupAVCSicResult = 171,
    tkOCLIntelSubgroupAVCImeResultSingleRefStreamout = 172,
    tkOCLIntelSubgroupAVCImeResultDualRefStreamout = 173,
    tkOCLIntelSubgroupAVCImeSingleRefStreamin = 174,
    tkOCLIntelSubgroupAVCImeDualRefStreamin = 175, tkExtVector = 176
type
  CXCallingConv* = enum ##  Describes the calling convention of a function type
                     ## **SKipped enum values**
                     ## - CXCallingConv_X86_64Win64 = CXCallingConv_Win64
    ccDefault = 0, ccC = 1, ccX86StdCall = 2, ccX86FastCall = 3, ccX86ThisCall = 4,
    ccX86Pascal = 5, ccAAPCS = 6, ccAAPCS_VFP = 7, ccX86RegCall = 8, ccIntelOclBicc = 9,
    ccWin64 = 10, ccX86_64SysV = 11, ccX86VectorCall = 12, ccSwift = 13,
    ccPreserveMost = 14, ccPreserveAll = 15, ccAArch64VectorCall = 16, ccInvalid = 100,
    ccUnexposed = 200
type
  CXType* {.pure, bycopy.} = object
    kind*: CXTypeKind
    data*: array[2, pointer]

proc getCursorType*(c: CXCursor): CXType {.cdecl, dynlib: libclang,
                                       importc: "clang_getCursorType".}
  ##  Retrieve the type of a CXCursor (if any).
proc getTypeSpelling*(cT: CXType): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getTypeSpelling".}
  ##  Pretty-print the underlying type using the rules of the
  ##  language of the translation unit from which it came.
  ##  If the type is invalid, an empty string is returned.
proc getTypedefDeclUnderlyingType*(c: CXCursor): CXType {.cdecl, dynlib: libclang,
    importc: "clang_getTypedefDeclUnderlyingType".}
  ##  Retrieve the underlying type of a typedef declaration.
  ##  If the cursor does not reference a typedef declaration, an invalid type is
  ##  returned.
proc getEnumDeclIntegerType*(c: CXCursor): CXType {.cdecl, dynlib: libclang,
    importc: "clang_getEnumDeclIntegerType".}
  ##  Retrieve the integer type of an enum declaration.
  ##  If the cursor does not reference an enum declaration, an invalid type is
  ##  returned.
proc getEnumConstantDeclValue*(c: CXCursor): clonglong {.cdecl, dynlib: libclang,
    importc: "clang_getEnumConstantDeclValue".}
  ##  Retrieve the integer value of an enum constant declaration as a signed
  ##   long long.
  ##  If the cursor does not reference an enum constant declaration, LLONG_MIN is returned.
  ##  Since this is also potentially a valid constant value, the kind of the cursor
  ##  must be verified before calling this function.
proc getEnumConstantDeclUnsignedValue*(c: CXCursor): culonglong {.cdecl,
    dynlib: libclang, importc: "clang_getEnumConstantDeclUnsignedValue".}
  ##  Retrieve the integer value of an enum constant declaration as an unsigned
  ##   long long.
  ##  If the cursor does not reference an enum constant declaration, ULLONG_MAX is returned.
  ##  Since this is also potentially a valid constant value, the kind of the cursor
  ##  must be verified before calling this function.
proc getFieldDeclBitWidth*(c: CXCursor): cint {.cdecl, dynlib: libclang,
    importc: "clang_getFieldDeclBitWidth".}
  ##  Retrieve the bit width of a bit field declaration as an integer.
  ##  If a cursor that is not a bit field declaration is passed in, -1 is returned.
proc getNumArguments*(c: CXCursor): cint {.cdecl, dynlib: libclang,
                                       importc: "clang_Cursor_getNumArguments".}
  ##  Retrieve the number of non-variadic arguments associated with a given
  ##  cursor.
  ##  The number of arguments can be determined for calls as well as for
  ##  declarations of functions or methods. For other cursors -1 is returned.
proc getArgument*(c: CXCursor; i: cuint): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getArgument".}
  ##  Retrieve the argument cursor of a function or method.
  ##  The argument cursor can be determined for calls as well as for declarations
  ##  of functions or methods. For other cursors and for invalid indices, an
  ##  invalid cursor is returned.
type
  CXTemplateArgumentKind* = enum ##  Describes the kind of a template argument.
                              ##  See the definition of llvm::clang::TemplateArgument::ArgKind for full
                              ##  element descriptions.
    takNull, takType, takDeclaration, takNullPtr, takIntegral, takTemplate,
    takTemplateExpansion, takExpression, takPack, takInvalid
proc getNumTemplateArguments*(c: CXCursor): cint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getNumTemplateArguments".}
  ## Returns the number of template args of a function decl representing a
  ##  template specialization.
  ##  If the argument cursor cannot be converted into a template function
  ##  declaration, -1 is returned.
  ##  For example, for the following declaration and specialization:
  ##    template
  ## <typename
  ##  T, int kInt, bool kBool>
  ##    void foo() { ... }
  ##    template
  ## <
  ## >
  ##    void foo
  ## <float
  ## , -7, true>();
  ##  The value 3 would be returned from this call.
proc getTemplateArgumentKind*(c: CXCursor; i: cuint): CXTemplateArgumentKind {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getTemplateArgumentKind".}
  ##  Retrieve the kind of the I'th template argument of the CXCursor C.
  ##  If the argument CXCursor does not represent a FunctionDecl, an invalid
  ##  template argument kind is returned.
  ##  For example, for the following declaration and specialization:
  ##    template
  ## <typename
  ##  T, int kInt, bool kBool>
  ##    void foo() { ... }
  ##    template
  ## <
  ## >
  ##    void foo
  ## <float
  ## , -7, true>();
  ##  For I = 0, 1, and 2, Type, Integral, and Integral will be returned,
  ##  respectively.
proc getTemplateArgumentType*(c: CXCursor; i: cuint): CXType {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getTemplateArgumentType".}
  ##  Retrieve a CXType representing the type of a TemplateArgument of a
  ##   function decl representing a template specialization.
  ##  If the argument CXCursor does not represent a FunctionDecl whose I'th
  ##  template argument has a kind of CXTemplateArgKind_Integral, an invalid type
  ##  is returned.
  ##  For example, for the following declaration and specialization:
  ##    template
  ## <typename
  ##  T, int kInt, bool kBool>
  ##    void foo() { ... }
  ##    template
  ## <
  ## >
  ##    void foo
  ## <float
  ## , -7, true>();
  ##  If called with I = 0, "float", will be returned.
  ##  Invalid types will be returned for I == 1 or 2.
proc getTemplateArgumentValue*(c: CXCursor; i: cuint): clonglong {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getTemplateArgumentValue".}
  ##  Retrieve the value of an Integral TemplateArgument (of a function
  ##   decl representing a template specialization) as a signed long long.
  ##  It is undefined to call this function on a CXCursor that does not represent a
  ##  FunctionDecl or whose I'th template argument is not an integral value.
  ##  For example, for the following declaration and specialization:
  ##    template
  ## <typename
  ##  T, int kInt, bool kBool>
  ##    void foo() { ... }
  ##    template
  ## <
  ## >
  ##    void foo
  ## <float
  ## , -7, true>();
  ##  If called with I = 1 or 2, -7 or true will be returned, respectively.
  ##  For I == 0, this function's behavior is undefined.
proc getTemplateArgumentUnsignedValue*(c: CXCursor; i: cuint): culonglong {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getTemplateArgumentUnsignedValue".}
  ##  Retrieve the value of an Integral TemplateArgument (of a function
  ##   decl representing a template specialization) as an unsigned long long.
  ##  It is undefined to call this function on a CXCursor that does not represent a
  ##  FunctionDecl or whose I'th template argument is not an integral value.
  ##  For example, for the following declaration and specialization:
  ##    template
  ## <typename
  ##  T, int kInt, bool kBool>
  ##    void foo() { ... }
  ##    template
  ## <
  ## >
  ##    void foo
  ## <float
  ## , 2147483649, true>();
  ##  If called with I = 1 or 2, 2147483649 or true will be returned, respectively.
  ##  For I == 0, this function's behavior is undefined.
proc equalTypes*(a: CXType; b: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_equalTypes".}
  ##  Determine whether two CXTypes represent the same type.
  ## **
  ##  non-zero if the CXTypes represent the same type and
  ##           zero otherwise.
proc getCanonicalType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                        importc: "clang_getCanonicalType".}
  ##  Return the canonical type for a CXType.
  ##  Clang's type system explicitly models typedefs and all the ways
  ##  a specific type can be represented.  The canonical type is the underlying
  ##  type with all the "sugar" removed.  For example, if 'T' is a typedef
  ##  for 'int', the canonical type for 'T' would be 'int'.
proc isConstQualifiedType*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isConstQualifiedType".}
  ##  Determine whether a CXType has the "const" qualifier set,
  ##  without looking through typedefs that may have added "const" at a
  ##  different level.
proc isMacroFunctionLike*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_isMacroFunctionLike".}
  ##  Determine whether a  CXCursor that is a macro, is
  ##  function like.
proc isMacroBuiltin*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
                                       importc: "clang_Cursor_isMacroBuiltin".}
  ##  Determine whether a  CXCursor that is a macro, is a
  ##  builtin one.
proc isFunctionInlined*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_isFunctionInlined".}
  ##  Determine whether a  CXCursor that is a function declaration, is an
  ##  inline declaration.
proc isVolatileQualifiedType*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isVolatileQualifiedType".}
  ##  Determine whether a CXType has the "volatile" qualifier set,
  ##  without looking through typedefs that may have added "volatile" at
  ##  a different level.
proc isRestrictQualifiedType*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isRestrictQualifiedType".}
  ##  Determine whether a CXType has the "restrict" qualifier set,
  ##  without looking through typedefs that may have added "restrict" at a
  ##  different level.
proc getAddressSpace*(t: CXType): cuint {.cdecl, dynlib: libclang,
                                      importc: "clang_getAddressSpace".}
  ##  Returns the address space of the given type.
proc getTypedefName*(cT: CXType): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getTypedefName".}
  ##  Returns the typedef name of the given type.
proc getPointeeType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                      importc: "clang_getPointeeType".}
  ##  For pointer types, returns the type of the pointee.
proc getTypeDeclaration*(t: CXType): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getTypeDeclaration".}
  ##  Return the cursor for the declaration of the given type.
proc getDeclObjCTypeEncoding*(c: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getDeclObjCTypeEncoding".}
  ##  Returns the Objective-C type encoding for the specified declaration.
proc getObjCEncoding*(cxtype: CXType): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Type_getObjCEncoding".}
  ##  Returns the Objective-C type encoding for the specified CXType.
proc getTypeKindSpelling*(k: CXTypeKind): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getTypeKindSpelling".}
  ##  Retrieve the spelling of a given CXTypeKind.
proc getFunctionTypeCallingConv*(t: CXType): CXCallingConv {.cdecl, dynlib: libclang,
    importc: "clang_getFunctionTypeCallingConv".}
  ##  Retrieve the calling convention associated with a function type.
  ##  If a non-function type is passed in, CXCallingConv_Invalid is returned.
proc getResultType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                     importc: "clang_getResultType".}
  ##  Retrieve the return type associated with a function type.
  ##  If a non-function type is passed in, an invalid type is returned.
proc getExceptionSpecificationType*(t: CXType): cint {.cdecl, dynlib: libclang,
    importc: "clang_getExceptionSpecificationType".}
  ##  Retrieve the exception specification type associated with a function type.
  ##  This is a value of type CXCursor_ExceptionSpecificationKind.
  ##  If a non-function type is passed in, an error code of -1 is returned.
proc getNumArgTypes*(t: CXType): cint {.cdecl, dynlib: libclang,
                                    importc: "clang_getNumArgTypes".}
  ##  Retrieve the number of non-variadic parameters associated with a
  ##  function type.
  ##  If a non-function type is passed in, -1 is returned.
proc getArgType*(t: CXType; i: cuint): CXType {.cdecl, dynlib: libclang,
    importc: "clang_getArgType".}
  ##  Retrieve the type of a parameter of a function type.
  ##  If a non-function type is passed in or the function does not have enough
  ##  parameters, an invalid type is returned.
proc getObjCObjectBaseType*(t: CXType): CXType {.cdecl, dynlib: libclang,
    importc: "clang_Type_getObjCObjectBaseType".}
  ##  Retrieves the base type of the ObjCObjectType.
  ##  If the type is not an ObjC object, an invalid type is returned.
proc getNumObjCProtocolRefs*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Type_getNumObjCProtocolRefs".}
  ##  Retrieve the number of protocol references associated with an ObjC object/id.
  ##  If the type is not an ObjC object, 0 is returned.
proc getObjCProtocolDecl*(t: CXType; i: cuint): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_Type_getObjCProtocolDecl".}
  ##  Retrieve the decl for a protocol reference for an ObjC object/id.
  ##  If the type is not an ObjC object or there are not enough protocol
  ##  references, an invalid cursor is returned.
proc getNumObjCTypeArgs*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Type_getNumObjCTypeArgs".}
  ##  Retreive the number of type arguments associated with an ObjC object.
  ##  If the type is not an ObjC object, 0 is returned.
proc getObjCTypeArg*(t: CXType; i: cuint): CXType {.cdecl, dynlib: libclang,
    importc: "clang_Type_getObjCTypeArg".}
  ##  Retrieve a type argument associated with an ObjC object.
  ##  If the type is not an ObjC or the index is not valid,
  ##  an invalid type is returned.
proc isFunctionTypeVariadic*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isFunctionTypeVariadic".}
  ##  Return 1 if the CXType is a variadic function type, and 0 otherwise.
proc getCursorResultType*(c: CXCursor): CXType {.cdecl, dynlib: libclang,
    importc: "clang_getCursorResultType".}
  ##  Retrieve the return type associated with a given cursor.
  ##  This only returns a valid type if the cursor refers to a function or method.
proc getCursorExceptionSpecificationType*(c: CXCursor): cint {.cdecl,
    dynlib: libclang, importc: "clang_getCursorExceptionSpecificationType".}
  ##  Retrieve the exception specification type associated with a given cursor.
  ##  This is a value of type CXCursor_ExceptionSpecificationKind.
  ##  This only returns a valid result if the cursor refers to a function or method.
proc isPODType*(t: CXType): cuint {.cdecl, dynlib: libclang, importc: "clang_isPODType".}
  ##  Return 1 if the CXType is a POD (plain old data) type, and 0
  ##   otherwise.
proc getElementType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                      importc: "clang_getElementType".}
  ##  Return the element type of an array, complex, or vector type.
  ##  If a type is passed in that is not an array, complex, or vector type,
  ##  an invalid type is returned.
proc getNumElements*(t: CXType): clonglong {.cdecl, dynlib: libclang,
    importc: "clang_getNumElements".}
  ##  Return the number of elements of an array or vector type.
  ##  If a type is passed in that is not an array or vector type,
  ##  -1 is returned.
proc getArrayElementType*(t: CXType): CXType {.cdecl, dynlib: libclang,
    importc: "clang_getArrayElementType".}
  ##  Return the element type of an array type.
  ##  If a non-array type is passed in, an invalid type is returned.
proc getArraySize*(t: CXType): clonglong {.cdecl, dynlib: libclang,
                                       importc: "clang_getArraySize".}
  ##  Return the array size of a constant array.
  ##  If a non-array type is passed in, -1 is returned.
proc getNamedType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                    importc: "clang_Type_getNamedType".}
  ##  Retrieve the type named by the qualified-id.
  ##  If a non-elaborated type is passed in, an invalid type is returned.
proc isTransparentTagTypedef*(t: CXType): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Type_isTransparentTagTypedef".}
  ##  Determine if a typedef is 'transparent' tag.
  ##  A typedef is considered 'transparent' if it shares a name and spelling
  ##  location with its underlying tag type, as is the case with the NS_ENUM macro.
  ## **
  ##  non-zero if transparent and zero otherwise.
type
  CXTypeNullabilityKind* = enum
    tnkNonNull = 0,             ##  Values of this type can never be null.
    tnkNullable = 1,            ##  Values of this type can be null.
    tnkUnspecified = 2, ##  Whether values of this type can be null is (explicitly)
                     ##  unspecified. This captures a (fairly rare) case where we
                     ##  can't conclude anything about the nullability of the type even
                     ##  though it has been considered.
    tnkInvalid = 3              ##  Nullability is not applicable to this type.
proc getNullability*(t: CXType): CXTypeNullabilityKind {.cdecl, dynlib: libclang,
    importc: "clang_Type_getNullability".}
  ##  Retrieve the nullability kind of a pointer type.
type
  CXTypeLayoutError* = enum ##  List the possible error codes for
                         ##  and
                         ##  A value of this enumeration type can be returned if the target type is not
                         ##  a valid argument to sizeof, alignof or offsetof.
    tleInvalid = 1,             ##  Type is of kind CXType_Invalid.
    tleIncomplete = 2,          ##  The type is an incomplete Type.
    tleDependent = 3,           ##  The type is a dependent Type.
    tleNotConstantSize = 4,     ##  The type is not a constant size type.
    tleInvalidFieldName = 5,    ##  The Field name is not valid for this record.
    tleUndeduced = 6            ##  The type is undeduced.
proc getAlignOf*(t: CXType): clonglong {.cdecl, dynlib: libclang,
                                     importc: "clang_Type_getAlignOf".}
  ##  Return the alignment of a type in bytes as per C++[expr.alignof]
  ##    standard.
  ##  If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
  ##  If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
  ##    is returned.
  ##  If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
  ##    returned.
  ##  If the type declaration is not a constant size type,
  ##    CXTypeLayoutError_NotConstantSize is returned.
proc getClassType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                    importc: "clang_Type_getClassType".}
  ##  Return the class type of an member pointer type.
  ##  If a non-member-pointer type is passed in, an invalid type is returned.
proc getSizeOf*(t: CXType): clonglong {.cdecl, dynlib: libclang,
                                    importc: "clang_Type_getSizeOf".}
  ##  Return the size of a type in bytes as per C++[expr.sizeof] standard.
  ##  If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
  ##  If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
  ##    is returned.
  ##  If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
  ##    returned.
proc getOffsetOf*(t: CXType; s: cstring): clonglong {.cdecl, dynlib: libclang,
    importc: "clang_Type_getOffsetOf".}
  ##  Return the offset of a field named S in a record of type T in bits
  ##    as it would be returned by __offsetof__ as per C++11[18.2p4]
  ##  If the cursor is not a record field declaration, CXTypeLayoutError_Invalid
  ##    is returned.
  ##  If the field's type declaration is an incomplete type,
  ##    CXTypeLayoutError_Incomplete is returned.
  ##  If the field's type declaration is a dependent type,
  ##    CXTypeLayoutError_Dependent is returned.
  ##  If the field's name S is not found,
  ##    CXTypeLayoutError_InvalidFieldName is returned.
proc getModifiedType*(t: CXType): CXType {.cdecl, dynlib: libclang,
                                       importc: "clang_Type_getModifiedType".}
  ##  Return the type that was modified by this attributed type.
  ##  If the type is not an attributed type, an invalid type is returned.
proc getOffsetOfField*(c: CXCursor): clonglong {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getOffsetOfField".}
  ##  Return the offset of the field represented by the Cursor.
  ##  If the cursor is not a field declaration, -1 is returned.
  ##  If the cursor semantic parent is not a record field declaration,
  ##    CXTypeLayoutError_Invalid is returned.
  ##  If the field's type declaration is an incomplete type,
  ##    CXTypeLayoutError_Incomplete is returned.
  ##  If the field's type declaration is a dependent type,
  ##    CXTypeLayoutError_Dependent is returned.
  ##  If the field's name S is not found,
  ##    CXTypeLayoutError_InvalidFieldName is returned.
proc isAnonymous*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
                                    importc: "clang_Cursor_isAnonymous".}
  ##  Determine whether the given cursor represents an anonymous
  ##  tag or namespace
proc isAnonymousRecordDecl*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_isAnonymousRecordDecl".}
  ##  Determine whether the given cursor represents an anonymous record
  ##  declaration.
proc isInlineNamespace*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_isInlineNamespace".}
  ##  Determine whether the given cursor represents an inline namespace
  ##  declaration.
type
  CXRefQualifierKind* = enum
    rqkNone = 0,                ##  No ref-qualifier was provided.
    rqkLValue,                ##  An lvalue ref-qualifier was provided (
    rqkRValue                 ##  An rvalue ref-qualifier was provided (
proc getNumTemplateArguments*(t: CXType): cint {.cdecl, dynlib: libclang,
    importc: "clang_Type_getNumTemplateArguments".}
  ##  Returns the number of template arguments for given template
  ##  specialization, or -1 if type
  ##  is not a template specialization.
proc getTemplateArgumentAsType*(t: CXType; i: cuint): CXType {.cdecl, dynlib: libclang,
    importc: "clang_Type_getTemplateArgumentAsType".}
  ##  Returns the type template argument of a template class specialization
  ##  at given index.
  ##  This function only returns template type arguments and does not handle
  ##  template template arguments or variadic packs.
proc getRefQualifier*(t: CXType): CXRefQualifierKind {.cdecl, dynlib: libclang,
    importc: "clang_Type_getCXXRefQualifier".}
  ##  Retrieve the ref-qualifier kind of a function or method.
  ##  The ref-qualifier is returned for C++ functions or methods. For other types
  ##  or non-C++ declarations, CXRefQualifier_None is returned.
proc isBitField*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
                                   importc: "clang_Cursor_isBitField".}
  ##  Returns non-zero if the cursor specifies a Record member that is a
  ##    bitfield.
proc isVirtualBase*(argCXCursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isVirtualBase".}
  ##  Returns 1 if the base class specified by the cursor with kind
  ##    CX_CXXBaseSpecifier is virtual.
type
  CX_AccessSpecifier* = enum ##  Represents the C++ access control level to a base class for a
                             ##  cursor with kind CX_CXXBaseSpecifier.
    asInvalidAccessSpecifier, asPublic, asProtected, asPrivate
proc getAccessSpecifier*(argCXCursor: CXCursor): CX_AccessSpecifier {.cdecl,
    dynlib: libclang, importc: "clang_getCXXAccessSpecifier".}
  ##  Returns the access control level for the referenced object.
  ##  If the cursor refers to a C++ declaration, its access control level within its
  ##  parent scope is returned. Otherwise, if the cursor refers to a base specifier or
  ##  access specifier, the specifier itself is returned.
type
  CX_StorageClass* = enum ##  Represents the storage classes as declared in the source. CX_SC_Invalid
                       ##  was added for the case that the passed cursor in not a declaration.
    scC_Invalid, scC_None, scC_Extern, scC_Static, scC_PrivateExtern,
    scC_OpenCLWorkGroupLocal, scC_Auto, scC_Register
proc getStorageClass*(argCXCursor: CXCursor): CX_StorageClass {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getStorageClass".}
  ##  Returns the storage class for a function or variable declaration.
  ##  If the passed in Cursor is not a function or variable declaration,
  ##  CX_SC_Invalid is returned else the storage class.
proc getNumOverloadedDecls*(cursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_getNumOverloadedDecls".}
  ##  Determine the number of overloaded declarations referenced by a
  ##  cursor.
  ## **cursor**
  ##  The cursor whose overloaded declarations are being queried.
  ## **
  ##  The number of overloaded declarations referenced by
  ##  If it
  ##  is not a
  ##  cursor, returns 0.
proc getOverloadedDecl*(cursor: CXCursor; index: cuint): CXCursor {.cdecl,
    dynlib: libclang, importc: "clang_getOverloadedDecl".}
  ##  Retrieve a cursor for one of the overloaded declarations referenced
  ##  by a
  ##  cursor.
  ## **cursor**
  ##  The cursor whose overloaded declarations are being queried.
  ## **index**
  ##  The zero-based index into the set of overloaded declarations in
  ##  the cursor.
  ## **
  ##  A cursor representing the declaration referenced by the given
  ##  at the specified
  ##  If the cursor does not have an
  ##  associated set of overloaded declarations, or if the index is out of bounds,
  ##  returns
proc getIBOutletCollectionType*(argCXCursor: CXCursor): CXType {.cdecl,
    dynlib: libclang, importc: "clang_getIBOutletCollectionType".}
  ##  For cursors representing an iboutletcollection attribute,
  ##   this function returns the collection element type.
type
  CXChildVisitResult* = enum ##  Describes how the traversal of the children of a particular
                          ##  cursor should proceed after visiting a particular child cursor.
                          ##  A value of this enumeration type should be returned by each
                          ##  to indicate how clang_visitChildren() proceed.
    cvrBreak,                 ##  Terminates the cursor traversal.
    cvrContinue, ##  Continues the cursor traversal with the next sibling of
                ##  the cursor just visited, without visiting its children.
    cvrRecurse ##  Recursively traverse the children of this cursor, using
              ##  the same visitor and client data.
type
  CXCursorVisitor* = distinct proc (a0: CXCursor; a1: CXCursor; a2: pointer): CXChildVisitResult {.
      cdecl.}
proc visitChildren*(parent: CXCursor; visitor: CXCursorVisitor;
                   client_data: CXClientData): cuint {.cdecl, dynlib: libclang,
    importc: "clang_visitChildren".}
  ##  Visit the children of a particular cursor.
  ##  This function visits all the direct children of the given cursor,
  ##  invoking the given
  ##  function with the cursors of each
  ##  visited child. The traversal may be recursive, if the visitor returns
  ##  The traversal may also be ended prematurely, if
  ##  the visitor returns
  ## **parent**
  ##  the cursor whose child may be visited. All kinds of
  ##  cursors can be visited, including invalid cursors (which, by
  ##  definition, have no children).
  ## **visitor**
  ##  the visitor function that will be invoked for each
  ##  child of
  ## **client_data**
  ##  pointer data supplied by the client, which will
  ##  be passed to the visitor each time it is invoked.
  ## **
  ##  a non-zero value if the traversal was terminated
  ##  prematurely by the visitor returning
proc getCursorUSR*(argCXCursor: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getCursorUSR".}
  ##  Retrieve a Unified Symbol Resolution (USR) for the entity referenced
  ##  by the given cursor.
  ##  A Unified Symbol Resolution (USR) is a string that identifies a particular
  ##  entity (function, class, variable, etc.) within a program. USRs can be
  ##  compared across translation units to determine, e.g., when references in
  ##  one translation refer to an entity defined in another translation unit.
proc constructUSR_ObjCClass*(class_name: cstring): CXString {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCClass".}
  ##  Construct a USR for a specified Objective-C class.
proc constructUSR_ObjCCategory*(class_name: cstring; category_name: cstring): CXString {.
    cdecl, dynlib: libclang, importc: "clang_constructUSR_ObjCCategory".}
  ##  Construct a USR for a specified Objective-C category.
proc constructUSR_ObjCProtocol*(protocol_name: cstring): CXString {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCProtocol".}
  ##  Construct a USR for a specified Objective-C protocol.
proc constructUSR_ObjCIvar*(name: cstring; classUSR: CXString): CXString {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCIvar".}
  ##  Construct a USR for a specified Objective-C instance variable and
  ##    the USR for its containing class.
proc constructUSR_ObjCMethod*(name: cstring; isInstanceMethod: cuint;
                             classUSR: CXString): CXString {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCMethod".}
  ##  Construct a USR for a specified Objective-C method and
  ##    the USR for its containing class.
proc constructUSR_ObjCProperty*(property: cstring; classUSR: CXString): CXString {.
    cdecl, dynlib: libclang, importc: "clang_constructUSR_ObjCProperty".}
  ##  Construct a USR for a specified Objective-C property and the USR
  ##   for its containing class.
proc getCursorSpelling*(argCXCursor: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getCursorSpelling".}
  ##  Retrieve a name for the entity referenced by this cursor.
proc getSpellingNameRange*(argCXCursor: CXCursor; pieceIndex: cuint; options: cuint): CXSourceRange {.
    cdecl, dynlib: libclang, importc: "clang_Cursor_getSpellingNameRange".}
  ##  Retrieve a range for a piece that forms the cursors spelling name.
  ##  Most of the times there is only one range for the complete spelling but for
  ##  Objective-C methods and Objective-C message expressions, there are multiple
  ##  pieces for each selector identifier.
  ## **pieceIndex**
  ##  the index of the spelling name piece. If this is greater
  ##  than the actual number of pieces, it will return a NULL (invalid) range.
  ## **options**
  ##  Reserved.
type
  CXPrintingPolicy* = distinct pointer
type
  CXPrintingPolicyProperty* = enum ##  Properties for the printing policy.
                                ##  See
                                ##  for more information.
                                ## **SKipped enum values**
                                ## - CXPrintingPolicy_LastProperty = CXPrintingPolicy_FullyQualifiedName
    pppIndentation, pppSuppressSpecifiers, pppSuppressTagKeyword,
    pppIncludeTagDefinition, pppSuppressScope, pppSuppressUnwrittenScope,
    pppSuppressInitializers, pppConstantArraySizeAsWritten,
    pppAnonymousTagLocations, pppSuppressStrongLifetime,
    pppSuppressLifetimeQualifiers, pppSuppressTemplateArgsInConstructors,
    pppBool, pppRestrict, pppAlignof, pppUnderscoreAlignof, pppUseVoidForZeroParams,
    pppTerseOutput, pppPolishForDeclaration, pppHalf, pppMSWChar,
    pppIncludeNewlines, pppMSVCFormatting, pppConstantsAsWritten,
    pppSuppressImplicitBase, pppFullyQualifiedName
proc getProperty*(policy: CXPrintingPolicy; property: CXPrintingPolicyProperty): cuint {.
    cdecl, dynlib: libclang, importc: "clang_PrintingPolicy_getProperty".}
  ##  Get a property value for the given printing policy.
proc setProperty*(policy: CXPrintingPolicy; property: CXPrintingPolicyProperty;
                 value: cuint): void {.cdecl, dynlib: libclang,
                                    importc: "clang_PrintingPolicy_setProperty".}
  ##  Set a property value for the given printing policy.
proc getCursorPrintingPolicy*(argCXCursor: CXCursor): CXPrintingPolicy {.cdecl,
    dynlib: libclang, importc: "clang_getCursorPrintingPolicy".}
  ##  Retrieve the default policy for the cursor.
  ##  The policy should be released after use with
proc dispose*(policy: CXPrintingPolicy): void {.cdecl, dynlib: libclang,
    importc: "clang_PrintingPolicy_dispose".}
  ##  Release a printing policy.
proc getCursorPrettyPrinted*(cursor: CXCursor; policy: CXPrintingPolicy): CXString {.
    cdecl, dynlib: libclang, importc: "clang_getCursorPrettyPrinted".}
  ##  Pretty print declarations.
  ## **Cursor**
  ##  The cursor representing a declaration.
  ## **Policy**
  ##  The policy to control the entities being printed. If
  ##  NULL, a default policy is used.
  ## **
  ##  The pretty printed declaration or the empty string for
  ##  other cursors.
proc getCursorDisplayName*(argCXCursor: CXCursor): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getCursorDisplayName".}
  ##  Retrieve the display name for the entity referenced by this cursor.
  ##  The display name contains extra information that helps identify the cursor,
  ##  such as the parameters of a function or template or the arguments of a
  ##  class template specialization.
proc getCursorReferenced*(argCXCursor: CXCursor): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getCursorReferenced".}
  ##  For a cursor that is a reference, retrieve a cursor representing the
  ##  entity that it references.
  ##  Reference cursors refer to other entities in the AST. For example, an
  ##  Objective-C superclass reference cursor refers to an Objective-C class.
  ##  This function produces the cursor for the Objective-C class from the
  ##  cursor for the superclass reference. If the input cursor is a declaration or
  ##  definition, it returns that declaration or definition unchanged.
  ##  Otherwise, returns the NULL cursor.
proc getCursorDefinition*(argCXCursor: CXCursor): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getCursorDefinition".}
  ##   For a cursor that is either a reference to or a declaration
  ##   of some entity, retrieve a cursor that describes the definition of
  ##   that entity.
  ##   Some entities can be declared multiple times within a translation
  ##   unit, but only one of those declarations can also be a
  ##   definition. For example, given:
  ## Error: cannot render: rnCodeBlock
  ##   there are three declarations of the function "f", but only the
  ##   second one is a definition. The clang_getCursorDefinition()
  ##   function will take any cursor pointing to a declaration of "f"
  ##   (the first or fourth lines of the example) or a cursor referenced
  ##   that uses "f" (the call to "f' inside "g") and will return a
  ##   declaration cursor pointing to the definition (the second "f"
  ##   declaration).
  ##   If given a cursor for which there is no corresponding definition,
  ##   e.g., because there is no definition of that entity within this
  ##   translation unit, returns a NULL cursor.
proc isCursorDefinition*(argCXCursor: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_isCursorDefinition".}
  ##  Determine whether the declaration pointed to by this cursor
  ##  is also a definition of that entity.
proc getCanonicalCursor*(argCXCursor: CXCursor): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getCanonicalCursor".}
  ##  Retrieve the canonical cursor corresponding to the given cursor.
  ##  In the C family of languages, many kinds of entities can be declared several
  ##  times within a single translation unit. For example, a structure type can
  ##  be forward-declared (possibly multiple times) and later defined:
  ## Error: cannot render: rnCodeBlock
  ##  The declarations and the definition of
  ##  are represented by three
  ##  different cursors, all of which are declarations of the same underlying
  ##  entity. One of these cursor is considered the "canonical" cursor, which
  ##  is effectively the representative for the underlying entity. One can
  ##  determine if two cursors are declarations of the same underlying entity by
  ##  comparing their canonical cursors.
  ## **
  ##  The canonical cursor for the entity referred to by the given cursor.
proc getObjCSelectorIndex*(argCXCursor: CXCursor): cint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCSelectorIndex".}
  ##  If the cursor points to a selector identifier in an Objective-C
  ##  method or message expression, this returns the selector index.
  ##  After getting a cursor with #clang_getCursor, this can be called to
  ##  determine if the location points to a selector identifier.
  ## **
  ##  The selector index if the cursor is an Objective-C method or message
  ##  expression and the cursor is pointing to a selector identifier, or -1
  ##  otherwise.
proc isDynamicCall*(c: CXCursor): cint {.cdecl, dynlib: libclang,
                                     importc: "clang_Cursor_isDynamicCall".}
  ##  Given a cursor pointing to a C++ method call or an Objective-C
  ##  message, returns non-zero if the method/message is "dynamic", meaning:
  ##  For a C++ method: the call is virtual.
  ##  For an Objective-C message: the receiver is an object instance, not 'super'
  ##  or a specific class.
  ##  If the method/message is "static" or the cursor does not point to a
  ##  method/message, it will return zero.
proc getReceiverType*(c: CXCursor): CXType {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getReceiverType".}
  ##  Given a cursor pointing to an Objective-C message or property
  ##  reference, or C++ method call, returns the CXType of the receiver.
type
  CXObjCPropertyAttrKind* = enum ##  Property attributes for a
    ocpaknoattr = 0, ocpakreadonly = 1, ocpakgetter = 2, ocpakassign = 4,
    ocpakreadwrite = 8, ocpakretain = 16, ocpakcopy = 32, ocpaknonatomic = 64,
    ocpaksetter = 128, ocpakatomic = 256, ocpakweak = 512, ocpakstrong = 1024,
    ocpakunsafe_unretained = 2048, ocpakclass = 4096
proc getObjCPropertyAttributes*(c: CXCursor; reserved: cuint): cuint {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getObjCPropertyAttributes".}
  ##  Given a cursor that represents a property declaration, return the
  ##  associated property attributes. The bits are formed from
  ## **reserved**
  ##  Reserved for future use, pass 0.
proc getObjCPropertyGetterName*(c: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCPropertyGetterName".}
  ##  Given a cursor that represents a property declaration, return the
  ##  name of the method that implements the getter.
proc getObjCPropertySetterName*(c: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCPropertySetterName".}
  ##  Given a cursor that represents a property declaration, return the
  ##  name of the method that implements the setter, if any.
type
  CXObjCDeclQualifierKind* = enum ##  'Qualifiers' written next to the return and parameter types in
                               ##  Objective-C method declarations.
    ocdqkNone = 0, ocdqkIn = 1, ocdqkInout = 2, ocdqkOut = 4, ocdqkBycopy = 8, ocdqkByref = 16,
    ocdqkOneway = 32
proc getObjCDeclQualifiers*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCDeclQualifiers".}
  ##  Given a cursor that represents an Objective-C method or parameter
  ##  declaration, return the associated Objective-C qualifiers for the return
  ##  type or the parameter respectively. The bits are formed from
  ##  CXObjCDeclQualifierKind.
proc isObjCOptional*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
                                       importc: "clang_Cursor_isObjCOptional".}
  ##  Given a cursor that represents an Objective-C method or property
  ##  declaration, return non-zero if the declaration was affected by "
  ## @
  ## optional".
  ##  Returns zero if the cursor is not such a declaration or it is "
  ## @
  ## required".
proc isVariadic*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
                                   importc: "clang_Cursor_isVariadic".}
  ##  Returns non-zero if the given cursor is a variadic function or method.
proc isExternalSymbol*(c: CXCursor; language: ptr[CXString];
                      definedIn: ptr[CXString]; isGenerated: ptr[cuint]): cuint {.
    cdecl, dynlib: libclang, importc: "clang_Cursor_isExternalSymbol".}
  ##  Returns non-zero if the given cursor points to a symbol marked with
  ##  external_source_symbol attribute.
  ## **language**
  ##  If non-NULL, and the attribute is present, will be set to
  ##  the 'language' string from the attribute.
  ## **definedIn**
  ##  If non-NULL, and the attribute is present, will be set to
  ##  the 'definedIn' string from the attribute.
  ## **isGenerated**
  ##  If non-NULL, and the attribute is present, will be set to
  ##  non-zero if the 'generated_declaration' is set in the attribute.
proc getCommentRange*(c: CXCursor): CXSourceRange {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getCommentRange".}
  ##  Given a cursor that represents a declaration, return the associated
  ##  comment's source range.  The range may include multiple consecutive comments
  ##  with whitespace in between.
proc getRawCommentText*(c: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getRawCommentText".}
  ##  Given a cursor that represents a declaration, return the associated
  ##  comment text, including comment markers.
proc getBriefCommentText*(c: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getBriefCommentText".}
  ##  Given a cursor that represents a documentable entity (e.g.,
  ##  declaration), return the associated
  ## ``
  ##  first paragraph.
proc getMangling*(argCXCursor: CXCursor): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getMangling".}
  ##  Retrieve the CXString representing the mangled name of the cursor.
proc getManglings*(argCXCursor: CXCursor): ptr[CXStringSet] {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getCXXManglings".}
  ##  Retrieve the CXStrings representing the mangled symbols of the C++
  ##  constructor or destructor at the cursor.
proc getObjCManglings*(argCXCursor: CXCursor): ptr[CXStringSet] {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getObjCManglings".}
  ##  Retrieve the CXStrings representing the mangled symbols of the ObjC
  ##  class interface or implementation at the cursor.
type
  CXModule* = distinct pointer
proc getModule*(c: CXCursor): CXModule {.cdecl, dynlib: libclang,
                                     importc: "clang_Cursor_getModule".}
  ##  Given a CXCursor_ModuleImportDecl cursor, return the associated module.
proc getModuleForFile*(argCXTranslationUnit: CXTranslationUnit; argCXFile: CXFile): CXModule {.
    cdecl, dynlib: libclang, importc: "clang_getModuleForFile".}
  ##  Given a CXFile header file, return the module that contains it, if one
  ##  exists.
proc getASTFile*(module: CXModule): CXFile {.cdecl, dynlib: libclang,
    importc: "clang_Module_getASTFile".}
  ## **Module**
  ##  a module object.
  ## **
  ##  the module file where the provided module object came from.
proc getParent*(module: CXModule): CXModule {.cdecl, dynlib: libclang,
    importc: "clang_Module_getParent".}
  ## **Module**
  ##  a module object.
  ## **
  ##  the parent of a sub-module or NULL if the given module is top-level,
  ##  e.g. for 'std.vector' it will return the 'std' module.
proc getName*(module: CXModule): CXString {.cdecl, dynlib: libclang,
                                        importc: "clang_Module_getName".}
  ## **Module**
  ##  a module object.
  ## **
  ##  the name of the module, e.g. for the 'std.vector' sub-module it
  ##  will return "vector".
proc getFullName*(module: CXModule): CXString {.cdecl, dynlib: libclang,
    importc: "clang_Module_getFullName".}
  ## **Module**
  ##  a module object.
  ## **
  ##  the full name of the module, e.g. "std.vector".
proc isSystem*(module: CXModule): cint {.cdecl, dynlib: libclang,
                                     importc: "clang_Module_isSystem".}
  ## **Module**
  ##  a module object.
  ## **
  ##  non-zero if the module is a system one.
proc module_getNumTopLevelHeaders*(argCXTranslationUnit: CXTranslationUnit;
                                  module: CXModule): cuint {.cdecl,
    dynlib: libclang, importc: "clang_Module_getNumTopLevelHeaders".}
  ## **Module**
  ##  a module object.
  ## **
  ##  the number of top level headers associated with this module.
proc module_getTopLevelHeader*(argCXTranslationUnit: CXTranslationUnit;
                              module: CXModule; index: cuint): CXFile {.cdecl,
    dynlib: libclang, importc: "clang_Module_getTopLevelHeader".}
  ## **Module**
  ##  a module object.
  ## **Index**
  ##  top level header index (zero-based).
  ## **
  ##  the specified top level header associated with the module.
proc constructor_isConvertingConstructor*(c: CXCursor): cuint {.cdecl,
    dynlib: libclang, importc: "clang_CXXConstructor_isConvertingConstructor".}
  ##  Determine if a C++ constructor is a converting constructor.
proc constructor_isCopyConstructor*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXConstructor_isCopyConstructor".}
  ##  Determine if a C++ constructor is a copy constructor.
proc constructor_isDefaultConstructor*(c: CXCursor): cuint {.cdecl,
    dynlib: libclang, importc: "clang_CXXConstructor_isDefaultConstructor".}
  ##  Determine if a C++ constructor is the default constructor.
proc constructor_isMoveConstructor*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXConstructor_isMoveConstructor".}
  ##  Determine if a C++ constructor is a move constructor.
proc field_isMutable*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXField_isMutable".}
  ##  Determine if a C++ field is declared 'mutable'.
proc method_isDefaulted*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXMethod_isDefaulted".}
  ##  Determine if a C++ method is declared '= default'.
proc method_isPureVirtual*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXMethod_isPureVirtual".}
  ##  Determine if a C++ member function or member function template is
  ##  pure virtual.
proc method_isStatic*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXMethod_isStatic".}
  ##  Determine if a C++ member function or member function template is
  ##  declared 'static'.
proc method_isVirtual*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXMethod_isVirtual".}
  ##  Determine if a C++ member function or member function template is
  ##  explicitly declared 'virtual' or if it overrides a virtual method from
  ##  one of the base classes.
proc record_isAbstract*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXRecord_isAbstract".}
  ##  Determine if a C++ record is abstract, i.e. whether a class or struct
  ##  has a pure virtual member function.
proc enumDecl_isScoped*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_EnumDecl_isScoped".}
  ##  Determine if an enum declaration refers to a scoped enum.
proc method_isConst*(c: CXCursor): cuint {.cdecl, dynlib: libclang,
    importc: "clang_CXXMethod_isConst".}
  ##  Determine if a C++ member function or member function template is
  ##  declared 'const'.
proc getTemplateCursorKind*(c: CXCursor): CXCursorKind {.cdecl, dynlib: libclang,
    importc: "clang_getTemplateCursorKind".}
  ##  Given a cursor that represents a template, determine
  ##  the cursor kind of the specializations would be generated by instantiating
  ##  the template.
  ##  This routine can be used to determine what flavor of function template,
  ##  class template, or class template partial specialization is stored in the
  ##  cursor. For example, it can describe whether a class template cursor is
  ##  declared with "struct", "class" or "union".
  ## **C**
  ##  The cursor to query. This cursor should represent a template
  ##  declaration.
  ## **
  ##  The cursor kind of the specializations that would be generated
  ##  by instantiating the template
  ##  If
  ##  is not a template, returns
proc getSpecializedCursorTemplate*(c: CXCursor): CXCursor {.cdecl, dynlib: libclang,
    importc: "clang_getSpecializedCursorTemplate".}
  ##  Given a cursor that may represent a specialization or instantiation
  ##  of a template, retrieve the cursor that represents the template that it
  ##  specializes or from which it was instantiated.
  ##  This routine determines the template involved both for explicit
  ##  specializations of templates and for implicit instantiations of the template,
  ##  both of which are referred to as "specializations". For a class template
  ##  specialization (e.g.,
  ##  this routine will return
  ##  either the primary template (
  ##  or, if the specialization was
  ##  instantiated from a class template partial specialization, the class template
  ##  partial specialization. For a class template partial specialization and a
  ##  function template specialization (including instantiations), this
  ##  this routine will return the specialized template.
  ##  For members of a class template (e.g., member functions, member classes, or
  ##  static data members), returns the specialized or instantiated member.
  ##  Although not strictly "templates" in the C++ language, members of class
  ##  templates have the same notions of specializations and instantiations that
  ##  templates do, so this routine treats them similarly.
  ## **C**
  ##  A cursor that may be a specialization of a template or a member
  ##  of a template.
  ## **
  ##  If the given cursor is a specialization or instantiation of a
  ##  template or a member thereof, the template or member that it specializes or
  ##  from which it was instantiated. Otherwise, returns a NULL cursor.
proc getCursorReferenceNameRange*(c: CXCursor; nameFlags: cuint; pieceIndex: cuint): CXSourceRange {.
    cdecl, dynlib: libclang, importc: "clang_getCursorReferenceNameRange".}
  ##  Given a cursor that references something else, return the source range
  ##  covering that reference.
  ## **C**
  ##  A cursor pointing to a member reference, a declaration reference, or
  ##  an operator call.
  ## **NameFlags**
  ##  A bitset with three independent flags:
  ##  CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and
  ##  CXNameRange_WantSinglePiece.
  ## **PieceIndex**
  ##  For contiguous names or when passing the flag
  ##  CXNameRange_WantSinglePiece, only one piece with index 0 is
  ##  available. When the CXNameRange_WantSinglePiece flag is not passed for a
  ##  non-contiguous names, this index can be used to retrieve the individual
  ##  pieces of the name. See also CXNameRange_WantSinglePiece.
  ## **
  ##  The piece of the name pointed to by the given cursor. If there is no
  ##  name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
type
  CXNameRefFlags* = enum
    nrfange_WantQualifier = 1, ##  Include the nested-name-specifier, e.g. Foo:: in x.Foo::y, in the
                            ##  range.
    nrfange_WantTemplateArgs = 2, ##  Include the explicit template arguments, e.g.
                               ## <
                               ## int> in x.f
                               ## <int
                               ## >,
                               ##  in the range.
    nrfange_WantSinglePiece = 4 ##  If the name is non-contiguous, return the full spanning range.
                             ##  Non-contiguous names occur in Objective-C when a selector with two or more
                             ##  parameters is used, or in C++ when using an operator:
                             ## Error: cannot render: rnCodeBlock
type
  CXTokenKind* = enum           ##  Describes a kind of token.
    tokPunctuation,           ##  A token that contains some kind of punctuation.
    tokKeyword,               ##  A language keyword.
    tokIdentifier,            ##  An identifier (that is not a keyword).
    tokLiteral,               ##  A numeric, string, or character literal.
    tokComment                ##  A comment.
type
  CXToken* {.pure, bycopy.} = object
    int_data*: array[4, cuint]
    ptr_data*: pointer

proc getToken*(tU: CXTranslationUnit; location: CXSourceLocation): ptr[CXToken] {.
    cdecl, dynlib: libclang, importc: "clang_getToken".}
  ##  Get the raw lexical token starting with the given location.
  ## **TU**
  ##  the translation unit whose text is being tokenized.
  ## **Location**
  ##  the source location with which the token starts.
  ## **
  ##  The token starting with the given location or NULL if no such token
  ##  exist. The returned pointer must be freed with clang_disposeTokens before the
  ##  translation unit is destroyed.
proc getTokenKind*(argCXToken: CXToken): CXTokenKind {.cdecl, dynlib: libclang,
    importc: "clang_getTokenKind".}
  ##  Determine the kind of the given token.
proc getTokenSpelling*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken): CXString {.
    cdecl, dynlib: libclang, importc: "clang_getTokenSpelling".}
  ##  Determine the spelling of the given token.
  ##  The spelling of a token is the textual representation of that token, e.g.,
  ##  the text of an identifier or keyword.
proc getTokenLocation*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken): CXSourceLocation {.
    cdecl, dynlib: libclang, importc: "clang_getTokenLocation".}
  ##  Retrieve the source location of the given token.
proc getTokenExtent*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken): CXSourceRange {.
    cdecl, dynlib: libclang, importc: "clang_getTokenExtent".}
  ##  Retrieve a source range that covers the given token.
proc tokenize*(tU: CXTranslationUnit; cxrange: CXSourceRange;
              tokens: ptr[ptr[CXToken]]; numTokens: ptr[cuint]): void {.cdecl,
    dynlib: libclang, importc: "clang_tokenize".}
  ##  Tokenize the source code described by the given range into raw
  ##  lexical tokens.
  ## **TU**
  ##  the translation unit whose text is being tokenized.
  ## **Range**
  ##  the source range in which text should be tokenized. All of the
  ##  tokens produced by tokenization will fall within this source range,
  ## **Tokens**
  ##  this pointer will be set to point to the array of tokens
  ##  that occur within the given source range. The returned pointer must be
  ##  freed with clang_disposeTokens() before the translation unit is destroyed.
  ## **NumTokens**
  ##  will be set to the number of tokens in the
  ##  array.
proc annotateTokens*(tU: CXTranslationUnit; tokens: ptr[CXToken]; numTokens: cuint;
                    cursors: ptr[CXCursor]): void {.cdecl, dynlib: libclang,
    importc: "clang_annotateTokens".}
  ##  Annotate the given set of tokens by providing cursors for each token
  ##  that can be mapped to a specific entity within the abstract syntax tree.
  ##  This token-annotation routine is equivalent to invoking
  ##  clang_getCursor() for the source locations of each of the
  ##  tokens. The cursors provided are filtered, so that only those
  ##  cursors that have a direct correspondence to the token are
  ##  accepted. For example, given a function call
  ##  clang_getCursor() would provide the following cursors:
  ##    * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.
  ##    * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.
  ##    * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'.
  ##  Only the first and last of these cursors will occur within the
  ##  annotate, since the tokens "f" and "x' directly refer to a function
  ##  and a variable, respectively, but the parentheses are just a small
  ##  part of the full syntax of the function call expression, which is
  ##  not provided as an annotation.
  ## **TU**
  ##  the translation unit that owns the given tokens.
  ## **Tokens**
  ##  the set of tokens to annotate.
  ## **NumTokens**
  ##  the number of tokens in
  ## **Cursors**
  ##  an array of
  ##  cursors, whose contents will be
  ##  replaced with the cursors corresponding to each token.
proc disposeTokens*(tU: CXTranslationUnit; tokens: ptr[CXToken]; numTokens: cuint): void {.
    cdecl, dynlib: libclang, importc: "clang_disposeTokens".}
  ##  Free the given set of tokens.
proc getCursorKindSpelling*(kind: CXCursorKind): CXString {.cdecl, dynlib: libclang,
    importc: "clang_getCursorKindSpelling".}
  ## ``
  ##  These routines are used for testing and debugging, only, and should not
  ##  be relied upon.
  ## @
  ## {
proc getDefinitionSpellingAndExtent*(argCXCursor: CXCursor; startBuf: cstringArray;
                                    endBuf: cstringArray; startLine: ptr[cuint];
                                    startColumn: ptr[cuint]; endLine: ptr[cuint];
                                    endColumn: ptr[cuint]): void {.cdecl,
    dynlib: libclang, importc: "clang_getDefinitionSpellingAndExtent".}
proc enableStackTraces*(): void {.cdecl, dynlib: libclang,
                               importc: "clang_enableStackTraces".}
proc executeOnThread*(fn: proc (a0: pointer): void {.cdecl.}; user_data: pointer;
                     stack_size: cuint): void {.cdecl, dynlib: libclang,
    importc: "clang_executeOnThread".}
type
  CXCompletionString* = distinct pointer
type
  CXCompletionResult* {.pure, bycopy.} = object
    cursorKind*: CXCursorKind
    completionString*: CXCompletionString

type
  CXCompletionChunkKind* = enum ##  Describes a single piece of text within a code-completion string.
                             ##  Each "chunk" within a code-completion string (
                             ##  is
                             ##  either a piece of text with a specific "kind" that describes how that text
                             ##  should be interpreted by the client or is another completion string.
    cckOptional, ##  A code-completion string that describes "optional" text that
                ##  could be a part of the template (but is not required).
                ##  The Optional chunk is the only kind of chunk that has a code-completion
                ##  string for its representation, which is accessible via
                ##  The code-completion string
                ##  describes an additional part of the template that is completely optional.
                ##  For example, optional chunks can be used to describe the placeholders for
                ##  arguments that match up with defaulted function parameters, e.g. given:
                ## Error: cannot render: rnCodeBlock
                ##  The code-completion string for this function would contain:
                ##    - a TypedText chunk for "f".
                ##    - a LeftParen chunk for "(".
                ##    - a Placeholder chunk for "int x"
                ##    - an Optional chunk containing the remaining defaulted arguments, e.g.,
                ##        - a Comma chunk for ","
                ##        - a Placeholder chunk for "float y"
                ##        - an Optional chunk containing the last defaulted argument:
                ##            - a Comma chunk for ","
                ##            - a Placeholder chunk for "double z"
                ##    - a RightParen chunk for ")"
                ##  There are many ways to handle Optional chunks. Two simple approaches are:
                ##    - Completely ignore optional chunks, in which case the template for the
                ##      function "f" would only include the first parameter ("int x").
                ##    - Fully expand all optional chunks, in which case the template for the
                ##      function "f" would have all of the parameters.
    cckTypedText, ##  Text that a user would be expected to type to get this
                 ##  code-completion result.
                 ##  There will be exactly one "typed text" chunk in a semantic string, which
                 ##  will typically provide the spelling of a keyword or the name of a
                 ##  declaration that could be used at the current code point. Clients are
                 ##  expected to filter the code-completion results based on the text in this
                 ##  chunk.
    cckText, ##  Text that should be inserted as part of a code-completion result.
            ##  A "text" chunk represents text that is part of the template to be
            ##  inserted into user code should this particular code-completion result
            ##  be selected.
    cckPlaceholder, ##  Placeholder text that should be replaced by the user.
                   ##  A "placeholder" chunk marks a place where the user should insert text
                   ##  into the code-completion template. For example, placeholders might mark
                   ##  the function parameters for a function declaration, to indicate that the
                   ##  user should provide arguments for each of those parameters. The actual
                   ##  text in a placeholder is a suggestion for the text to display before
                   ##  the user replaces the placeholder with real code.
    cckInformative, ##  Informative text that should be displayed but never inserted as
                   ##  part of the template.
                   ##  An "informative" chunk contains annotations that can be displayed to
                   ##  help the user decide whether a particular code-completion result is the
                   ##  right option, but which is not part of the actual template to be inserted
                   ##  by code completion.
    cckCurrentParameter, ##  Text that describes the current parameter when code-completion is
                        ##  referring to function call, message send, or template specialization.
                        ##  A "current parameter" chunk occurs when code-completion is providing
                        ##  information about a parameter corresponding to the argument at the
                        ##  code-completion point. For example, given a function
                        ## Error: cannot render: rnCodeBlock
                        ##  and the source code
                        ##  where the code-completion point is after the
                        ##  "(", the code-completion string will contain a "current parameter" chunk
                        ##  for "int x", indicating that the current argument will initialize that
                        ##  parameter. After typing further, to
                        ##  (where the code-completion
                        ##  point is after the ","), the code-completion string will contain a
                        ##  "current parameter" chunk to "int y".
    cckLeftParen, ##  A left parenthesis ('('), used to initiate a function call or
                 ##  signal the beginning of a function parameter list.
    cckRightParen, ##  A right parenthesis (')'), used to finish a function call or
                  ##  signal the end of a function parameter list.
    cckLeftBracket,           ##  A left bracket ('[').
    cckRightBracket,          ##  A right bracket (']').
    cckLeftBrace,             ##  A left brace ('{').
    cckRightBrace,            ##  A right brace ('}').
    cckLeftAngle,             ##  A left angle bracket ('
                 ## <
                 ## ').
    cckRightAngle,            ##  A right angle bracket ('>').
    cckComma,                 ##  A comma separator (',').
    cckResultType, ##  Text that specifies the result type of a given result.
                  ##  This special kind of informative chunk is not meant to be inserted into
                  ##  the text buffer. Rather, it is meant to illustrate the type that an
                  ##  expression using the given completion string would have.
    cckColon,                 ##  A colon (':').
    cckSemiColon,             ##  A semicolon (';').
    cckEqual,                 ##  An '=' sign.
    cckHorizontalSpace,       ##  Horizontal space (' ').
    cckVerticalSpace          ##  Vertical space ('
                    ## \\
                    ## n'), after which it is generally a good idea to
                    ##  perform indentation.
proc getCompletionChunkKind*(completion_string: CXCompletionString;
                            chunk_number: cuint): CXCompletionChunkKind {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionChunkKind".}
  ##  Determine the kind of a particular chunk within a completion string.
  ## **completion_string**
  ##  the completion string to query.
  ## **chunk_number**
  ##  the 0-based index of the chunk in the completion string.
  ## **
  ##  the kind of the chunk at the index
proc getCompletionChunkText*(completion_string: CXCompletionString;
                            chunk_number: cuint): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionChunkText".}
  ##  Retrieve the text associated with a particular chunk within a
  ##  completion string.
  ## **completion_string**
  ##  the completion string to query.
  ## **chunk_number**
  ##  the 0-based index of the chunk in the completion string.
  ## **
  ##  the text associated with the chunk at index
proc getCompletionChunkCompletionString*(completion_string: CXCompletionString;
                                        chunk_number: cuint): CXCompletionString {.
    cdecl, dynlib: libclang, importc: "clang_getCompletionChunkCompletionString".}
  ##  Retrieve the completion string associated with a particular chunk
  ##  within a completion string.
  ## **completion_string**
  ##  the completion string to query.
  ## **chunk_number**
  ##  the 0-based index of the chunk in the completion string.
  ## **
  ##  the completion string associated with the chunk at index
proc getNumCompletionChunks*(completion_string: CXCompletionString): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getNumCompletionChunks".}
  ##  Retrieve the number of chunks in the given code-completion string.
proc getCompletionPriority*(completion_string: CXCompletionString): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionPriority".}
  ##  Determine the priority of this code completion.
  ##  The priority of a code completion indicates how likely it is that this
  ##  particular completion is the completion that the user will select. The
  ##  priority is selected by various internal heuristics.
  ## **completion_string**
  ##  The completion string to query.
  ## **
  ##  The priority of this completion string. Smaller values indicate
  ##  higher-priority (more likely) completions.
proc getCompletionAvailability*(completion_string: CXCompletionString): CXAvailabilityKind {.
    cdecl, dynlib: libclang, importc: "clang_getCompletionAvailability".}
  ##  Determine the availability of the entity that this code-completion
  ##  string refers to.
  ## **completion_string**
  ##  The completion string to query.
  ## **
  ##  The availability of the completion string.
proc getCompletionNumAnnotations*(completion_string: CXCompletionString): cuint {.
    cdecl, dynlib: libclang, importc: "clang_getCompletionNumAnnotations".}
  ##  Retrieve the number of annotations associated with the given
  ##  completion string.
  ## **completion_string**
  ##  the completion string to query.
  ## **
  ##  the number of annotations associated with the given completion
  ##  string.
proc getCompletionAnnotation*(completion_string: CXCompletionString;
                             annotation_number: cuint): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionAnnotation".}
  ##  Retrieve the annotation associated with the given completion string.
  ## **completion_string**
  ##  the completion string to query.
  ## **annotation_number**
  ##  the 0-based index of the annotation of the
  ##  completion string.
  ## **
  ##  annotation string associated with the completion at index
  ##  or a NULL string if that annotation is not available.
proc getCompletionParent*(completion_string: CXCompletionString;
                         kind: ptr[CXCursorKind]): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionParent".}
  ##  Retrieve the parent context of the given completion string.
  ##  The parent context of a completion string is the semantic parent of
  ##  the declaration (if any) that the code completion represents. For example,
  ##  a code completion for an Objective-C method would have the method's class
  ##  or protocol as its context.
  ## **completion_string**
  ##  The code completion string whose parent is
  ##  being queried.
  ## **kind**
  ##  DEPRECATED: always set to CXCursor_NotImplemented if non-NULL.
  ## **
  ##  The name of the completion parent, e.g., "NSObject" if
  ##  the completion string represents a method in the NSObject class.
proc getCompletionBriefComment*(completion_string: CXCompletionString): CXString {.
    cdecl, dynlib: libclang, importc: "clang_getCompletionBriefComment".}
  ##  Retrieve the brief documentation comment attached to the declaration
  ##  that corresponds to the given completion string.
proc getCursorCompletionString*(cursor: CXCursor): CXCompletionString {.cdecl,
    dynlib: libclang, importc: "clang_getCursorCompletionString".}
  ##  Retrieve a completion string for an arbitrary declaration or macro
  ##  definition cursor.
  ## **cursor**
  ##  The cursor to query.
  ## **
  ##  A non-context-sensitive completion string for declaration and macro
  ##  definition cursors, or NULL for other kinds of cursors.
type
  CXCodeCompleteResults* {.pure, bycopy.} = object
    results*: ptr[CXCompletionResult]
    numResults*: cuint

proc getCompletionNumFixIts*(results: ptr[CXCodeCompleteResults];
                            completion_index: cuint): cuint {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionNumFixIts".}
  ##  Retrieve the number of fix-its for the given completion index.
  ##  Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts
  ##  option was set.
  ## **results**
  ##  The structure keeping all completion results
  ## **completion_index**
  ##  The index of the completion
  ## **
  ##  The number of fix-its which must be applied before the completion at
  ##  completion_index can be applied
proc getCompletionFixIt*(results: ptr[CXCodeCompleteResults];
                        completion_index: cuint; fixit_index: cuint;
                        replacement_range: ptr[CXSourceRange]): CXString {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionFixIt".}
  ##  Fix-its that *must* be applied before inserting the text for the
  ##  corresponding completion.
  ##  By default, clang_codeCompleteAt() only returns completions with empty
  ##  fix-its. Extra completions with non-empty fix-its should be explicitly
  ##  requested by setting CXCodeComplete_IncludeCompletionsWithFixIts.
  ##  For the clients to be able to compute position of the cursor after applying
  ##  fix-its, the following conditions are guaranteed to hold for
  ##  replacement_range of the stored fix-its:
  ##   - Ranges in the fix-its are guaranteed to never contain the completion
  ##   point (or identifier under completion point, if any) inside them, except
  ##   at the start or at the end of the range.
  ##   - If a fix-it range starts or ends with completion point (or starts or
  ##   ends after the identifier under completion point), it will contain at
  ##   least one character. It allows to unambiguously recompute completion
  ##   point after applying the fix-it.
  ##  The intuition is that provided fix-its change code around the identifier we
  ##  complete, but are not allowed to touch the identifier itself or the
  ##  completion point. One example of completions with corrections are the ones
  ##  replacing '.' with '->' and vice versa:
  ##  std::unique_ptr
  ## <std
  ## ::vector
  ## <int
  ## >> vec_ptr;
  ##  In 'vec_ptr.^', one of the completions is 'push_back', it requires
  ##  replacing '.' with '->'.
  ##  In 'vec_ptr->^', one of the completions is 'release', it requires
  ##  replacing '->' with '.'.
  ## **results**
  ##  The structure keeping all completion results
  ## **completion_index**
  ##  The index of the completion
  ## **fixit_index**
  ##  The index of the fix-it for the completion at
  ##  completion_index
  ## **replacement_range**
  ##  The fix-it range that must be replaced before the
  ##  completion at completion_index can be applied
  ## **
  ##  The fix-it string that must replace the code at replacement_range
  ##  before the completion at completion_index can be applied
type
  CXCodeComplete_Flags* = enum ##  Flags that can be passed to
                            ##  to
                            ##  modify its behavior.
                            ##  The enumerators in this enumeration can be bitwise-OR'd together to
                            ##  provide multiple options to
    ccfIncludeMacros = 1,       ##  Whether to include macros within the set of code
                       ##  completions returned.
    ccfIncludeCodePatterns = 2, ##  Whether to include code patterns for language constructs
                             ##  within the set of code completions, e.g., for loops.
    ccfIncludeBriefComments = 4, ##  Whether to include brief documentation within the set of code
                              ##  completions returned.
    ccfSkipPreamble = 8, ##  Whether to speed up completion by omitting top- or namespace-level entities
                      ##  defined in the preamble. There's no guarantee any particular entity is
                      ##  omitted. This may be useful if the headers are indexed externally.
    ccfIncludeCompletionsWithFixIts = 16 ##  Whether to include completions with small
                                      ##  fix-its, e.g. change '.' to '->' on member access, etc.
type
  CXCompletionContext* = enum ##  Bits that represent the context under which completion is occurring.
                           ##  The enumerators in this enumeration may be bitwise-OR'd together if multiple
                           ##  contexts are occurring simultaneously.
    cocUnexposed = 0, ##  The context for completions is unexposed, as only Clang results
                   ##  should be included. (This is equivalent to having no context bits set.)
    cocAnyType = 1,             ##  Completions for any possible type should be included in the results.
    cocAnyValue = 2, ##  Completions for any possible value (variables, function calls, etc.)
                  ##  should be included in the results.
    cocObjCObjectValue = 4, ##  Completions for values that resolve to an Objective-C object should
                         ##  be included in the results.
    cocObjCSelectorValue = 8, ##  Completions for values that resolve to an Objective-C selector
                           ##  should be included in the results.
    cocClassTypeValue = 16, ##  Completions for values that resolve to a C++ class type should be
                            ##  included in the results.
    cocDotMemberAccess = 32, ##  Completions for fields of the member being accessed using the dot
                          ##  operator should be included in the results.
    cocArrowMemberAccess = 64, ##  Completions for fields of the member being accessed using the arrow
                            ##  operator should be included in the results.
    cocObjCPropertyAccess = 128, ##  Completions for properties of the Objective-C object being accessed
                              ##  using the dot operator should be included in the results.
    cocEnumTag = 256,           ##  Completions for enum tags should be included in the results.
    cocUnionTag = 512,          ##  Completions for union tags should be included in the results.
    cocStructTag = 1024,        ##  Completions for struct tags should be included in the results.
    cocClassTag = 2048,         ##  Completions for C++ class names should be included in the results.
    cocNamespace = 4096, ##  Completions for C++ namespaces and namespace aliases should be
                      ##  included in the results.
    cocNestedNameSpecifier = 8192, ##  Completions for C++ nested name specifiers should be included in
                                ##  the results.
    cocObjCInterface = 16384, ##  Completions for Objective-C interfaces (classes) should be included
                           ##  in the results.
    cocObjCProtocol = 32768, ##  Completions for Objective-C protocols should be included in
                          ##  the results.
    cocObjCCategory = 65536, ##  Completions for Objective-C categories should be included in
                          ##  the results.
    cocObjCInstanceMessage = 131072, ##  Completions for Objective-C instance messages should be included
                                  ##  in the results.
    cocObjCClassMessage = 262144, ##  Completions for Objective-C class messages should be included in
                               ##  the results.
    cocObjCSelectorName = 524288, ##  Completions for Objective-C selector names should be included in
                               ##  the results.
    cocMacroName = 1048576, ##  Completions for preprocessor macro names should be included in
                         ##  the results.
    cocNaturalLanguage = 2097152, ##  Natural language completions should be included in the results.
    cocIncludedFile = 4194304,  ##  #include file completions should be included in the results.
    cocUnknown                ##  The current context is unknown, so set all contexts.
proc defaultCodeCompleteOptions*(): cuint {.cdecl, dynlib: libclang,
    importc: "clang_defaultCodeCompleteOptions".}
  ##  Returns a default set of code-completion options that can be
  ##  passed to
proc codeCompleteAt*(tU: CXTranslationUnit; complete_filename: cstring;
                    complete_line: cuint; complete_column: cuint;
                    unsaved_files: ptr[CXUnsavedFile]; num_unsaved_files: cuint;
                    options: cuint): ptr[CXCodeCompleteResults] {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteAt".}
  ##  Perform code completion at a given location in a translation unit.
  ##  This function performs code completion at a particular file, line, and
  ##  column within source code, providing results that suggest potential
  ##  code snippets based on the context of the completion. The basic model
  ##  for code completion is that Clang will parse a complete source file,
  ##  performing syntax checking up to the location where code-completion has
  ##  been requested. At that point, a special code-completion token is passed
  ##  to the parser, which recognizes this token and determines, based on the
  ##  current location in the C/Objective-C/C++ grammar and the state of
  ##  semantic analysis, what completions to provide. These completions are
  ##  returned via a new
  ##  structure.
  ##  Code completion itself is meant to be triggered by the client when the
  ##  user types punctuation characters or whitespace, at which point the
  ##  code-completion location will coincide with the cursor. For example, if
  ##  is a pointer, code-completion might be triggered after the "-" and then
  ##  after the ">" in
  ##  When the code-completion location is after the ">",
  ##  the completion results will provide, e.g., the members of the struct that
  ##  "p" points to. The client is responsible for placing the cursor at the
  ##  beginning of the token currently being typed, then filtering the results
  ##  based on the contents of the token. For example, when code-completing for
  ##  the expression
  ##  the client should provide the location just after
  ##  the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the
  ##  client can filter the results based on the current token text ("get"), only
  ##  showing those results that start with "get". The intent of this interface
  ##  is to separate the relatively high-latency acquisition of code-completion
  ##  results from the filtering of results on a per-character basis, which must
  ##  have a lower latency.
  ## **TU**
  ##  The translation unit in which code-completion should
  ##  occur. The source files for this translation unit need not be
  ##  completely up-to-date (and the contents of those source files may
  ##  be overridden via
  ##  Cursors referring into the
  ##  translation unit may be invalidated by this invocation.
  ## **complete_filename**
  ##  The name of the source file where code
  ##  completion should be performed. This filename may be any file
  ##  included in the translation unit.
  ## **complete_line**
  ##  The line at which code-completion should occur.
  ## **complete_column**
  ##  The column at which code-completion should occur.
  ##  Note that the column should point just after the syntactic construct that
  ##  initiated code completion, and not in the middle of a lexical token.
  ## **unsaved_files**
  ##  the Files that have not yet been saved to disk
  ##  but may be required for parsing or code completion, including the
  ##  contents of those files.  The contents and name of these files (as
  ##  specified by CXUnsavedFile) are copied when necessary, so the
  ##  client only needs to guarantee their validity until the call to
  ##  this function returns.
  ## **num_unsaved_files**
  ##  The number of unsaved file entries in
  ## **options**
  ##  Extra options that control the behavior of code
  ##  completion, expressed as a bitwise OR of the enumerators of the
  ##  CXCodeComplete_Flags enumeration. The
  ##  function returns a default set
  ##  of code-completion options.
  ## **
  ##  If successful, a new
  ##  structure
  ##  containing code-completion results, which should eventually be
  ##  freed with
  ##  If code
  ##  completion fails, returns NULL.
proc sortCodeCompletionResults*(results: ptr[CXCompletionResult]; numResults: cuint): void {.
    cdecl, dynlib: libclang, importc: "clang_sortCodeCompletionResults".}
  ##  Sort the code-completion results in case-insensitive alphabetical
  ##  order.
  ## **Results**
  ##  The set of results to sort.
  ## **NumResults**
  ##  The number of results in
proc disposeCodeCompleteResults*(results: ptr[CXCodeCompleteResults]): void {.cdecl,
    dynlib: libclang, importc: "clang_disposeCodeCompleteResults".}
  ##  Free the given set of code-completion results.
proc codeCompleteGetNumDiagnostics*(results: ptr[CXCodeCompleteResults]): cuint {.
    cdecl, dynlib: libclang, importc: "clang_codeCompleteGetNumDiagnostics".}
  ##  Determine the number of diagnostics produced prior to the
  ##  location where code completion was performed.
proc codeCompleteGetDiagnostic*(results: ptr[CXCodeCompleteResults]; index: cuint): CXDiagnostic {.
    cdecl, dynlib: libclang, importc: "clang_codeCompleteGetDiagnostic".}
  ##  Retrieve a diagnostic associated with the given code completion.
  ## **Results**
  ##  the code completion results to query.
  ## **Index**
  ##  the zero-based diagnostic number to retrieve.
  ## **
  ##  the requested diagnostic. This diagnostic must be freed
  ##  via a call to
proc codeCompleteGetContexts*(results: ptr[CXCodeCompleteResults]): culonglong {.
    cdecl, dynlib: libclang, importc: "clang_codeCompleteGetContexts".}
  ##  Determines what completions are appropriate for the context
  ##  the given code completion.
  ## **Results**
  ##  the code completion results to query
  ## **
  ##  the kinds of completions that are appropriate for use
  ##  along with the given code completion results.
proc codeCompleteGetContainerKind*(results: ptr[CXCodeCompleteResults];
                                  isIncomplete: ptr[cuint]): CXCursorKind {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteGetContainerKind".}
  ##  Returns the cursor kind for the container for the current code
  ##  completion context. The container is only guaranteed to be set for
  ##  contexts where a container exists (i.e. member accesses or Objective-C
  ##  message sends); if there is not a container, this function will return
  ##  CXCursor_InvalidCode.
  ## **Results**
  ##  the code completion results to query
  ## **IsIncomplete**
  ##  on return, this value will be false if Clang has complete
  ##  information about the container. If Clang does not have complete
  ##  information, this value will be true.
  ## **
  ##  the container kind, or CXCursor_InvalidCode if there is not a
  ##  container
proc codeCompleteGetContainerUSR*(results: ptr[CXCodeCompleteResults]): CXString {.
    cdecl, dynlib: libclang, importc: "clang_codeCompleteGetContainerUSR".}
  ##  Returns the USR for the container for the current code completion
  ##  context. If there is not a container for the current context, this
  ##  function will return the empty string.
  ## **Results**
  ##  the code completion results to query
  ## **
  ##  the USR for the container
proc codeCompleteGetObjCSelector*(results: ptr[CXCodeCompleteResults]): CXString {.
    cdecl, dynlib: libclang, importc: "clang_codeCompleteGetObjCSelector".}
  ##  Returns the currently-entered selector for an Objective-C message
  ##  send, formatted like "initWithFoo:bar:". Only guaranteed to return a
  ##  non-empty string for CXCompletionContext_ObjCInstanceMessage and
  ##  CXCompletionContext_ObjCClassMessage.
  ## **Results**
  ##  the code completion results to query
  ## **
  ##  the selector (or partial selector) that has been entered thus far
  ##  for an Objective-C message send.
proc getClangVersion*(): CXString {.cdecl, dynlib: libclang,
                                 importc: "clang_getClangVersion".}
  ##  Return a version string, suitable for showing to a user, but not
  ##         intended to be parsed (the format is not guaranteed to be stable).
proc toggleCrashRecovery*(isEnabled: cuint): void {.cdecl, dynlib: libclang,
    importc: "clang_toggleCrashRecovery".}
  ##  Enable/disable crash recovery.
  ## **isEnabled**
  ##  Flag to indicate if crash recovery is enabled.  A non-zero
  ##         value enables crash recovery, while 0 disables it.
type
  CXInclusionVisitor* = distinct proc (a0: pointer; a1: ptr[CXSourceLocation];
                                   a2: cuint; a3: pointer): void {.cdecl.}
proc getInclusions*(tu: CXTranslationUnit; visitor: CXInclusionVisitor;
                   client_data: CXClientData): void {.cdecl, dynlib: libclang,
    importc: "clang_getInclusions".}
  ##  Visit the set of preprocessor inclusions in a translation unit.
  ##    The visitor function is called with the provided data for every included
  ##    file.  This does not include headers included by the PCH file (unless one
  ##    is inspecting the inclusions in the PCH file itself).
type
  CXEvalResultKind* = enum
    erkUnExposed = 0, erkInt = 1, erkFloat = 2, erkObjCStrLiteral = 3, erkStrLiteral = 4,
    erkCFStr = 5, erkOther = 6
type
  CXEvalResult* = distinct pointer
proc evaluate*(c: CXCursor): CXEvalResult {.cdecl, dynlib: libclang,
                                        importc: "clang_Cursor_Evaluate".}
  ##  If cursor is a statement declaration tries to evaluate the
  ##  statement and if its variable, tries to evaluate its initializer,
  ##  into its corresponding type.
proc getKind*(e: CXEvalResult): CXEvalResultKind {.cdecl, dynlib: libclang,
    importc: "clang_EvalResult_getKind".}
  ##  Returns the kind of the evaluated result.
proc getAsInt*(e: CXEvalResult): cint {.cdecl, dynlib: libclang,
                                    importc: "clang_EvalResult_getAsInt".}
  ##  Returns the evaluation result as integer if the
  ##  kind is Int.
proc getAsLongLong*(e: CXEvalResult): clonglong {.cdecl, dynlib: libclang,
    importc: "clang_EvalResult_getAsLongLong".}
  ##  Returns the evaluation result as a long long integer if the
  ##  kind is Int. This prevents overflows that may happen if the result is
  ##  returned with clang_EvalResult_getAsInt.
proc isUnsignedInt*(e: CXEvalResult): cuint {.cdecl, dynlib: libclang,
    importc: "clang_EvalResult_isUnsignedInt".}
  ##  Returns a non-zero value if the kind is Int and the evaluation
  ##  result resulted in an unsigned integer.
proc getAsUnsigned*(e: CXEvalResult): culonglong {.cdecl, dynlib: libclang,
    importc: "clang_EvalResult_getAsUnsigned".}
  ##  Returns the evaluation result as an unsigned integer if
  ##  the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
proc getAsDouble*(e: CXEvalResult): cdouble {.cdecl, dynlib: libclang,
    importc: "clang_EvalResult_getAsDouble".}
  ##  Returns the evaluation result as double if the
  ##  kind is double.
proc getAsStr*(e: CXEvalResult): cstring {.cdecl, dynlib: libclang,
                                       importc: "clang_EvalResult_getAsStr".}
  ##  Returns the evaluation result as a constant string if the
  ##  kind is other than Int or float. User must not free this pointer,
  ##  instead call clang_EvalResult_dispose on the CXEvalResult returned
  ##  by clang_Cursor_Evaluate.
proc dispose*(e: CXEvalResult): void {.cdecl, dynlib: libclang,
                                   importc: "clang_EvalResult_dispose".}
  ##  Disposes the created Eval memory.
type
  CXRemapping* = distinct pointer
proc getRemappings*(path: cstring): CXRemapping {.cdecl, dynlib: libclang,
    importc: "clang_getRemappings".}
  ##  Retrieve a remapping.
  ## **path**
  ##  the path that contains metadata about remappings.
  ## **
  ##  the requested remapping. This remapping must be freed
  ##  via a call to
  ##  Can return NULL if an error occurred.
proc getRemappingsFromFileList*(filePaths: cstringArray; numFiles: cuint): CXRemapping {.
    cdecl, dynlib: libclang, importc: "clang_getRemappingsFromFileList".}
  ##  Retrieve a remapping.
  ## **filePaths**
  ##  pointer to an array of file paths containing remapping info.
  ## **numFiles**
  ##  number of file paths.
  ## **
  ##  the requested remapping. This remapping must be freed
  ##  via a call to
  ##  Can return NULL if an error occurred.
proc remap_getNumFiles*(argCXRemapping: CXRemapping): cuint {.cdecl,
    dynlib: libclang, importc: "clang_remap_getNumFiles".}
  ##  Determine the number of remappings.
proc remap_getFilenames*(argCXRemapping: CXRemapping; index: cuint;
                        original: ptr[CXString]; transformed: ptr[CXString]): void {.
    cdecl, dynlib: libclang, importc: "clang_remap_getFilenames".}
  ##  Get the original and the associated filename from the remapping.
  ## **original**
  ##  If non-NULL, will be set to the original filename.
  ## **transformed**
  ##  If non-NULL, will be set to the filename that the original
  ##  is associated with.
proc remap_dispose*(argCXRemapping: CXRemapping): void {.cdecl, dynlib: libclang,
    importc: "clang_remap_dispose".}
  ##  Dispose the remapping.
type
  CXVisitorResult* = enum       ## ``
                       ## @
                       ## {
    vrBreak, vrContinue
type
  CXCursorAndRangeVisitor* {.pure, bycopy.} = object
    context*: pointer
    visit*: proc (a0: pointer; a1: CXCursor; a2: CXSourceRange): CXVisitorResult {.cdecl.}

type
  CXResult* = enum
    rSuccess = 0,               ##  Function returned successfully.
    rInvalid = 1,               ##  One of the parameters was invalid for the function.
    rVisitBreak = 2 ##  The function was terminated by a callback (e.g. it returned
                 ##  CXVisit_Break)
proc findReferencesInFile*(cursor: CXCursor; file: CXFile;
                          visitor: CXCursorAndRangeVisitor): CXResult {.cdecl,
    dynlib: libclang, importc: "clang_findReferencesInFile".}
  ##  Find references of a declaration in a specific file.
  ## **cursor**
  ##  pointing to a declaration or a reference of one.
  ## **file**
  ##  to search for references.
  ## **visitor**
  ##  callback that will receive pairs of CXCursor/CXSourceRange for
  ##  each reference found.
  ##  The CXSourceRange will point inside the file; if the reference is inside
  ##  a macro (and not a macro argument) the CXSourceRange will be invalid.
  ## **
  ##  one of the CXResult enumerators.
proc findIncludesInFile*(tU: CXTranslationUnit; file: CXFile;
                        visitor: CXCursorAndRangeVisitor): CXResult {.cdecl,
    dynlib: libclang, importc: "clang_findIncludesInFile".}
  ##  Find #import/#include directives in a specific file.
  ## **TU**
  ##  translation unit containing the file to query.
  ## **file**
  ##  to search for #import/#include directives.
  ## **visitor**
  ##  callback that will receive pairs of CXCursor/CXSourceRange for
  ##  each directive found.
  ## **
  ##  one of the CXResult enumerators.
type
  CXIdxClientFile* = distinct pointer
type
  CXIdxClientEntity* = distinct pointer
type
  CXIdxClientContainer* = distinct pointer
type
  CXIdxClientASTFile* = distinct pointer
type
  CXIdxLoc* {.pure, bycopy.} = object
    ptr_data*: array[2, pointer]
    int_data*: cuint

type
  CXIdxIncludedFileInfo* {.pure, bycopy.} = object
    hashLoc*: CXIdxLoc
    filename*: cstring
    file*: CXFile
    isImport*: cint
    isAngled*: cint
    isModuleImport*: cint

type
  CXIdxImportedASTFileInfo* {.pure, bycopy.} = object
    file*: CXFile
    module*: CXModule
    loc*: CXIdxLoc
    isImplicit*: cint

type
  CXIdxEntityKind* = enum
    iekUnexposed = 0, iekTypedef = 1, iekFunction = 2, iekVariable = 3, iekField = 4,
    iekEnumConstant = 5, iekObjCClass = 6, iekObjCProtocol = 7, iekObjCCategory = 8,
    iekObjCInstanceMethod = 9, iekObjCClassMethod = 10, iekObjCProperty = 11,
    iekObjCIvar = 12, iekEnum = 13, iekStruct = 14, iekUnion = 15, iekClass = 16,
    iekNamespace = 17, iekNamespaceAlias = 18, iekStaticVariable = 19,
    iekStaticMethod = 20, iekInstanceMethod = 21, iekConstructor = 22,
    iekDestructor = 23, iekConversionFunction = 24, iekTypeAlias = 25,
    iekInterface = 26
type
  CXIdxEntityLanguage* = enum
    ielNone = 0, ielC = 1, ielObjC = 2, ielCXX = 3, ielSwift = 4
type
  CXIdxEntityTemplateKind* = enum ##  Extra C++ template information for an entity. This can apply to:
                                  ##  CXIdxEntity_Function
                                  ##  CXIdxEntity_CXXClass
                                  ##  CXIdxEntity_CXXStaticMethod
                                  ##  CXIdxEntity_CXXInstanceMethod
                                  ##  CXIdxEntity_CXXConstructor
                                  ##  CXIdxEntity_CXXConversionFunction
                                  ##  CXIdxEntity_CXXTypeAlias
    ietkNonTemplate = 0, ietkTemplate = 1, ietkTemplatePartialSpecialization = 2,
    ietkTemplateSpecialization = 3
type
  CXIdxAttrKind* = enum
    iakUnexposed = 0, iakIBAction = 1, iakIBOutlet = 2, iakIBOutletCollection = 3
type
  CXIdxAttrInfo* {.pure, bycopy.} = object
    kind*: CXIdxAttrKind
    cursor*: CXCursor
    loc*: CXIdxLoc

type
  CXIdxEntityInfo* {.pure, bycopy.} = object
    kind*: CXIdxEntityKind
    templateKind*: CXIdxEntityTemplateKind
    lang*: CXIdxEntityLanguage
    name*: cstring
    uSR*: cstring
    cursor*: CXCursor
    attributes*: ptr[ptr[CXIdxAttrInfo]]
    numAttributes*: cuint

type
  CXIdxContainerInfo* {.pure, bycopy.} = object
    cursor*: CXCursor

type
  CXIdxIBOutletCollectionAttrInfo* {.pure, bycopy.} = object
    attrInfo*: ptr[CXIdxAttrInfo]
    objcClass*: ptr[CXIdxEntityInfo]
    classCursor*: CXCursor
    classLoc*: CXIdxLoc

type
  CXIdxDeclInfoFlags* = enum
    idifFlag_Skipped = 1
type
  CXIdxDeclInfo* {.pure, bycopy.} = object
    entityInfo*: ptr[CXIdxEntityInfo]
    cursor*: CXCursor
    loc*: CXIdxLoc
    semanticContainer*: ptr[CXIdxContainerInfo]
    lexicalContainer*: ptr[CXIdxContainerInfo]
    isRedeclaration*: cint
    isDefinition*: cint
    isContainer*: cint
    declAsContainer*: ptr[CXIdxContainerInfo]
    isImplicit*: cint
    attributes*: ptr[ptr[CXIdxAttrInfo]]
    numAttributes*: cuint
    flags*: cuint

type
  CXIdxObjCContainerKind* = enum
    iocckForwardRef = 0, iocckInterface = 1, iocckImplementation = 2
type
  CXIdxObjCContainerDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[CXIdxDeclInfo]
    kind*: CXIdxObjCContainerKind

type
  CXIdxBaseClassInfo* {.pure, bycopy.} = object
    base*: ptr[CXIdxEntityInfo]
    cursor*: CXCursor
    loc*: CXIdxLoc

type
  CXIdxObjCProtocolRefInfo* {.pure, bycopy.} = object
    protocol*: ptr[CXIdxEntityInfo]
    cursor*: CXCursor
    loc*: CXIdxLoc

type
  CXIdxObjCProtocolRefListInfo* {.pure, bycopy.} = object
    protocols*: ptr[ptr[CXIdxObjCProtocolRefInfo]]
    numProtocols*: cuint

type
  CXIdxObjCInterfaceDeclInfo* {.pure, bycopy.} = object
    containerInfo*: ptr[CXIdxObjCContainerDeclInfo]
    superInfo*: ptr[CXIdxBaseClassInfo]
    protocols*: ptr[CXIdxObjCProtocolRefListInfo]

type
  CXIdxObjCCategoryDeclInfo* {.pure, bycopy.} = object
    containerInfo*: ptr[CXIdxObjCContainerDeclInfo]
    objcClass*: ptr[CXIdxEntityInfo]
    classCursor*: CXCursor
    classLoc*: CXIdxLoc
    protocols*: ptr[CXIdxObjCProtocolRefListInfo]

type
  CXIdxObjCPropertyDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[CXIdxDeclInfo]
    getter*: ptr[CXIdxEntityInfo]
    setter*: ptr[CXIdxEntityInfo]

type
  CXIdxClassDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[CXIdxDeclInfo]
    bases*: ptr[ptr[CXIdxBaseClassInfo]]
    numBases*: cuint

type
  CXIdxEntityRefKind* = enum ##  Data for IndexerCallbacks#indexEntityReference.
                          ##  This may be deprecated in a future version as this duplicates
                          ##  the
                          ##  bit in
    ierkDirect = 1,             ##  The entity is referenced directly in user's code.
    ierkImplicit = 2 ##  An implicit reference, e.g. a reference of an Objective-C method
                  ##  via the dot syntax.
type
  CXSymbolRole* = enum ##  Roles that are attributed to symbol occurrences.
                    ##  Internal: this currently mirrors low 9 bits of clang::index::SymbolRole with
                    ##  higher bits zeroed. These high bits may be exposed in the future.
    srNone = 0, srDeclaration = 1, srDefinition = 2, srReference = 4, srRead = 8, srWrite = 16,
    srCall = 32, srDynamic = 64, srAddressOf = 128, srImplicit = 256
type
  CXIdxEntityRefInfo* {.pure, bycopy.} = object
    kind*: CXIdxEntityRefKind
    cursor*: CXCursor
    loc*: CXIdxLoc
    referencedEntity*: ptr[CXIdxEntityInfo]
    parentEntity*: ptr[CXIdxEntityInfo]
    container*: ptr[CXIdxContainerInfo]
    role*: CXSymbolRole

type
  IndexerCallbacks* {.pure, bycopy.} = object
    abortQuery*: proc (a0: CXClientData; a1: pointer): cint {.cdecl.}
    diagnostic*: proc (a0: CXClientData; a1: CXDiagnosticSet; a2: pointer): void {.cdecl.}
    enteredMainFile*: proc (a0: CXClientData; a1: CXFile; a2: pointer): CXIdxClientFile {.
        cdecl.}
    ppIncludedFile*: proc (a0: CXClientData; a1: ptr[CXIdxIncludedFileInfo]): CXIdxClientFile {.
        cdecl.}
    importedASTFile*: proc (a0: CXClientData; a1: ptr[CXIdxImportedASTFileInfo]): CXIdxClientASTFile {.
        cdecl.}
    startedTranslationUnit*: proc (a0: CXClientData; a1: pointer): CXIdxClientContainer {.
        cdecl.}
    indexDeclaration*: proc (a0: CXClientData; a1: ptr[CXIdxDeclInfo]): void {.cdecl.}
    indexEntityReference*: proc (a0: CXClientData; a1: ptr[CXIdxEntityRefInfo]): void {.
        cdecl.}

proc index_isEntityObjCContainerKind*(argCXIdxEntityKind: CXIdxEntityKind): cint {.
    cdecl, dynlib: libclang, importc: "clang_index_isEntityObjCContainerKind".}
proc index_getObjCContainerDeclInfo*(argCXIdxDeclInfo: ptr[CXIdxDeclInfo]): ptr[
    CXIdxObjCContainerDeclInfo] {.cdecl, dynlib: libclang, importc: "clang_index_getObjCContainerDeclInfo".}
proc index_getObjCInterfaceDeclInfo*(argCXIdxDeclInfo: ptr[CXIdxDeclInfo]): ptr[
    CXIdxObjCInterfaceDeclInfo] {.cdecl, dynlib: libclang, importc: "clang_index_getObjCInterfaceDeclInfo".}
proc index_getObjCCategoryDeclInfo*(argCXIdxDeclInfo: ptr[CXIdxDeclInfo]): ptr[
    CXIdxObjCCategoryDeclInfo] {.cdecl, dynlib: libclang,
                                importc: "clang_index_getObjCCategoryDeclInfo".}
proc index_getObjCProtocolRefListInfo*(argCXIdxDeclInfo: ptr[CXIdxDeclInfo]): ptr[
    CXIdxObjCProtocolRefListInfo] {.cdecl, dynlib: libclang, importc: "clang_index_getObjCProtocolRefListInfo".}
proc index_getObjCPropertyDeclInfo*(argCXIdxDeclInfo: ptr[CXIdxDeclInfo]): ptr[
    CXIdxObjCPropertyDeclInfo] {.cdecl, dynlib: libclang,
                                importc: "clang_index_getObjCPropertyDeclInfo".}
proc index_getIBOutletCollectionAttrInfo*(argCXIdxAttrInfo: ptr[CXIdxAttrInfo]): ptr[
    CXIdxIBOutletCollectionAttrInfo] {.cdecl, dynlib: libclang, importc: "clang_index_getIBOutletCollectionAttrInfo".}
proc index_getClassDeclInfo*(argCXIdxDeclInfo: ptr[CXIdxDeclInfo]): ptr[
    CXIdxClassDeclInfo] {.cdecl, dynlib: libclang,
                            importc: "clang_index_getCXXClassDeclInfo".}
proc index_getClientContainer*(argCXIdxContainerInfo: ptr[CXIdxContainerInfo]): CXIdxClientContainer {.
    cdecl, dynlib: libclang, importc: "clang_index_getClientContainer".}
  ##  For retrieving a custom CXIdxClientContainer attached to a
  ##  container.
proc index_setClientContainer*(argCXIdxContainerInfo: ptr[CXIdxContainerInfo];
                              argCXIdxClientContainer: CXIdxClientContainer): void {.
    cdecl, dynlib: libclang, importc: "clang_index_setClientContainer".}
  ##  For setting a custom CXIdxClientContainer attached to a
  ##  container.
proc index_getClientEntity*(argCXIdxEntityInfo: ptr[CXIdxEntityInfo]): CXIdxClientEntity {.
    cdecl, dynlib: libclang, importc: "clang_index_getClientEntity".}
  ##  For retrieving a custom CXIdxClientEntity attached to an entity.
proc index_setClientEntity*(argCXIdxEntityInfo: ptr[CXIdxEntityInfo];
                           argCXIdxClientEntity: CXIdxClientEntity): void {.cdecl,
    dynlib: libclang, importc: "clang_index_setClientEntity".}
  ##  For setting a custom CXIdxClientEntity attached to an entity.
type
  CXIndexAction* = distinct pointer
proc indexAction_create*(cIdx: CXIndex): CXIndexAction {.cdecl, dynlib: libclang,
    importc: "clang_IndexAction_create".}
  ##  An indexing action/session, to be applied to one or multiple
  ##  translation units.
  ## **CIdx**
  ##  The index object with which the index action will be associated.
proc dispose*(argCXIndexAction: CXIndexAction): void {.cdecl, dynlib: libclang,
    importc: "clang_IndexAction_dispose".}
  ##  Destroy the given index action.
  ##  The index action must not be destroyed until all of the translation units
  ##  created within that index action have been destroyed.
type
  CXIndexOptFlags* = enum
    iofNone = 0,                ##  Used to indicate that no special indexing options are needed.
    iofSuppressRedundantRefs = 1, ##  Used to indicate that IndexerCallbacks#indexEntityReference should
                               ##  be invoked for only one reference of an entity per source file that does
                               ##  not also include a declaration/definition of the entity.
    iofIndexFunctionLocalSymbols = 2, ##  Function-local symbols should be indexed. If this is not set
                                   ##  function-local symbols will be ignored.
    iofIndexImplicitTemplateInstantiations = 4, ##  Implicit function/class template instantiations should be indexed.
                                             ##  If this is not set, implicit instantiations will be ignored.
    iofSuppressWarnings = 8,    ##  Suppress all compiler warnings when parsing for indexing.
    iofSkipParsedBodiesInSession = 16 ##  Skip a function/method body that was already parsed during an
                                   ##  indexing session associated with a
                                   ##  object.
                                   ##  Bodies in system headers are always skipped.
proc indexSourceFile*(argCXIndexAction: CXIndexAction; client_data: CXClientData;
                     index_callbacks: ptr[IndexerCallbacks];
                     index_callbacks_size: cuint; index_options: cuint;
                     source_filename: cstring; command_line_args: cstringArray;
                     num_command_line_args: cint;
                     unsaved_files: ptr[CXUnsavedFile]; num_unsaved_files: cuint;
                     out_TU: ptr[CXTranslationUnit]; tU_options: cuint): cint {.
    cdecl, dynlib: libclang, importc: "clang_indexSourceFile".}
  ##  Index the given source file and the translation unit corresponding
  ##  to that file via callbacks implemented through #IndexerCallbacks.
  ## **client_data**
  ##  pointer data supplied by the client, which will
  ##  be passed to the invoked callbacks.
  ## **index_callbacks**
  ##  Pointer to indexing callbacks that the client
  ##  implements.
  ## **index_callbacks_size**
  ##  Size of #IndexerCallbacks structure that gets
  ##  passed in index_callbacks.
  ## **index_options**
  ##  A bitmask of options that affects how indexing is
  ##  performed. This should be a bitwise OR of the CXIndexOpt_XXX flags.
  ## **out_TU**
  ##  pointer to store a
  ##  that can be
  ##  reused after indexing is finished. Set to
  ##  if you do not require it.
  ## **
  ##  0 on success or if there were errors from which the compiler could
  ##  recover.  If there is a failure from which there is no recovery, returns
  ##  a non-zero
  ##  The rest of the parameters are the same as #clang_parseTranslationUnit.
proc indexSourceFileFullArgv*(argCXIndexAction: CXIndexAction;
                             client_data: CXClientData;
                             index_callbacks: ptr[IndexerCallbacks];
                             index_callbacks_size: cuint; index_options: cuint;
                             source_filename: cstring;
                             command_line_args: cstringArray;
                             num_command_line_args: cint;
                             unsaved_files: ptr[CXUnsavedFile];
                             num_unsaved_files: cuint;
                             out_TU: ptr[CXTranslationUnit]; tU_options: cuint): cint {.
    cdecl, dynlib: libclang, importc: "clang_indexSourceFileFullArgv".}
  ##  Same as clang_indexSourceFile but requires a full command line
  ##  for
  ##  including argv[0]. This is useful if the standard
  ##  library paths are relative to the binary.
proc indexTranslationUnit*(argCXIndexAction: CXIndexAction;
                          client_data: CXClientData;
                          index_callbacks: ptr[IndexerCallbacks];
                          index_callbacks_size: cuint; index_options: cuint;
                          argCXTranslationUnit: CXTranslationUnit): cint {.cdecl,
    dynlib: libclang, importc: "clang_indexTranslationUnit".}
  ##  Index the given translation unit via callbacks implemented through
  ##  #IndexerCallbacks.
  ##  The order of callback invocations is not guaranteed to be the same as
  ##  when indexing a source file. The high level order will be:
  ##    -Preprocessor callbacks invocations
  ##    -Declaration/reference callbacks invocations
  ##    -Diagnostic callback invocations
  ##  The parameters are the same as #clang_indexSourceFile.
  ## **
  ##  If there is a failure from which there is no recovery, returns
  ##  non-zero, otherwise returns 0.
proc indexLoc_getFileLocation*(loc: CXIdxLoc; indexFile: ptr[CXIdxClientFile];
                              file: ptr[CXFile]; line: ptr[cuint];
                              column: ptr[cuint]; offset: ptr[cuint]): void {.cdecl,
    dynlib: libclang, importc: "clang_indexLoc_getFileLocation".}
  ##  Retrieve the CXIdxFile, file, line, column, and offset represented by
  ##  the given CXIdxLoc.
  ##  If the location refers into a macro expansion, retrieves the
  ##  location of the macro expansion and if it refers into a macro argument
  ##  retrieves the location of the argument.
proc indexLoc_getCXSourceLocation*(loc: CXIdxLoc): CXSourceLocation {.cdecl,
    dynlib: libclang, importc: "clang_indexLoc_getCXSourceLocation".}
  ##  Retrieve the CXSourceLocation represented by the given CXIdxLoc.
type
  CXFieldVisitor* = distinct proc (a0: CXCursor; a1: pointer): CXVisitorResult {.cdecl.}
proc visitFields*(t: CXType; visitor: CXFieldVisitor; client_data: CXClientData): cuint {.
    cdecl, dynlib: libclang, importc: "clang_Type_visitFields".}
  ##  Visit the fields of a particular type.
  ##  This function visits all the direct fields of the given cursor,
  ##  invoking the given
  ##  function with the cursors of each
  ##  visited field. The traversal may be ended prematurely, if
  ##  the visitor returns
  ## **T**
  ##  the record type whose field may be visited.
  ## **visitor**
  ##  the visitor function that will be invoked for each
  ##  field of
  ## **client_data**
  ##  pointer data supplied by the client, which will
  ##  be passed to the visitor each time it is invoked.
  ## **
  ##  a non-zero value if the traversal was terminated
  ##  prematurely by the visitor returning
type
  CXComment* {.pure, bycopy.} = object
    aSTNode*: pointer
    translationUnit*: CXTranslationUnit

proc getParsedComment*(c: CXCursor): CXComment {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getParsedComment".}
  ##  Given a cursor that represents a documentable entity (e.g.,
  ##  declaration), return the associated parsed comment as a
  ##  AST node.
type
  CXCommentKind* = enum ##  Describes the type of the comment AST node (
                     ##   A comment
                     ##  node can be considered block content (e. g., paragraph), inline content
                     ##  (plain text) or neither (the root AST node).
    cokNull = 0, ##  Null comment.  No AST node is constructed at the requested location
              ##  because there is no text or a syntax error.
    cokText = 1,                ##  Plain text.  Inline content.
    cokInlineCommand = 2, ##  A command with word-like arguments that is considered inline content.
                       ##  For example:
                       ## \\
                       ## c command.
    cokHTMLStartTag = 3, ##  HTML start tag with attributes (name-value pairs).  Considered
                      ##  inline content.
                      ##  For example:
                      ## Error: cannot render: rnCodeBlock
    cokHTMLEndTag = 4,          ##  HTML end tag.  Considered inline content.
                    ##  For example:
                    ## Error: cannot render: rnCodeBlock
    cokParagraph = 5, ##  A paragraph, contains inline comment.  The paragraph itself is
                   ##  block content.
    cokBlockCommand = 6, ##  A command that has zero or more word-like arguments (number of
                      ##  word-like arguments depends on command name) and a paragraph as an
                      ##  argument.  Block command is block content.
                      ##  Paragraph argument is also a child of the block command.
                      ##  For example:
                      ##  0 word-like arguments and a paragraph argument.
                      ##  AST nodes of special kinds that parser knows about (e. g.,
                      ## \\
                      ## param
                      ##  command) have their own node kinds.
    cokParamCommand = 7,        ##  A
                      ## \\
                      ## param or
                      ## \\
                      ## arg command that describes the function parameter
                      ##  (name, passing direction, description).
                      ##  For example:
                      ## \\
                      ## param [in] ParamName description.
    cokTParamCommand = 8, ##  A
                       ## \\
                       ## tparam command that describes a template parameter (name and
                       ##  description).
                       ##  For example:
                       ## \\
                       ## tparam T description.
    cokVerbatimBlockCommand = 9, ##  A verbatim block command (e. g., preformatted code).  Verbatim
                              ##  block has an opening and a closing command and contains multiple lines of
                              ##  text (
                              ##  child nodes).
                              ##  For example:
                              ## \\
                              ## verbatim
                              ##  aaa
                              ## \\
                              ## endverbatim
    cokVerbatimBlockLine = 10,  ##  A line of text that is contained within a
                            ##  CXComment_VerbatimBlockCommand node.
    cokVerbatimLine = 11, ##  A verbatim line command.  Verbatim line has an opening command,
                       ##  a single line of text (up to the newline after the opening command) and
                       ##  has no closing command.
    cokFullComment = 12         ##  A full comment attached to a declaration, contains block content.
type
  CXCommentInlineCommandRenderKind* = enum ##  The most appropriate rendering mode for an inline command, chosen on
                                        ##  command semantics in Doxygen.
    cicrkNormal,              ##  Command argument should be rendered in a normal font.
    cicrkBold,                ##  Command argument should be rendered in a bold font.
    cicrkMonospaced,          ##  Command argument should be rendered in a monospaced font.
    cicrkEmphasized, ##  Command argument should be rendered emphasized (typically italic
                    ##  font).
    cicrkAnchor               ##  Command argument should not be rendered (since it only defines an anchor).
type
  CXCommentParamPassDirection* = enum ##  Describes parameter passing direction for
                                   ## \\
                                   ## param or
                                   ## \\
                                   ## arg command.
    cppdIn,                   ##  The parameter is an input parameter.
    cppdOut,                  ##  The parameter is an output parameter.
    cppdInOut                 ##  The parameter is an input and output parameter.
proc getKind*(comment: CXComment): CXCommentKind {.cdecl, dynlib: libclang,
    importc: "clang_Comment_getKind".}
  ## **Comment**
  ##  AST node of any kind.
  ## **
  ##  the type of the AST node.
proc getNumChildren*(comment: CXComment): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Comment_getNumChildren".}
  ## **Comment**
  ##  AST node of any kind.
  ## **
  ##  number of children of the AST node.
proc getChild*(comment: CXComment; childIdx: cuint): CXComment {.cdecl,
    dynlib: libclang, importc: "clang_Comment_getChild".}
  ## **Comment**
  ##  AST node of any kind.
  ## **ChildIdx**
  ##  child index (zero-based).
  ## **
  ##  the specified child of the AST node.
proc isWhitespace*(comment: CXComment): cuint {.cdecl, dynlib: libclang,
    importc: "clang_Comment_isWhitespace".}
  ##  A
  ##  node is considered whitespace if it contains
  ##  only
  ##  nodes that are empty or whitespace.
  ##  Other AST nodes (except
  ##  and
  ##  are
  ##  never considered whitespace.
  ## **
  ##  non-zero if
  ##  is whitespace.
proc inlineContentComment_hasTrailingNewline*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_InlineContentComment_hasTrailingNewline".}
  ## **
  ##  non-zero if
  ##  is inline content and has a newline
  ##  immediately following it in the comment text.  Newlines between paragraphs
  ##  do not count.
proc textComment_getText*(comment: CXComment): CXString {.cdecl, dynlib: libclang,
    importc: "clang_TextComment_getText".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  text contained in the AST node.
proc inlineCommandComment_getCommandName*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_InlineCommandComment_getCommandName".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  name of the inline command.
proc inlineCommandComment_getRenderKind*(comment: CXComment): CXCommentInlineCommandRenderKind {.
    cdecl, dynlib: libclang, importc: "clang_InlineCommandComment_getRenderKind".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  the most appropriate rendering mode, chosen on command
  ##  semantics in Doxygen.
proc inlineCommandComment_getNumArgs*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_InlineCommandComment_getNumArgs".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  number of command arguments.
proc inlineCommandComment_getArgText*(comment: CXComment; argIdx: cuint): CXString {.
    cdecl, dynlib: libclang, importc: "clang_InlineCommandComment_getArgText".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **ArgIdx**
  ##  argument index (zero-based).
  ## **
  ##  text of the specified argument.
proc hTMLTagComment_getTagName*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_HTMLTagComment_getTagName".}
  ## **Comment**
  ##  a
  ##  or
  ##  AST
  ##  node.
  ## **
  ##  HTML tag name.
proc hTMLStartTagComment_isSelfClosing*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_HTMLStartTagComment_isSelfClosing".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  non-zero if tag is self-closing (for example,
  ## <
  ## br /
  ## >
  ## ).
proc hTMLStartTag_getNumAttrs*(comment: CXComment): cuint {.cdecl, dynlib: libclang,
    importc: "clang_HTMLStartTag_getNumAttrs".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  number of attributes (name-value pairs) attached to the start tag.
proc hTMLStartTag_getAttrName*(comment: CXComment; attrIdx: cuint): CXString {.cdecl,
    dynlib: libclang, importc: "clang_HTMLStartTag_getAttrName".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **AttrIdx**
  ##  attribute index (zero-based).
  ## **
  ##  name of the specified attribute.
proc hTMLStartTag_getAttrValue*(comment: CXComment; attrIdx: cuint): CXString {.cdecl,
    dynlib: libclang, importc: "clang_HTMLStartTag_getAttrValue".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **AttrIdx**
  ##  attribute index (zero-based).
  ## **
  ##  value of the specified attribute.
proc blockCommandComment_getCommandName*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_BlockCommandComment_getCommandName".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  name of the block command.
proc blockCommandComment_getNumArgs*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_BlockCommandComment_getNumArgs".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  number of word-like arguments.
proc blockCommandComment_getArgText*(comment: CXComment; argIdx: cuint): CXString {.
    cdecl, dynlib: libclang, importc: "clang_BlockCommandComment_getArgText".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **ArgIdx**
  ##  argument index (zero-based).
  ## **
  ##  text of the specified word-like argument.
proc blockCommandComment_getParagraph*(comment: CXComment): CXComment {.cdecl,
    dynlib: libclang, importc: "clang_BlockCommandComment_getParagraph".}
  ## **Comment**
  ##  a
  ##  or
  ##  AST node.
  ## **
  ##  paragraph argument of the block command.
proc paramCommandComment_getParamName*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_getParamName".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  parameter name.
proc paramCommandComment_isParamIndexValid*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_isParamIndexValid".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  non-zero if the parameter that this AST node represents was found
  ##  in the function prototype and
  ##  function will return a meaningful value.
proc paramCommandComment_getParamIndex*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_getParamIndex".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  zero-based parameter index in function prototype.
proc paramCommandComment_isDirectionExplicit*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_isDirectionExplicit".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  non-zero if parameter passing direction was specified explicitly in
  ##  the comment.
proc paramCommandComment_getDirection*(comment: CXComment): CXCommentParamPassDirection {.
    cdecl, dynlib: libclang, importc: "clang_ParamCommandComment_getDirection".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  parameter passing direction.
proc tParamCommandComment_getParamName*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_getParamName".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  template parameter name.
proc tParamCommandComment_isParamPositionValid*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_isParamPositionValid".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  non-zero if the parameter that this AST node represents was found
  ##  in the template parameter list and
  ##  and
  ##  functions will return a meaningful
  ##  value.
proc tParamCommandComment_getDepth*(comment: CXComment): cuint {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_getDepth".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  zero-based nesting depth of this parameter in the template parameter list.
  ##  For example,
  ## Error: cannot render: rnCodeBlock
  ##  for C and TT nesting depth is 0,
  ##  for T nesting depth is 1.
proc tParamCommandComment_getIndex*(comment: CXComment; depth: cuint): cuint {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_getIndex".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  zero-based parameter index in the template parameter list at a
  ##  given nesting depth.
  ##  For example,
  ## Error: cannot render: rnCodeBlock
  ##  for C and TT nesting depth is 0, so we can ask for index at depth 0:
  ##  at depth 0 C's index is 0, TT's index is 1.
  ##  For T nesting depth is 1, so we can ask for index at depth 0 and 1:
  ##  at depth 0 T's index is 1 (same as TT's),
  ##  at depth 1 T's index is 0.
proc verbatimBlockLineComment_getText*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_VerbatimBlockLineComment_getText".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  text contained in the AST node.
proc verbatimLineComment_getText*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_VerbatimLineComment_getText".}
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  text contained in the AST node.
proc hTMLTagComment_getAsString*(comment: CXComment): CXString {.cdecl,
    dynlib: libclang, importc: "clang_HTMLTagComment_getAsString".}
  ##  Convert an HTML tag AST node to string.
  ## **Comment**
  ##  a
  ##  or
  ##  AST
  ##  node.
  ## **
  ##  string containing an HTML tag.
proc fullComment_getAsHTML*(comment: CXComment): CXString {.cdecl, dynlib: libclang,
    importc: "clang_FullComment_getAsHTML".}
  ##  Convert a given full parsed comment to an HTML fragment.
  ##  Specific details of HTML layout are subject to change.  Don't try to parse
  ##  this HTML back into an AST, use other APIs instead.
  ##  Currently the following CSS classes are used:
  ## **
  ##  "para-brief" for
  ## ``
  ## **
  ##  "para-returns" for
  ## \\
  ## returns paragraph and equivalent commands;
  ## **
  ##  "word-returns" for the "Returns" word in
  ## \\
  ## returns paragraph.
  ##  Function argument documentation is rendered as a
  ## <
  ## dl
  ## >
  ##  list with arguments
  ##  sorted in function prototype order.  CSS classes used:
  ## **
  ##  "param-name-index-NUMBER" for parameter name (
  ## <
  ## dt
  ## >
  ## );
  ## **
  ##  "param-descr-index-NUMBER" for parameter description (
  ## <
  ## dd
  ## >
  ## );
  ## **
  ##  "param-name-index-invalid" and "param-descr-index-invalid" are used if
  ##  parameter index is invalid.
  ##  Template parameter documentation is rendered as a
  ## <
  ## dl
  ## >
  ##  list with
  ##  parameters sorted in template parameter list order.  CSS classes used:
  ## **
  ##  "tparam-name-index-NUMBER" for parameter name (
  ## <
  ## dt
  ## >
  ## );
  ## **
  ##  "tparam-descr-index-NUMBER" for parameter description (
  ## <
  ## dd
  ## >
  ## );
  ## **
  ##  "tparam-name-index-other" and "tparam-descr-index-other" are used for
  ##  names inside template template parameters;
  ## **
  ##  "tparam-name-index-invalid" and "tparam-descr-index-invalid" are used if
  ##  parameter position is invalid.
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  string containing an HTML fragment.
proc fullComment_getAsXML*(comment: CXComment): CXString {.cdecl, dynlib: libclang,
    importc: "clang_FullComment_getAsXML".}
  ##  Convert a given full parsed comment to an XML document.
  ##  A Relax NG schema for the XML can be found in comment-xml-schema.rng file
  ##  inside clang source tree.
  ## **Comment**
  ##  a
  ##  AST node.
  ## **
  ##  string containing an XML document.
