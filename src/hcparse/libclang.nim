
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
  CXTargetInfoImpl* = object
  CXTranslationUnitImpl* = object
  CXVirtualFileOverlayImpl* = object
  CXModuleMapDescriptorImpl* = object
  time_t* = clong
proc install_aborting_llvm_fatal_error_handler*() {.cdecl, dynlib: libclang,
    importc: "clang_install_aborting_llvm_fatal_error_handler".}
  ## 
  ## 
  ##  Installs error handler that prints error message to stderr and calls abort(). Replaces currently installed error handler (if any).
proc uninstall_llvm_fatal_error_handler*() {.cdecl, dynlib: libclang,
    importc: "clang_uninstall_llvm_fatal_error_handler".}
  ## 
  ## 
  ##  Removes currently installed error handler (if any). If no error handler is intalled, the default strategy is to print error message to stderr and call exit(1).
type
  CXString = object
    data*: pointer
    private_flags*: cuint

type
  CXStringSet = object
    strings*: ptr[CXString]
    count*: cuint

proc getCString*(cxstring: CXString) {.cdecl, dynlib: libclang,
                                    importc: "clang_getCString".}
  ## 
  ## 
  ##  Retrieve the character data associated with the given string.
proc disposeString*(cxstring: CXString) {.cdecl, dynlib: libclang,
                                       importc: "clang_disposeString".}
  ## 
  ## 
  ##  Free the given string.
proc disposeStringSet*(cxset: ptr[CXStringSet]) {.cdecl, dynlib: libclang,
    importc: "clang_disposeStringSet".}
  ## 
  ## 
  ##  Free the given string set.
type
  CXIndex = distinct pointer
type
  CXTargetInfo = distinct ptr[CXTargetInfoImpl]
type
  CXTranslationUnit = distinct ptr[CXTranslationUnitImpl]
type
  CXClientData = distinct pointer
type
  CXVersion = object
    major*: int
    minor*: int
    subminor*: int

proc createIndex*(excludeDeclarationsFromPCH: int; displayDiagnostics: int) {.cdecl,
    dynlib: libclang, importc: "clang_createIndex".}
  ## 
  ## 
  ##  Provides a shared context for creating translation units.
  ## 
  ##  It provides two options:
  ## 
  ##  - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local" declarations (when loading any new translation units). A "local" declaration is one that belongs in the translation unit itself and not in a precompiled header that was used by the translation unit. If zero, all declarations will be enumerated.
  ## 
  ##  Here is an example:
  ## 
  ##  Error: cannot render: rnCodeBlock
  ## 
  ##  This process of creating the 'pch', loading it separately, and using it (via -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks (which gives the indexer the same performance benefit as the compiler).
proc disposeIndex*(index: CXIndex) {.cdecl, dynlib: libclang,
                                  importc: "clang_disposeIndex".}
  ## 
  ## 
  ##  Destroy the given index.
  ## 
  ##  The index must not be destroyed until all of the translation units created within that index have been destroyed.
type
  CXGlobalOptFlags = enum
    gofNone = 0,                ## 
              ## 
              ##  Used to indicate that no special CXIndex options are needed.
    gofThreadBackgroundPriorityForIndexing = 1, ## 
                                             ## 
                                             ##  Used to indicate that threads that libclang creates for indexing purposes should use background priority.
                                             ## 
                                             ##  Affects #clang_indexSourceFile, #clang_indexTranslationUnit, #clang_parseTranslationUnit, #clang_saveTranslationUnit.
    gofThreadBackgroundPriorityForEditing = 2, ## 
                                            ## 
                                            ##  Used to indicate that threads that libclang creates for editing purposes should use background priority.
                                            ## 
                                            ##  Affects #clang_reparseTranslationUnit, #clang_codeCompleteAt, #clang_annotateTokens
    gofThreadBackgroundPriorityForAll ## 
                                     ## 
                                     ##  Used to indicate that all threads that libclang creates should use background priority.
proc setGlobalOptions*(argCXIndex: CXIndex; options: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_CXIndex_setGlobalOptions".}
  ## 
  ## 
  ##  Sets general options associated with a CXIndex.
  ## 
  ##  For example: Error: cannot render: rnCodeBlock
  ## 
  ##  **
  ## 
  ##  A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
proc getGlobalOptions*(argCXIndex: CXIndex) {.cdecl, dynlib: libclang,
    importc: "clang_CXIndex_getGlobalOptions".}
  ## 
  ## 
  ##  Gets the general options associated with a CXIndex.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that are associated with the given CXIndex object.
proc setInvocationEmissionPathOption*(argCXIndex: CXIndex; path: cstring) {.cdecl,
    dynlib: libclang, importc: "clang_CXIndex_setInvocationEmissionPathOption".}
  ## 
  ## 
  ##  Sets the invocation emission path option in a CXIndex.
  ## 
  ##  The invocation emission path specifies a path which will contain log files for certain libclang invocations. A null value (default) implies that libclang invocations are not logged..
type
  CXFile = distinct pointer
proc getFileName*(sFile: CXFile) {.cdecl, dynlib: libclang,
                                importc: "clang_getFileName".}
  ## 
  ## 
  ##  Retrieve the complete file and path name of the given file.
proc getFileTime*(sFile: CXFile) {.cdecl, dynlib: libclang,
                                importc: "clang_getFileTime".}
  ## 
  ## 
  ##  Retrieve the last modification time of the given file.
type
  CXFileUniqueID = object
    data*: array[3, culonglong]

proc getFileUniqueID*(file: CXFile; outID: ptr[CXFileUniqueID]) {.cdecl,
    dynlib: libclang, importc: "clang_getFileUniqueID".}
  ## 
  ## 
  ##  Retrieve the unique ID for the given Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  the file to get the ID for. **
  ## 
  ##  stores the returned CXFileUniqueID. 
  ## 
  ## **
  ## 
  ##  If there was a failure getting the unique ID, returns non-zero, otherwise returns 0.
proc isFileMultipleIncludeGuarded*(tu: CXTranslationUnit; file: CXFile) {.cdecl,
    dynlib: libclang, importc: "clang_isFileMultipleIncludeGuarded".}
  ## 
  ## 
  ##  Determine whether the given header is guarded against multiple inclusions, either with the conventional #ifndef/#define/#endif macro guards or with #pragma once.
proc getFile*(tu: CXTranslationUnit; file_name: cstring) {.cdecl, dynlib: libclang,
    importc: "clang_getFile".}
  ## 
  ## 
  ##  Retrieve a file handle within the given translation unit.
  ## 
  ##  **
  ## 
  ##  the translation unit
  ## 
  ##  **
  ## 
  ##  the name of the file.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the file handle for the named file in the translation unit Error: cannot render: rnLiteralBlock or a NULL file handle if the file was not a part of this translation unit.
proc getFileContents*(tu: CXTranslationUnit; file: CXFile; size: ptr[int]) {.cdecl,
    dynlib: libclang, importc: "clang_getFileContents".}
proc isEqual*(file1: CXFile; file2: CXFile) {.cdecl, dynlib: libclang,
    importc: "clang_File_isEqual".}
  ## 
  ## 
  ##  Returns non-zero if the Error: cannot render: rnLiteralBlock and Error: cannot render: rnLiteralBlock point to the same file, or they are both NULL.
proc tryGetRealPathName*(file: CXFile) {.cdecl, dynlib: libclang,
                                      importc: "clang_File_tryGetRealPathName".}
  ## 
  ## 
  ##  Returns the real path name of Error: cannot render: rnLiteralBlock
  ## 
  ##  An empty string may be returned. Use Error: cannot render: rnLiteralBlock in that case.
type
  CXSourceLocation = object
    ptr_data*: array[2, pointer]
    int_data*: cuint

type
  CXSourceRange = object
    ptr_data*: array[2, pointer]
    begin_int_data*: cuint
    end_int_data*: cuint

proc getNullLocation*() {.cdecl, dynlib: libclang, importc: "clang_getNullLocation".}
  ## 
  ## 
  ##  Retrieve a NULL (invalid) source location.
proc equalLocations*(loc1: CXSourceLocation; loc2: CXSourceLocation) {.cdecl,
    dynlib: libclang, importc: "clang_equalLocations".}
  ## 
  ## 
  ##  Determine whether two source locations, which must refer into the same translation unit, refer to exactly the same point in the source code.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the source locations refer to the same location, zero if they refer to different locations.
proc getLocation*(tu: CXTranslationUnit; file: CXFile; line: cuint; column: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_getLocation".}
  ## 
  ## 
  ##  Retrieves the source location associated with a given file/line/column in a particular translation unit.
proc getLocationForOffset*(tu: CXTranslationUnit; file: CXFile; offset: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_getLocationForOffset".}
  ## 
  ## 
  ##  Retrieves the source location associated with a given character offset in a particular translation unit.
proc Location_isInSystemHeader*(location: CXSourceLocation) {.cdecl,
    dynlib: libclang, importc: "clang_Location_isInSystemHeader".}
  ## 
  ## 
  ##  Returns non-zero if the given source location is in a system header.
proc Location_isFromMainFile*(location: CXSourceLocation) {.cdecl, dynlib: libclang,
    importc: "clang_Location_isFromMainFile".}
  ## 
  ## 
  ##  Returns non-zero if the given source location is in the main file of the corresponding translation unit.
proc getNullRange*() {.cdecl, dynlib: libclang, importc: "clang_getNullRange".}
  ## 
  ## 
  ##  Retrieve a NULL (invalid) source range.
proc getRange*(cxbegin: CXSourceLocation; cxend: CXSourceLocation) {.cdecl,
    dynlib: libclang, importc: "clang_getRange".}
  ## 
  ## 
  ##  Retrieve a source range given the beginning and ending source locations.
proc equalRanges*(range1: CXSourceRange; range2: CXSourceRange) {.cdecl,
    dynlib: libclang, importc: "clang_equalRanges".}
  ## 
  ## 
  ##  Determine whether two ranges are equivalent.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the ranges are the same, zero if they differ.
proc Range_isNull*(cxrange: CXSourceRange) {.cdecl, dynlib: libclang,
    importc: "clang_Range_isNull".}
  ## 
  ## 
  ##  Returns non-zero if Error: cannot render: rnLiteralBlock is null.
proc getExpansionLocation*(location: CXSourceLocation; file: ptr[CXFile];
                          line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]) {.
    cdecl, dynlib: libclang, importc: "clang_getExpansionLocation".}
  ## 
  ## 
  ##  Retrieve the file, line, column, and offset represented by the given source location.
  ## 
  ##  If the location refers into a macro expansion, retrieves the location of the macro expansion.
  ## 
  ##  **
  ## 
  ##  the location within a source file that will be decomposed into its parts.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the file to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the line to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the column to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
proc getPresumedLocation*(location: CXSourceLocation; filename: ptr[CXString];
                         line: ptr[cuint]; column: ptr[cuint]) {.cdecl,
    dynlib: libclang, importc: "clang_getPresumedLocation".}
  ## 
  ## 
  ##  Retrieve the file, line and column represented by the given source location, as specified in a # line directive.
  ## 
  ##  Example: given the following source code in a file somefile.c
  ## 
  ##  Error: cannot render: rnCodeBlock
  ## 
  ##  the location information returned by this function would be
  ## 
  ##  File: dummy.c Line: 124 Column: 12
  ## 
  ##  whereas clang_getExpansionLocation would have returned
  ## 
  ##  File: somefile.c Line: 3 Column: 12
  ## 
  ##  **
  ## 
  ##  the location within a source file that will be decomposed into its parts.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the filename of the source location. Note that filenames returned will be for "virtual" files, which don't necessarily exist on the machine running clang - e.g. when parsing preprocessed output obtained from a different environment. If a non-NULL value is passed in, remember to dispose of the returned value using Error: cannot render: rnLiteralBlock once you've finished with it. For an invalid source location, an empty string is returned.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the line number of the source location. For an invalid source location, zero is returned.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the column number of the source location. For an invalid source location, zero is returned.
proc getInstantiationLocation*(location: CXSourceLocation; file: ptr[CXFile];
                              line: ptr[cuint]; column: ptr[cuint];
                              offset: ptr[cuint]) {.cdecl, dynlib: libclang,
    importc: "clang_getInstantiationLocation".}
  ## 
  ## 
  ##  Legacy API to retrieve the file, line, column, and offset represented by the given source location.
  ## 
  ##  This interface has been replaced by the newer interface #clang_getExpansionLocation(). See that interface's documentation for details.
proc getSpellingLocation*(location: CXSourceLocation; file: ptr[CXFile];
                         line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]) {.
    cdecl, dynlib: libclang, importc: "clang_getSpellingLocation".}
  ## 
  ## 
  ##  Retrieve the file, line, column, and offset represented by the given source location.
  ## 
  ##  If the location refers into a macro instantiation, return where the location was originally spelled in the source file.
  ## 
  ##  **
  ## 
  ##  the location within a source file that will be decomposed into its parts.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the file to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the line to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the column to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
proc getFileLocation*(location: CXSourceLocation; file: ptr[CXFile];
                     line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]) {.
    cdecl, dynlib: libclang, importc: "clang_getFileLocation".}
  ## 
  ## 
  ##  Retrieve the file, line, column, and offset represented by the given source location.
  ## 
  ##  If the location refers into a macro expansion, return where the macro was expanded or where the macro argument was written, if the location points at a macro argument.
  ## 
  ##  **
  ## 
  ##  the location within a source file that will be decomposed into its parts.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the file to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the line to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the column to which the given source location points.
  ## 
  ##  **
  ## 
  ##  [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
proc getRangeStart*(cxrange: CXSourceRange) {.cdecl, dynlib: libclang,
    importc: "clang_getRangeStart".}
  ## 
  ## 
  ##  Retrieve a source location representing the first character within a source range.
proc getRangeEnd*(cxrange: CXSourceRange) {.cdecl, dynlib: libclang,
    importc: "clang_getRangeEnd".}
  ## 
  ## 
  ##  Retrieve a source location representing the last character within a source range.
type
  CXSourceRangeList = object
    count*: cuint
    ranges*: ptr[CXSourceRange]

proc getSkippedRanges*(tu: CXTranslationUnit; file: CXFile) {.cdecl, dynlib: libclang,
    importc: "clang_getSkippedRanges".}
  ## 
  ## 
  ##  Retrieve all ranges that were skipped by the preprocessor.
  ## 
  ##  The preprocessor will skip lines when they are surrounded by an if/ifdef/ifndef directive whose condition does not evaluate to true.
proc getAllSkippedRanges*(tu: CXTranslationUnit) {.cdecl, dynlib: libclang,
    importc: "clang_getAllSkippedRanges".}
  ## 
  ## 
  ##  Retrieve all ranges from all files that were skipped by the preprocessor.
  ## 
  ##  The preprocessor will skip lines when they are surrounded by an if/ifdef/ifndef directive whose condition does not evaluate to true.
proc disposeSourceRangeList*(ranges: ptr[CXSourceRangeList]) {.cdecl,
    dynlib: libclang, importc: "clang_disposeSourceRangeList".}
  ## 
  ## 
  ##  Destroy the given Error: cannot render: rnLiteralBlock 
type
  CXDiagnostic = distinct pointer
type
  CXDiagnosticSet = distinct pointer
proc getNumDiagnosticsInSet*(diags: CXDiagnosticSet) {.cdecl, dynlib: libclang,
    importc: "clang_getNumDiagnosticsInSet".}
  ## 
  ## 
  ##  Determine the number of diagnostics in a CXDiagnosticSet.
proc getDiagnosticInSet*(diags: CXDiagnosticSet; index: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticInSet".}
  ## 
  ## 
  ##  Retrieve a diagnostic associated with the given CXDiagnosticSet.
  ## 
  ##  **
  ## 
  ##  the CXDiagnosticSet to query. **
  ## 
  ##  the zero-based diagnostic number to retrieve.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
proc loadDiagnostics*(file: cstring; error: ptr[CXLoadDiag_Error];
                     errorString: ptr[CXString]) {.cdecl, dynlib: libclang,
    importc: "clang_loadDiagnostics".}
  ## 
  ## 
  ##  Deserialize a set of diagnostics from a Clang diagnostics bitcode file.
  ## 
  ##  **
  ## 
  ##  The name of the file to deserialize. **
  ## 
  ##  A pointer to a enum value recording if there was a problem        deserializing the diagnostics. **
  ## 
  ##  A pointer to a CXString for recording the error string        if the file was not successfully loaded.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A loaded CXDiagnosticSet if successful, and NULL otherwise.  These diagnostics should be released using clang_disposeDiagnosticSet().
proc disposeDiagnosticSet*(diags: CXDiagnosticSet) {.cdecl, dynlib: libclang,
    importc: "clang_disposeDiagnosticSet".}
  ## 
  ## 
  ##  Release a CXDiagnosticSet and all of its contained diagnostics.
proc getChildDiagnostics*(d: CXDiagnostic) {.cdecl, dynlib: libclang,
    importc: "clang_getChildDiagnostics".}
  ## 
  ## 
  ##  Retrieve the child diagnostics of a CXDiagnostic.
  ## 
  ##  This CXDiagnosticSet does not need to be released by clang_disposeDiagnosticSet.
proc getNumDiagnostics*(unit: CXTranslationUnit) {.cdecl, dynlib: libclang,
    importc: "clang_getNumDiagnostics".}
  ## 
  ## 
  ##  Determine the number of diagnostics produced for the given translation unit.
proc getDiagnostic*(unit: CXTranslationUnit; index: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getDiagnostic".}
  ## 
  ## 
  ##  Retrieve a diagnostic associated with the given translation unit.
  ## 
  ##  **
  ## 
  ##  the translation unit to query. **
  ## 
  ##  the zero-based diagnostic number to retrieve.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
proc getDiagnosticSetFromTU*(unit: CXTranslationUnit) {.cdecl, dynlib: libclang,
    importc: "clang_getDiagnosticSetFromTU".}
  ## 
  ## 
  ##  Retrieve the complete set of diagnostics associated with a        translation unit.
  ## 
  ##  **
  ## 
  ##  the translation unit to query.
proc disposeDiagnostic*(diagnostic: CXDiagnostic) {.cdecl, dynlib: libclang,
    importc: "clang_disposeDiagnostic".}
  ## 
  ## 
  ##  Destroy a diagnostic.
proc formatDiagnostic*(diagnostic: CXDiagnostic; options: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_formatDiagnostic".}
  ## 
  ## 
  ##  Format the given diagnostic in a manner that is suitable for display.
  ## 
  ##  This routine will format the given diagnostic to a string, rendering the diagnostic according to the various options given. The Error: cannot render: rnLiteralBlock function returns the set of options that most closely mimics the behavior of the clang compiler.
  ## 
  ##  **
  ## 
  ##  The diagnostic to print.
  ## 
  ##  **
  ## 
  ##  A set of options that control the diagnostic display, created by combining Error: cannot render: rnLiteralBlock values.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A new string containing for formatted diagnostic.
proc defaultDiagnosticDisplayOptions*() {.cdecl, dynlib: libclang, importc: "clang_defaultDiagnosticDisplayOptions".}
  ## 
  ## 
  ##  Retrieve the set of display options most similar to the default behavior of the clang compiler.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A set of display options suitable for use with Error: cannot render: rnLiteralBlock 
proc getDiagnosticSeverity*(argCXDiagnostic: CXDiagnostic) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticSeverity".}
  ## 
  ## 
  ##  Determine the severity of the given diagnostic.
proc getDiagnosticLocation*(argCXDiagnostic: CXDiagnostic) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticLocation".}
  ## 
  ## 
  ##  Retrieve the source location of the given diagnostic.
  ## 
  ##  This location is where Clang would print the caret ('^') when displaying the diagnostic on the command line.
proc getDiagnosticSpelling*(argCXDiagnostic: CXDiagnostic) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticSpelling".}
  ## 
  ## 
  ##  Retrieve the text of the given diagnostic.
proc getDiagnosticOption*(diag: CXDiagnostic; disable: ptr[CXString]) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticOption".}
  ## 
  ## 
  ##  Retrieve the name of the command-line option that enabled this diagnostic.
  ## 
  ##  **
  ## 
  ##  The diagnostic to be queried.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to the option that disables this diagnostic (if any).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A string that contains the command-line option used to enable this warning, such as "-Wconversion" or "-pedantic".
proc getDiagnosticCategory*(argCXDiagnostic: CXDiagnostic) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticCategory".}
  ## 
  ## 
  ##  Retrieve the category number for this diagnostic.
  ## 
  ##  Diagnostics can be categorized into groups along with other, related diagnostics (e.g., diagnostics under the same warning flag). This routine retrieves the category number for the given diagnostic.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The number of the category that contains this diagnostic, or zero if this diagnostic is uncategorized.
proc getDiagnosticCategoryName*(category: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getDiagnosticCategoryName".}
  ## 
  ## 
  ##  Retrieve the name of a particular diagnostic category.  This  is now deprecated.  Use clang_getDiagnosticCategoryText()  instead.
  ## 
  ##  **
  ## 
  ##  A diagnostic category number, as returned by Error: cannot render: rnLiteralBlock
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The name of the given diagnostic category.
proc getDiagnosticCategoryText*(argCXDiagnostic: CXDiagnostic) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticCategoryText".}
  ## 
  ## 
  ##  Retrieve the diagnostic category text for a given diagnostic.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The text of the given diagnostic category.
proc getDiagnosticNumRanges*(argCXDiagnostic: CXDiagnostic) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticNumRanges".}
  ## 
  ## 
  ##  Determine the number of source ranges associated with the given diagnostic.
proc getDiagnosticRange*(diagnostic: CXDiagnostic; cxrange: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticRange".}
  ## 
  ## 
  ##  Retrieve a source range associated with the diagnostic.
  ## 
  ##  A diagnostic's source ranges highlight important elements in the source code. On the command line, Clang displays source ranges by underlining them with '~' characters.
  ## 
  ##  **
  ## 
  ##  the diagnostic whose range is being extracted.
  ## 
  ##  **
  ## 
  ##  the zero-based index specifying which range to
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the requested source range.
proc getDiagnosticNumFixIts*(diagnostic: CXDiagnostic) {.cdecl, dynlib: libclang,
    importc: "clang_getDiagnosticNumFixIts".}
  ## 
  ## 
  ##  Determine the number of fix-it hints associated with the given diagnostic.
proc getDiagnosticFixIt*(diagnostic: CXDiagnostic; fixIt: cuint;
                        replacementRange: ptr[CXSourceRange]) {.cdecl,
    dynlib: libclang, importc: "clang_getDiagnosticFixIt".}
  ## 
  ## 
  ##  Retrieve the replacement information for a given fix-it.
  ## 
  ##  Fix-its are described in terms of a source range whose contents should be replaced by a string. This approach generalizes over three kinds of operations: removal of source code (the range covers the code to be removed and the replacement string is empty), replacement of source code (the range covers the code to be replaced and the replacement string provides the new code), and insertion (both the start and end of the range point at the insertion location, and the replacement string provides the text to insert).
  ## 
  ##  **
  ## 
  ##  The diagnostic whose fix-its are being queried.
  ## 
  ##  **
  ## 
  ##  The zero-based index of the fix-it.
  ## 
  ##  **
  ## 
  ##  The source range whose contents will be replaced with the returned replacement string. Note that source ranges are half-open ranges [a, b), so the source code should be replaced from a and up to (but not including) b.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A string containing text that should be replace the source code indicated by the Error: cannot render: rnLiteralBlock 
proc getTranslationUnitSpelling*(cTUnit: CXTranslationUnit) {.cdecl,
    dynlib: libclang, importc: "clang_getTranslationUnitSpelling".}
  ## 
  ## 
  ##  Get the original translation unit source file name.
proc createTranslationUnitFromSourceFile*(cIdx: CXIndex; source_filename: cstring;
    num_clang_command_line_args: int; clang_command_line_args: ptr[cstring];
    num_unsaved_files: cuint; unsaved_files: ptr[CXUnsavedFile]) {.cdecl,
    dynlib: libclang, importc: "clang_createTranslationUnitFromSourceFile".}
  ## 
  ## 
  ##  Return the CXTranslationUnit for a given source file and the provided command line arguments one would pass to the compiler.
  ## 
  ##  Note: The 'source_filename' argument is optional.  If the caller provides a NULL pointer, the name of the source file is expected to reside in the specified command line arguments.
  ## 
  ##  Note: When encountered in 'clang_command_line_args', the following options are ignored:
  ## 
  ##    '-c'   '-emit-ast'   '-fsyntax-only'   '-o <output file>'  (both '-o' and '<output file>' are ignored)
  ## 
  ##  **
  ## 
  ##  The index object with which the translation unit will be associated.
  ## 
  ##  **
  ## 
  ##  The name of the source file to load, or NULL if the source file is included in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  The number of command-line arguments in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  The command-line arguments that would be passed to the Error: cannot render: rnLiteralBlock executable if it were being invoked out-of-process. These command-line options will be parsed and will affect how the translation unit is parsed. Note that the following options are ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default), and '-o <output file>'.
  ## 
  ##  **
  ## 
  ##  the number of unsaved file entries in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  the files that have not yet been saved to disk but may be required for code completion, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns.
proc createTranslationUnit*(cIdx: CXIndex; ast_filename: cstring) {.cdecl,
    dynlib: libclang, importc: "clang_createTranslationUnit".}
  ## 
  ## 
  ##  Same as Error: cannot render: rnLiteralBlock but returns the Error: cannot render: rnLiteralBlock instead of an error code.  In case of an error this routine returns a Error: cannot render: rnLiteralBlock Error: cannot render: rnLiteralBlock without further detailed error codes.
proc createTranslationUnit2*(cIdx: CXIndex; ast_filename: cstring;
                            out_TU: ptr[CXTranslationUnit]) {.cdecl,
    dynlib: libclang, importc: "clang_createTranslationUnit2".}
  ## 
  ## 
  ##  Create a translation unit from an AST file (Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  A non-NULL pointer to store the created Error: cannot render: rnLiteralBlock
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  Zero on success, otherwise returns an error code.
proc defaultEditingTranslationUnitOptions*() {.cdecl, dynlib: libclang,
    importc: "clang_defaultEditingTranslationUnitOptions".}
  ## 
  ## 
  ##  Returns the set of flags that is suitable for parsing a translation unit that is being edited.
  ## 
  ##  The set of flags returned provide options for Error: cannot render: rnLiteralBlock to indicate that the translation unit is likely to be reparsed many times, either explicitly (via Error: cannot render: rnLiteralBlock or implicitly (e.g., by code completion (Error: cannot render: rnLiteralBlock The returned flag set contains an unspecified set of optimizations (e.g., the precompiled preamble) geared toward improving the performance of these routines. The set of optimizations enabled may change from one version to the next.
proc parseTranslationUnit*(cIdx: CXIndex; source_filename: cstring;
                          command_line_args: ptr[cstring];
                          num_command_line_args: int;
                          unsaved_files: ptr[CXUnsavedFile];
                          num_unsaved_files: cuint; options: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_parseTranslationUnit".}
  ## 
  ## 
  ##  Same as Error: cannot render: rnLiteralBlock but returns the Error: cannot render: rnLiteralBlock instead of an error code.  In case of an error this routine returns a Error: cannot render: rnLiteralBlock Error: cannot render: rnLiteralBlock without further detailed error codes.
proc parseTranslationUnit2*(cIdx: CXIndex; source_filename: cstring;
                           command_line_args: ptr[cstring];
                           num_command_line_args: int;
                           unsaved_files: ptr[CXUnsavedFile];
                           num_unsaved_files: cuint; options: cuint;
                           out_TU: ptr[CXTranslationUnit]) {.cdecl,
    dynlib: libclang, importc: "clang_parseTranslationUnit2".}
  ## 
  ## 
  ##  Parse the given source file and the translation unit corresponding to that file.
  ## 
  ##  This routine is the main entry point for the Clang C API, providing the ability to parse a source file into a translation unit that can then be queried by other functions in the API. This routine accepts a set of command-line arguments so that the compilation can be configured in the same way that the compiler is configured on the command line.
  ## 
  ##  **
  ## 
  ##  The index object with which the translation unit will be associated.
  ## 
  ##  **
  ## 
  ##  The name of the source file to load, or NULL if the source file is included in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  The command-line arguments that would be passed to the Error: cannot render: rnLiteralBlock executable if it were being invoked out-of-process. These command-line options will be parsed and will affect how the translation unit is parsed. Note that the following options are ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default), and '-o <output file>'.
  ## 
  ##  **
  ## 
  ##  The number of command-line arguments in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  the files that have not yet been saved to disk but may be required for parsing, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns.
  ## 
  ##  **
  ## 
  ##  the number of unsaved file entries in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  A bitmask of options that affects how the translation unit is managed but not its compilation. This should be a bitwise OR of the CXTranslationUnit_XXX flags.
  ## 
  ##  **
  ## 
  ##  A non-NULL pointer to store the created Error: cannot render: rnLiteralBlock describing the parsed code and containing any diagnostics produced by the compiler.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  Zero on success, otherwise returns an error code.
proc parseTranslationUnit2FullArgv*(cIdx: CXIndex; source_filename: cstring;
                                   command_line_args: ptr[cstring];
                                   num_command_line_args: int;
                                   unsaved_files: ptr[CXUnsavedFile];
                                   num_unsaved_files: cuint; options: cuint;
                                   out_TU: ptr[CXTranslationUnit]) {.cdecl,
    dynlib: libclang, importc: "clang_parseTranslationUnit2FullArgv".}
  ## 
  ## 
  ##  Same as clang_parseTranslationUnit2 but requires a full command line for Error: cannot render: rnLiteralBlock including argv[0]. This is useful if the standard library paths are relative to the binary.
proc defaultSaveOptions*(tU: CXTranslationUnit) {.cdecl, dynlib: libclang,
    importc: "clang_defaultSaveOptions".}
  ## 
  ## 
  ##  Returns the set of flags that is suitable for saving a translation unit.
  ## 
  ##  The set of flags returned provide options for Error: cannot render: rnLiteralBlock by default. The returned flag set contains an unspecified set of options that save translation units with the most commonly-requested data.
proc saveTranslationUnit*(tU: CXTranslationUnit; fileName: cstring; options: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_saveTranslationUnit".}
  ## 
  ## 
  ##  Saves a translation unit into a serialized representation of that translation unit on disk.
  ## 
  ##  Any translation unit that was parsed without error can be saved into a file. The translation unit can then be deserialized into a new Error: cannot render: rnLiteralBlock with Error: cannot render: rnLiteralBlock or, if it is an incomplete translation unit that corresponds to a header, used as a precompiled header when parsing other translation units.
  ## 
  ##  **
  ## 
  ##  The translation unit to save.
  ## 
  ##  **
  ## 
  ##  The file to which the translation unit will be saved.
  ## 
  ##  **
  ## 
  ##  A bitmask of options that affects how the translation unit is saved. This should be a bitwise OR of the CXSaveTranslationUnit_XXX flags.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A value that will match one of the enumerators of the CXSaveError enumeration. Zero (CXSaveError_None) indicates that the translation unit was saved successfully, while a non-zero value indicates that a problem occurred.
proc suspendTranslationUnit*(argCXTranslationUnit: CXTranslationUnit) {.cdecl,
    dynlib: libclang, importc: "clang_suspendTranslationUnit".}
  ## 
  ## 
  ##  Suspend a translation unit in order to free memory associated with it.
  ## 
  ##  A suspended translation unit uses significantly less memory but on the other side does not support any other calls than Error: cannot render: rnLiteralBlock to resume it or Error: cannot render: rnLiteralBlock to dispose it completely.
proc disposeTranslationUnit*(argCXTranslationUnit: CXTranslationUnit) {.cdecl,
    dynlib: libclang, importc: "clang_disposeTranslationUnit".}
  ## 
  ## 
  ##  Destroy the specified CXTranslationUnit object.
proc defaultReparseOptions*(tU: CXTranslationUnit) {.cdecl, dynlib: libclang,
    importc: "clang_defaultReparseOptions".}
  ## 
  ## 
  ##  Returns the set of flags that is suitable for reparsing a translation unit.
  ## 
  ##  The set of flags returned provide options for Error: cannot render: rnLiteralBlock by default. The returned flag set contains an unspecified set of optimizations geared toward common uses of reparsing. The set of optimizations enabled may change from one version to the next.
proc reparseTranslationUnit*(tU: CXTranslationUnit; num_unsaved_files: cuint;
                            unsaved_files: ptr[CXUnsavedFile]; options: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_reparseTranslationUnit".}
  ## 
  ## 
  ##  Reparse the source files that produced this translation unit.
  ## 
  ##  This routine can be used to re-parse the source files that originally created the given translation unit, for example because those source files have changed (either on disk or as passed via Error: cannot render: rnLiteralBlock The source code will be reparsed with the same command-line options as it was originally parsed.
  ## 
  ##  Reparsing a translation unit invalidates all cursors and source locations that refer into that translation unit. This makes reparsing a translation unit semantically equivalent to destroying the translation unit and then creating a new translation unit with the same command-line arguments. However, it may be more efficient to reparse a translation unit using this routine.
  ## 
  ##  **
  ## 
  ##  The translation unit whose contents will be re-parsed. The translation unit must originally have been built with Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  The number of unsaved file entries in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  The files that have not yet been saved to disk but may be required for parsing, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns.
  ## 
  ##  **
  ## 
  ##  A bitset of options composed of the flags in CXReparse_Flags. The function Error: cannot render: rnLiteralBlock produces a default set of options recommended for most uses, based on the translation unit.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  0 if the sources could be reparsed.  A non-zero error code will be returned if reparsing was impossible, such that the translation unit is invalid. In such cases, the only valid call for Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock  The error codes returned by this routine are described by the Error: cannot render: rnLiteralBlock enum.
proc getTUResourceUsageName*(kind: CXTUResourceUsageKind) {.cdecl, dynlib: libclang,
    importc: "clang_getTUResourceUsageName".}
  ## 
  ## 
  ##  Returns the human-readable null-terminated C string that represents  the name of the memory category.  This string should never be freed.
type
  CXTUResourceUsageEntry = object
    kind*: CXTUResourceUsageKind
    amount*: culong

type
  CXTUResourceUsage = object
    data*: pointer
    numEntries*: cuint
    entries*: ptr[CXTUResourceUsageEntry]

proc getCXTUResourceUsage*(tU: CXTranslationUnit) {.cdecl, dynlib: libclang,
    importc: "clang_getCXTUResourceUsage".}
  ## 
  ## 
  ##  Return the memory usage of a translation unit.  This object  should be released with clang_disposeCXTUResourceUsage().
proc disposeCXTUResourceUsage*(usage: CXTUResourceUsage) {.cdecl, dynlib: libclang,
    importc: "clang_disposeCXTUResourceUsage".}
proc getTranslationUnitTargetInfo*(cTUnit: CXTranslationUnit) {.cdecl,
    dynlib: libclang, importc: "clang_getTranslationUnitTargetInfo".}
  ## 
  ## 
  ##  Get target information for this translation unit.
  ## 
  ##  The CXTargetInfo object cannot outlive the CXTranslationUnit object.
proc dispose*(info: CXTargetInfo) {.cdecl, dynlib: libclang,
                                 importc: "clang_TargetInfo_dispose".}
  ## 
  ## 
  ##  Destroy the CXTargetInfo object.
proc getTriple*(info: CXTargetInfo) {.cdecl, dynlib: libclang,
                                   importc: "clang_TargetInfo_getTriple".}
  ## 
  ## 
  ##  Get the normalized target triple as a string.
  ## 
  ##  Returns the empty string in case of any error.
proc getPointerWidth*(info: CXTargetInfo) {.cdecl, dynlib: libclang,
    importc: "clang_TargetInfo_getPointerWidth".}
  ## 
  ## 
  ##  Get the pointer width of the target in bits.
  ## 
  ##  Returns -1 in case of error.
type
  CXCursor = object
    kind*: CXCursorKind
    xdata*: int
    data*: array[3, pointer]

proc getNullCursor*() {.cdecl, dynlib: libclang, importc: "clang_getNullCursor".}
  ## 
  ## 
  ##  Retrieve the NULL cursor, which represents no entity.
proc getTranslationUnitCursor*(argCXTranslationUnit: CXTranslationUnit) {.cdecl,
    dynlib: libclang, importc: "clang_getTranslationUnitCursor".}
  ## 
  ## 
  ##  Retrieve the cursor that represents the given translation unit.
  ## 
  ##  The translation unit cursor can be used to start traversing the various declarations within the given translation unit.
proc equalCursors*(argCXCursor: CXCursor; argCXCursor: CXCursor) {.cdecl,
    dynlib: libclang, importc: "clang_equalCursors".}
  ## 
  ## 
  ##  Determine whether two cursors are equivalent.
proc isNull*(cursor: CXCursor) {.cdecl, dynlib: libclang,
                              importc: "clang_Cursor_isNull".}
  ## 
  ## 
  ##  Returns non-zero if Error: cannot render: rnLiteralBlock is null.
proc hashCursor*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
                                       importc: "clang_hashCursor".}
  ## 
  ## 
  ##  Compute a hash value for the given cursor.
proc getCursorKind*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorKind".}
  ## 
  ## 
  ##  Retrieve the kind of the given cursor.
proc isDeclaration*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isDeclaration".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents a declaration.
proc isInvalidDeclaration*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_isInvalidDeclaration".}
  ## 
  ## 
  ##  Determine whether the given declaration is invalid.
  ## 
  ##  A declaration is invalid if it could not be parsed successfully.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the cursor represents a declaration and it is invalid, otherwise NULL.
proc isReference*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isReference".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents a simple reference.
  ## 
  ##  Note that other kinds of cursors (such as expressions) can also refer to other cursors. Use clang_getCursorReferenced() to determine whether a particular cursor refers to another entity.
proc isExpression*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isExpression".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents an expression.
proc isStatement*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isStatement".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents a statement.
proc isAttribute*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isAttribute".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents an attribute.
proc hasAttrs*(c: CXCursor) {.cdecl, dynlib: libclang,
                           importc: "clang_Cursor_hasAttrs".}
  ## 
  ## 
  ##  Determine whether the given cursor has any attributes.
proc isInvalid*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isInvalid".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents an invalid cursor.
proc isTranslationUnit*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isTranslationUnit".}
  ## 
  ## 
  ##  Determine whether the given cursor kind represents a translation unit.
proc isPreprocessing*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isPreprocessing".}
  ## 
  ## 
  ## * Determine whether the given cursor represents a preprocessing element, such as a preprocessor directive or macro instantiation.
proc isUnexposed*(argCXCursorKind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_isUnexposed".}
  ## 
  ## 
  ## * Determine whether the given cursor represents a currently  unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
proc getCursorLinkage*(cursor: CXCursor) {.cdecl, dynlib: libclang,
                                        importc: "clang_getCursorLinkage".}
  ## 
  ## 
  ##  Determine the linkage of the entity referred to by a given cursor.
proc getCursorVisibility*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorVisibility".}
  ## 
  ## 
  ##  Describe the visibility of the entity referred to by a cursor.
  ## 
  ##  This returns the default visibility if not explicitly specified by a visibility attribute. The default visibility may be changed by commandline arguments.
  ## 
  ##  **
  ## 
  ##  The cursor to query.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The visibility of the cursor.
proc getCursorAvailability*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorAvailability".}
  ## 
  ## 
  ##  Determine the availability of the entity that this cursor refers to, taking the current target platform into account.
  ## 
  ##  **
  ## 
  ##  The cursor to query.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The availability of the cursor.
type
  CXPlatformAvailability = object
    platform*: CXString
    introduced*: CXVersion
    deprecated*: CXVersion
    obsoleted*: CXVersion
    unavailable*: int
    message*: CXString

proc getCursorPlatformAvailability*(cursor: CXCursor; always_deprecated: ptr[int];
                                   deprecated_message: ptr[CXString];
                                   always_unavailable: ptr[int];
                                   unavailable_message: ptr[CXString];
                                   availability: ptr[CXPlatformAvailability];
                                   availability_size: int) {.cdecl,
    dynlib: libclang, importc: "clang_getCursorPlatformAvailability".}
  ## 
  ## 
  ##  Determine the availability of the entity that this cursor refers to on any platforms for which availability information is known.
  ## 
  ##  **
  ## 
  ##  The cursor to query.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to indicate whether the entity is deprecated on all platforms.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to the message text provided along with the unconditional deprecation of this entity. The client is responsible for deallocating this string.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to indicate whether the entity is unavailable on all platforms.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to the message text provided along with the unconditional unavailability of this entity. The client is responsible for deallocating this string.
  ## 
  ##  **
  ## 
  ##  If non-NULL, an array of CXPlatformAvailability instances that will be populated with platform availability information, up to either the number of platforms for which availability information is available (as returned by this function) or Error: cannot render: rnLiteralBlock whichever is smaller.
  ## 
  ##  **
  ## 
  ##  The number of elements available in the Error: cannot render: rnLiteralBlock array.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The number of platforms (N) for which availability information is available (which is unrelated to Error: cannot render: rnLiteralBlock
  ## 
  ##  Note that the client is responsible for calling Error: cannot render: rnLiteralBlock to free each of the platform-availability structures returned. There are Error: cannot render: rnLiteralBlock availability_size) such structures.
proc disposeCXPlatformAvailability*(availability: ptr[CXPlatformAvailability]) {.
    cdecl, dynlib: libclang, importc: "clang_disposeCXPlatformAvailability".}
  ## 
  ## 
  ##  Free the memory associated with a Error: cannot render: rnLiteralBlock structure.
proc getCursorLanguage*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorLanguage".}
  ## 
  ## 
  ##  Determine the "language" of the entity referred to by a given cursor.
proc getCursorTLSKind*(cursor: CXCursor) {.cdecl, dynlib: libclang,
                                        importc: "clang_getCursorTLSKind".}
  ## 
  ## 
  ##  Determine the "thread-local storage (TLS) kind" of the declaration referred to by a cursor.
proc getTranslationUnit*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getTranslationUnit".}
  ## 
  ## 
  ##  Returns the translation unit that a cursor originated from.
type
  CXCursorSet = distinct ptr[CXCursorSetImpl]
proc createCXCursorSet*() {.cdecl, dynlib: libclang,
                          importc: "clang_createCXCursorSet".}
  ## 
  ## 
  ##  Creates an empty CXCursorSet.
proc disposeCXCursorSet*(cset: CXCursorSet) {.cdecl, dynlib: libclang,
    importc: "clang_disposeCXCursorSet".}
  ## 
  ## 
  ##  Disposes a CXCursorSet and releases its associated memory.
proc contains*(cset: CXCursorSet; cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXCursorSet_contains".}
  ## 
  ## 
  ##  Queries a CXCursorSet to see if it contains a specific CXCursor.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the set contains the specified cursor.
proc insert*(cset: CXCursorSet; cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXCursorSet_insert".}
  ## 
  ## 
  ##  Inserts a CXCursor into a CXCursorSet.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  zero if the CXCursor was already in the set, and non-zero otherwise.
proc getCursorSemanticParent*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorSemanticParent".}
  ## 
  ## 
  ##  Determine the semantic parent of the given cursor.
  ## 
  ##  The semantic parent of a cursor is the cursor that semantically contains the given Error: cannot render: rnLiteralBlock For many declarations, the lexical and semantic parents are equivalent (the lexical parent is returned by Error: cannot render: rnLiteralBlock They diverge when declarations or definitions are provided out-of-line. For example:
  ## 
  ##  Error: cannot render: rnCodeBlock
  ## 
  ##  In the out-of-line definition of Error: cannot render: rnLiteralBlock the semantic parent is the class Error: cannot render: rnLiteralBlock of which this function is a member. The lexical parent is the place where the declaration actually occurs in the source code; in this case, the definition occurs in the translation unit. In general, the lexical parent for a given entity can change without affecting the semantics of the program, and the lexical parent of different declarations of the same entity may be different. Changing the semantic parent of a declaration, on the other hand, can have a major impact on semantics, and redeclarations of a particular entity should all have the same semantic context.
  ## 
  ##  In the example above, both declarations of Error: cannot render: rnLiteralBlock have Error: cannot render: rnLiteralBlock as their semantic context, while the lexical context of the first Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock and the lexical context of the second Error: cannot render: rnLiteralBlock is the translation unit.
  ## 
  ##  For global declarations, the semantic parent is the translation unit.
proc getCursorLexicalParent*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorLexicalParent".}
  ## 
  ## 
  ##  Determine the lexical parent of the given cursor.
  ## 
  ##  The lexical parent of a cursor is the cursor in which the given Error: cannot render: rnLiteralBlock was actually written. For many declarations, the lexical and semantic parents are equivalent (the semantic parent is returned by Error: cannot render: rnLiteralBlock They diverge when declarations or definitions are provided out-of-line. For example:
  ## 
  ##  Error: cannot render: rnCodeBlock
  ## 
  ##  In the out-of-line definition of Error: cannot render: rnLiteralBlock the semantic parent is the class Error: cannot render: rnLiteralBlock of which this function is a member. The lexical parent is the place where the declaration actually occurs in the source code; in this case, the definition occurs in the translation unit. In general, the lexical parent for a given entity can change without affecting the semantics of the program, and the lexical parent of different declarations of the same entity may be different. Changing the semantic parent of a declaration, on the other hand, can have a major impact on semantics, and redeclarations of a particular entity should all have the same semantic context.
  ## 
  ##  In the example above, both declarations of Error: cannot render: rnLiteralBlock have Error: cannot render: rnLiteralBlock as their semantic context, while the lexical context of the first Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock and the lexical context of the second Error: cannot render: rnLiteralBlock is the translation unit.
  ## 
  ##  For declarations written in the global scope, the lexical parent is the translation unit.
proc getOverriddenCursors*(cursor: CXCursor; overridden: ptr[ptr[CXCursor]];
                          num_overridden: ptr[cuint]) {.cdecl, dynlib: libclang,
    importc: "clang_getOverriddenCursors".}
  ## 
  ## 
  ##  Determine the set of methods that are overridden by the given method.
  ## 
  ##  In both Objective-C and C++, a method (aka virtual member function, in C++) can override a virtual method in a base class. For Objective-C, a method is said to override any method in the class's base class, its protocols, or its categories' protocols, that has the same selector and is of the same kind (class or instance). If no such method exists, the search continues to the class's superclass, its protocols, and its categories, and so on. A method from an Objective-C implementation is considered to override the same methods as its corresponding method in the interface.
  ## 
  ##  For C++, a virtual member function overrides any virtual member function with the same signature that occurs in its base classes. With multiple inheritance, a virtual member function can override several virtual member functions coming from different base classes.
  ## 
  ##  In all cases, this function determines the immediate overridden method, rather than all of the overridden methods. For example, if a method is originally declared in a class A, then overridden in B (which in inherits from A) and also in C (which inherited from B), then the only overridden method returned from this function when invoked on C's method will be B's method. The client may then invoke this function again, given the previously-found overridden methods, to map out the complete method-override set.
  ## 
  ##  **
  ## 
  ##  A cursor representing an Objective-C or C++ method. This routine will compute the set of methods that this method overrides.
  ## 
  ##  **
  ## 
  ##  A pointer whose pointee will be replaced with a pointer to an array of cursors, representing the set of overridden methods. If there are no overridden methods, the pointee will be set to NULL. The pointee must be freed via a call to Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  A pointer to the number of overridden functions, will be set to the number of overridden functions in the array pointed to by Error: cannot render: rnLiteralBlock 
proc disposeOverriddenCursors*(overridden: ptr[CXCursor]) {.cdecl, dynlib: libclang,
    importc: "clang_disposeOverriddenCursors".}
  ## 
  ## 
  ##  Free the set of overridden cursors returned by Error: cannot render: rnLiteralBlock 
proc getIncludedFile*(cursor: CXCursor) {.cdecl, dynlib: libclang,
                                       importc: "clang_getIncludedFile".}
  ## 
  ## 
  ##  Retrieve the file that is included by the given inclusion directive cursor.
proc getCursor*(argCXTranslationUnit: CXTranslationUnit;
               argCXSourceLocation: CXSourceLocation) {.cdecl, dynlib: libclang,
    importc: "clang_getCursor".}
  ## 
  ## 
  ##  Map a source location to the cursor that describes the entity at that location in the source code.
  ## 
  ##  clang_getCursor() maps an arbitrary source location within a translation unit down to the most specific cursor that describes the entity at that location. For example, given an expression Error: cannot render: rnLiteralBlock + y, invoking clang_getCursor() with a source location pointing to "x" will return the cursor for "x"; similarly for "y". If the cursor points anywhere between "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor() will return a cursor referring to the "+" expression.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  a cursor representing the entity at the given source location, or a NULL cursor if no such entity can be found.
proc getCursorLocation*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorLocation".}
  ## 
  ## 
  ##  Retrieve the physical location of the source constructor referenced by the given cursor.
  ## 
  ##  The location of a declaration is typically the location of the name of that declaration, where the name of that declaration would occur if it is unnamed, or some keyword that introduces that particular declaration. The location of a reference is where that reference occurs within the source code.
proc getCursorExtent*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorExtent".}
  ## 
  ## 
  ##  Retrieve the physical extent of the source construct referenced by the given cursor.
  ## 
  ##  The extent of a cursor starts with the file/line/column pointing at the first character within the source construct that the cursor refers to and ends with the last character within that source construct. For a declaration, the extent covers the declaration itself. For a reference, the extent covers the location of the reference (e.g., where the referenced entity was actually used).
type
  CXType = object
    kind*: CXTypeKind
    data*: array[2, pointer]

proc getCursorType*(c: CXCursor) {.cdecl, dynlib: libclang,
                                importc: "clang_getCursorType".}
  ## 
  ## 
  ##  Retrieve the type of a CXCursor (if any).
proc getTypeSpelling*(cT: CXType) {.cdecl, dynlib: libclang,
                                 importc: "clang_getTypeSpelling".}
  ## 
  ## 
  ##  Pretty-print the underlying type using the rules of the language of the translation unit from which it came.
  ## 
  ##  If the type is invalid, an empty string is returned.
proc getTypedefDeclUnderlyingType*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getTypedefDeclUnderlyingType".}
  ## 
  ## 
  ##  Retrieve the underlying type of a typedef declaration.
  ## 
  ##  If the cursor does not reference a typedef declaration, an invalid type is returned.
proc getEnumDeclIntegerType*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getEnumDeclIntegerType".}
  ## 
  ## 
  ##  Retrieve the integer type of an enum declaration.
  ## 
  ##  If the cursor does not reference an enum declaration, an invalid type is returned.
proc getEnumConstantDeclValue*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getEnumConstantDeclValue".}
  ## 
  ## 
  ##  Retrieve the integer value of an enum constant declaration as a signed  long long.
  ## 
  ##  If the cursor does not reference an enum constant declaration, LLONG_MIN is returned. Since this is also potentially a valid constant value, the kind of the cursor must be verified before calling this function.
proc getEnumConstantDeclUnsignedValue*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getEnumConstantDeclUnsignedValue".}
  ## 
  ## 
  ##  Retrieve the integer value of an enum constant declaration as an unsigned  long long.
  ## 
  ##  If the cursor does not reference an enum constant declaration, ULLONG_MAX is returned. Since this is also potentially a valid constant value, the kind of the cursor must be verified before calling this function.
proc getFieldDeclBitWidth*(c: CXCursor) {.cdecl, dynlib: libclang,
                                       importc: "clang_getFieldDeclBitWidth".}
  ## 
  ## 
  ##  Retrieve the bit width of a bit field declaration as an integer.
  ## 
  ##  If a cursor that is not a bit field declaration is passed in, -1 is returned.
proc getNumArguments*(c: CXCursor) {.cdecl, dynlib: libclang,
                                  importc: "clang_Cursor_getNumArguments".}
  ## 
  ## 
  ##  Retrieve the number of non-variadic arguments associated with a given cursor.
  ## 
  ##  The number of arguments can be determined for calls as well as for declarations of functions or methods. For other cursors -1 is returned.
proc getArgument*(c: CXCursor; i: cuint) {.cdecl, dynlib: libclang,
                                      importc: "clang_Cursor_getArgument".}
  ## 
  ## 
  ##  Retrieve the argument cursor of a function or method.
  ## 
  ##  The argument cursor can be determined for calls as well as for declarations of functions or methods. For other cursors and for invalid indices, an invalid cursor is returned.
proc getNumTemplateArguments*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getNumTemplateArguments".}
  ## 
  ## 
  ## Returns the number of template args of a function decl representing a template specialization.
  ## 
  ##  If the argument cursor cannot be converted into a template function declaration, -1 is returned.
  ## 
  ##  For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }
  ## 
  ##    template <>   void foo<float, -7, true>();
  ## 
  ##  The value 3 would be returned from this call.
proc getTemplateArgumentKind*(c: CXCursor; i: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getTemplateArgumentKind".}
  ## 
  ## 
  ##  Retrieve the kind of the I'th template argument of the CXCursor C.
  ## 
  ##  If the argument CXCursor does not represent a FunctionDecl, an invalid template argument kind is returned.
  ## 
  ##  For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }
  ## 
  ##    template <>   void foo<float, -7, true>();
  ## 
  ##  For I = 0, 1, and 2, Type, Integral, and Integral will be returned, respectively.
proc getTemplateArgumentType*(c: CXCursor; i: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getTemplateArgumentType".}
  ## 
  ## 
  ##  Retrieve a CXType representing the type of a TemplateArgument of a  function decl representing a template specialization.
  ## 
  ##  If the argument CXCursor does not represent a FunctionDecl whose I'th template argument has a kind of CXTemplateArgKind_Integral, an invalid type is returned.
  ## 
  ##  For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }
  ## 
  ##    template <>   void foo<float, -7, true>();
  ## 
  ##  If called with I = 0, "float", will be returned. Invalid types will be returned for I == 1 or 2.
proc getTemplateArgumentValue*(c: CXCursor; i: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getTemplateArgumentValue".}
  ## 
  ## 
  ##  Retrieve the value of an Integral TemplateArgument (of a function  decl representing a template specialization) as a signed long long.
  ## 
  ##  It is undefined to call this function on a CXCursor that does not represent a FunctionDecl or whose I'th template argument is not an integral value.
  ## 
  ##  For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }
  ## 
  ##    template <>   void foo<float, -7, true>();
  ## 
  ##  If called with I = 1 or 2, -7 or true will be returned, respectively. For I == 0, this function's behavior is undefined.
proc getTemplateArgumentUnsignedValue*(c: CXCursor; i: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getTemplateArgumentUnsignedValue".}
  ## 
  ## 
  ##  Retrieve the value of an Integral TemplateArgument (of a function  decl representing a template specialization) as an unsigned long long.
  ## 
  ##  It is undefined to call this function on a CXCursor that does not represent a FunctionDecl or whose I'th template argument is not an integral value.
  ## 
  ##  For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }
  ## 
  ##    template <>   void foo<float, 2147483649, true>();
  ## 
  ##  If called with I = 1 or 2, 2147483649 or true will be returned, respectively. For I == 0, this function's behavior is undefined.
proc equalTypes*(a: CXType; b: CXType) {.cdecl, dynlib: libclang,
                                    importc: "clang_equalTypes".}
  ## 
  ## 
  ##  Determine whether two CXTypes represent the same type.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the CXTypes represent the same type and          zero otherwise.
proc getCanonicalType*(t: CXType) {.cdecl, dynlib: libclang,
                                 importc: "clang_getCanonicalType".}
  ## 
  ## 
  ##  Return the canonical type for a CXType.
  ## 
  ##  Clang's type system explicitly models typedefs and all the ways a specific type can be represented.  The canonical type is the underlying type with all the "sugar" removed.  For example, if 'T' is a typedef for 'int', the canonical type for 'T' would be 'int'.
proc isConstQualifiedType*(t: CXType) {.cdecl, dynlib: libclang,
                                     importc: "clang_isConstQualifiedType".}
  ## 
  ## 
  ##  Determine whether a CXType has the "const" qualifier set, without looking through typedefs that may have added "const" at a different level.
proc isMacroFunctionLike*(c: CXCursor) {.cdecl, dynlib: libclang, importc: "clang_Cursor_isMacroFunctionLike".}
  ## 
  ## 
  ##  Determine whether a  CXCursor that is a macro, is function like.
proc isMacroBuiltin*(c: CXCursor) {.cdecl, dynlib: libclang,
                                 importc: "clang_Cursor_isMacroBuiltin".}
  ## 
  ## 
  ##  Determine whether a  CXCursor that is a macro, is a builtin one.
proc isFunctionInlined*(c: CXCursor) {.cdecl, dynlib: libclang,
                                    importc: "clang_Cursor_isFunctionInlined".}
  ## 
  ## 
  ##  Determine whether a  CXCursor that is a function declaration, is an inline declaration.
proc isVolatileQualifiedType*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_isVolatileQualifiedType".}
  ## 
  ## 
  ##  Determine whether a CXType has the "volatile" qualifier set, without looking through typedefs that may have added "volatile" at a different level.
proc isRestrictQualifiedType*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_isRestrictQualifiedType".}
  ## 
  ## 
  ##  Determine whether a CXType has the "restrict" qualifier set, without looking through typedefs that may have added "restrict" at a different level.
proc getAddressSpace*(t: CXType) {.cdecl, dynlib: libclang,
                                importc: "clang_getAddressSpace".}
  ## 
  ## 
  ##  Returns the address space of the given type.
proc getTypedefName*(cT: CXType) {.cdecl, dynlib: libclang,
                                importc: "clang_getTypedefName".}
  ## 
  ## 
  ##  Returns the typedef name of the given type.
proc getPointeeType*(t: CXType) {.cdecl, dynlib: libclang,
                               importc: "clang_getPointeeType".}
  ## 
  ## 
  ##  For pointer types, returns the type of the pointee.
proc getTypeDeclaration*(t: CXType) {.cdecl, dynlib: libclang,
                                   importc: "clang_getTypeDeclaration".}
  ## 
  ## 
  ##  Return the cursor for the declaration of the given type.
proc getDeclObjCTypeEncoding*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getDeclObjCTypeEncoding".}
  ## 
  ## 
  ##  Returns the Objective-C type encoding for the specified declaration.
proc getObjCEncoding*(cxtype: CXType) {.cdecl, dynlib: libclang,
                                     importc: "clang_Type_getObjCEncoding".}
  ## 
  ## 
  ##  Returns the Objective-C type encoding for the specified CXType.
proc getTypeKindSpelling*(k: CXTypeKind) {.cdecl, dynlib: libclang,
                                        importc: "clang_getTypeKindSpelling".}
  ## 
  ## 
  ##  Retrieve the spelling of a given CXTypeKind.
proc getFunctionTypeCallingConv*(t: CXType) {.cdecl, dynlib: libclang,
    importc: "clang_getFunctionTypeCallingConv".}
  ## 
  ## 
  ##  Retrieve the calling convention associated with a function type.
  ## 
  ##  If a non-function type is passed in, CXCallingConv_Invalid is returned.
proc getResultType*(t: CXType) {.cdecl, dynlib: libclang,
                              importc: "clang_getResultType".}
  ## 
  ## 
  ##  Retrieve the return type associated with a function type.
  ## 
  ##  If a non-function type is passed in, an invalid type is returned.
proc getExceptionSpecificationType*(t: CXType) {.cdecl, dynlib: libclang,
    importc: "clang_getExceptionSpecificationType".}
  ## 
  ## 
  ##  Retrieve the exception specification type associated with a function type. This is a value of type CXCursor_ExceptionSpecificationKind.
  ## 
  ##  If a non-function type is passed in, an error code of -1 is returned.
proc getNumArgTypes*(t: CXType) {.cdecl, dynlib: libclang,
                               importc: "clang_getNumArgTypes".}
  ## 
  ## 
  ##  Retrieve the number of non-variadic parameters associated with a function type.
  ## 
  ##  If a non-function type is passed in, -1 is returned.
proc getArgType*(t: CXType; i: cuint) {.cdecl, dynlib: libclang,
                                   importc: "clang_getArgType".}
  ## 
  ## 
  ##  Retrieve the type of a parameter of a function type.
  ## 
  ##  If a non-function type is passed in or the function does not have enough parameters, an invalid type is returned.
proc getObjCObjectBaseType*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_Type_getObjCObjectBaseType".}
  ## 
  ## 
  ##  Retrieves the base type of the ObjCObjectType.
  ## 
  ##  If the type is not an ObjC object, an invalid type is returned.
proc getNumObjCProtocolRefs*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_Type_getNumObjCProtocolRefs".}
  ## 
  ## 
  ##  Retrieve the number of protocol references associated with an ObjC object/id.
  ## 
  ##  If the type is not an ObjC object, 0 is returned.
proc getObjCProtocolDecl*(t: CXType; i: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_Type_getObjCProtocolDecl".}
  ## 
  ## 
  ##  Retrieve the decl for a protocol reference for an ObjC object/id.
  ## 
  ##  If the type is not an ObjC object or there are not enough protocol references, an invalid cursor is returned.
proc getNumObjCTypeArgs*(t: CXType) {.cdecl, dynlib: libclang,
                                   importc: "clang_Type_getNumObjCTypeArgs".}
  ## 
  ## 
  ##  Retreive the number of type arguments associated with an ObjC object.
  ## 
  ##  If the type is not an ObjC object, 0 is returned.
proc getObjCTypeArg*(t: CXType; i: cuint) {.cdecl, dynlib: libclang,
                                       importc: "clang_Type_getObjCTypeArg".}
  ## 
  ## 
  ##  Retrieve a type argument associated with an ObjC object.
  ## 
  ##  If the type is not an ObjC or the index is not valid, an invalid type is returned.
proc isFunctionTypeVariadic*(t: CXType) {.cdecl, dynlib: libclang,
                                       importc: "clang_isFunctionTypeVariadic".}
  ## 
  ## 
  ##  Return 1 if the CXType is a variadic function type, and 0 otherwise.
proc getCursorResultType*(c: CXCursor) {.cdecl, dynlib: libclang,
                                      importc: "clang_getCursorResultType".}
  ## 
  ## 
  ##  Retrieve the return type associated with a given cursor.
  ## 
  ##  This only returns a valid type if the cursor refers to a function or method.
proc getCursorExceptionSpecificationType*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorExceptionSpecificationType".}
  ## 
  ## 
  ##  Retrieve the exception specification type associated with a given cursor. This is a value of type CXCursor_ExceptionSpecificationKind.
  ## 
  ##  This only returns a valid result if the cursor refers to a function or method.
proc isPODType*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_isPODType".}
  ## 
  ## 
  ##  Return 1 if the CXType is a POD (plain old data) type, and 0  otherwise.
proc getElementType*(t: CXType) {.cdecl, dynlib: libclang,
                               importc: "clang_getElementType".}
  ## 
  ## 
  ##  Return the element type of an array, complex, or vector type.
  ## 
  ##  If a type is passed in that is not an array, complex, or vector type, an invalid type is returned.
proc getNumElements*(t: CXType) {.cdecl, dynlib: libclang,
                               importc: "clang_getNumElements".}
  ## 
  ## 
  ##  Return the number of elements of an array or vector type.
  ## 
  ##  If a type is passed in that is not an array or vector type, -1 is returned.
proc getArrayElementType*(t: CXType) {.cdecl, dynlib: libclang,
                                    importc: "clang_getArrayElementType".}
  ## 
  ## 
  ##  Return the element type of an array type.
  ## 
  ##  If a non-array type is passed in, an invalid type is returned.
proc getArraySize*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_getArraySize".}
  ## 
  ## 
  ##  Return the array size of a constant array.
  ## 
  ##  If a non-array type is passed in, -1 is returned.
proc getNamedType*(t: CXType) {.cdecl, dynlib: libclang,
                             importc: "clang_Type_getNamedType".}
  ## 
  ## 
  ##  Retrieve the type named by the qualified-id.
  ## 
  ##  If a non-elaborated type is passed in, an invalid type is returned.
proc isTransparentTagTypedef*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_Type_isTransparentTagTypedef".}
  ## 
  ## 
  ##  Determine if a typedef is 'transparent' tag.
  ## 
  ##  A typedef is considered 'transparent' if it shares a name and spelling location with its underlying tag type, as is the case with the NS_ENUM macro.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if transparent and zero otherwise.
proc getNullability*(t: CXType) {.cdecl, dynlib: libclang,
                               importc: "clang_Type_getNullability".}
  ## 
  ## 
  ##  Retrieve the nullability kind of a pointer type.
proc getAlignOf*(t: CXType) {.cdecl, dynlib: libclang,
                           importc: "clang_Type_getAlignOf".}
  ## 
  ## 
  ##  Return the alignment of a type in bytes as per C++[expr.alignof]   standard.
  ## 
  ##  If the type declaration is invalid, CXTypeLayoutError_Invalid is returned. If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete   is returned. If the type declaration is a dependent type, CXTypeLayoutError_Dependent is   returned. If the type declaration is not a constant size type,   CXTypeLayoutError_NotConstantSize is returned.
proc getClassType*(t: CXType) {.cdecl, dynlib: libclang,
                             importc: "clang_Type_getClassType".}
  ## 
  ## 
  ##  Return the class type of an member pointer type.
  ## 
  ##  If a non-member-pointer type is passed in, an invalid type is returned.
proc getSizeOf*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_Type_getSizeOf".}
  ## 
  ## 
  ##  Return the size of a type in bytes as per C++[expr.sizeof] standard.
  ## 
  ##  If the type declaration is invalid, CXTypeLayoutError_Invalid is returned. If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete   is returned. If the type declaration is a dependent type, CXTypeLayoutError_Dependent is   returned.
proc getOffsetOf*(t: CXType; s: cstring) {.cdecl, dynlib: libclang,
                                      importc: "clang_Type_getOffsetOf".}
  ## 
  ## 
  ##  Return the offset of a field named S in a record of type T in bits   as it would be returned by __offsetof__ as per C++11[18.2p4]
  ## 
  ##  If the cursor is not a record field declaration, CXTypeLayoutError_Invalid   is returned. If the field's type declaration is an incomplete type,   CXTypeLayoutError_Incomplete is returned. If the field's type declaration is a dependent type,   CXTypeLayoutError_Dependent is returned. If the field's name S is not found,   CXTypeLayoutError_InvalidFieldName is returned.
proc getModifiedType*(t: CXType) {.cdecl, dynlib: libclang,
                                importc: "clang_Type_getModifiedType".}
  ## 
  ## 
  ##  Return the type that was modified by this attributed type.
  ## 
  ##  If the type is not an attributed type, an invalid type is returned.
proc getOffsetOfField*(c: CXCursor) {.cdecl, dynlib: libclang,
                                   importc: "clang_Cursor_getOffsetOfField".}
  ## 
  ## 
  ##  Return the offset of the field represented by the Cursor.
  ## 
  ##  If the cursor is not a field declaration, -1 is returned. If the cursor semantic parent is not a record field declaration,   CXTypeLayoutError_Invalid is returned. If the field's type declaration is an incomplete type,   CXTypeLayoutError_Incomplete is returned. If the field's type declaration is a dependent type,   CXTypeLayoutError_Dependent is returned. If the field's name S is not found,   CXTypeLayoutError_InvalidFieldName is returned.
proc isAnonymous*(c: CXCursor) {.cdecl, dynlib: libclang,
                              importc: "clang_Cursor_isAnonymous".}
  ## 
  ## 
  ##  Determine whether the given cursor represents an anonymous tag or namespace
proc isAnonymousRecordDecl*(c: CXCursor) {.cdecl, dynlib: libclang, importc: "clang_Cursor_isAnonymousRecordDecl".}
  ## 
  ## 
  ##  Determine whether the given cursor represents an anonymous record declaration.
proc isInlineNamespace*(c: CXCursor) {.cdecl, dynlib: libclang,
                                    importc: "clang_Cursor_isInlineNamespace".}
  ## 
  ## 
  ##  Determine whether the given cursor represents an inline namespace declaration.
proc getNumTemplateArguments*(t: CXType) {.cdecl, dynlib: libclang, importc: "clang_Type_getNumTemplateArguments".}
  ## 
  ## 
  ##  Returns the number of template arguments for given template specialization, or -1 if type Error: cannot render: rnLiteralBlock is not a template specialization.
proc getTemplateArgumentAsType*(t: CXType; i: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_Type_getTemplateArgumentAsType".}
  ## 
  ## 
  ##  Returns the type template argument of a template class specialization at given index.
  ## 
  ##  This function only returns template type arguments and does not handle template template arguments or variadic packs.
proc getCXXRefQualifier*(t: CXType) {.cdecl, dynlib: libclang,
                                   importc: "clang_Type_getCXXRefQualifier".}
  ## 
  ## 
  ##  Retrieve the ref-qualifier kind of a function or method.
  ## 
  ##  The ref-qualifier is returned for C++ functions or methods. For other types or non-C++ declarations, CXRefQualifier_None is returned.
proc isBitField*(c: CXCursor) {.cdecl, dynlib: libclang,
                             importc: "clang_Cursor_isBitField".}
  ## 
  ## 
  ##  Returns non-zero if the cursor specifies a Record member that is a   bitfield.
proc isVirtualBase*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_isVirtualBase".}
  ## 
  ## 
  ##  Returns 1 if the base class specified by the cursor with kind   CX_CXXBaseSpecifier is virtual.
proc getCXXAccessSpecifier*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCXXAccessSpecifier".}
  ## 
  ## 
  ##  Returns the access control level for the referenced object.
  ## 
  ##  If the cursor refers to a C++ declaration, its access control level within its parent scope is returned. Otherwise, if the cursor refers to a base specifier or access specifier, the specifier itself is returned.
proc getStorageClass*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getStorageClass".}
  ## 
  ## 
  ##  Returns the storage class for a function or variable declaration.
  ## 
  ##  If the passed in Cursor is not a function or variable declaration, CX_SC_Invalid is returned else the storage class.
proc getNumOverloadedDecls*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getNumOverloadedDecls".}
  ## 
  ## 
  ##  Determine the number of overloaded declarations referenced by a Error: cannot render: rnLiteralBlock cursor.
  ## 
  ##  **
  ## 
  ##  The cursor whose overloaded declarations are being queried.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The number of overloaded declarations referenced by Error: cannot render: rnLiteralBlock If it is not a Error: cannot render: rnLiteralBlock cursor, returns 0.
proc getOverloadedDecl*(cursor: CXCursor; index: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getOverloadedDecl".}
  ## 
  ## 
  ##  Retrieve a cursor for one of the overloaded declarations referenced by a Error: cannot render: rnLiteralBlock cursor.
  ## 
  ##  **
  ## 
  ##  The cursor whose overloaded declarations are being queried.
  ## 
  ##  **
  ## 
  ##  The zero-based index into the set of overloaded declarations in the cursor.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A cursor representing the declaration referenced by the given Error: cannot render: rnLiteralBlock at the specified Error: cannot render: rnLiteralBlock If the cursor does not have an associated set of overloaded declarations, or if the index is out of bounds, returns Error: cannot render: rnLiteralBlock 
proc getIBOutletCollectionType*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getIBOutletCollectionType".}
  ## 
  ## 
  ##  For cursors representing an iboutletcollection attribute,  this function returns the collection element type.
  ## 
  ##  
type
  CXCursorVisitor = distinct ptr[proc (a0: CXCursor; a1: CXCursor; a2: pointer): CXChildVisitResult {.
      cdecl.}]
proc visitChildren*(parent: CXCursor; visitor: CXCursorVisitor;
                   client_data: CXClientData) {.cdecl, dynlib: libclang,
    importc: "clang_visitChildren".}
  ## 
  ## 
  ##  Visit the children of a particular cursor.
  ## 
  ##  This function visits all the direct children of the given cursor, invoking the given Error: cannot render: rnLiteralBlock function with the cursors of each visited child. The traversal may be recursive, if the visitor returns Error: cannot render: rnLiteralBlock The traversal may also be ended prematurely, if the visitor returns Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  the cursor whose child may be visited. All kinds of cursors can be visited, including invalid cursors (which, by definition, have no children).
  ## 
  ##  **
  ## 
  ##  the visitor function that will be invoked for each child of Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  pointer data supplied by the client, which will be passed to the visitor each time it is invoked.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  a non-zero value if the traversal was terminated prematurely by the visitor returning Error: cannot render: rnLiteralBlock 
proc getCursorUSR*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorUSR".}
  ## 
  ## 
  ##  Retrieve a Unified Symbol Resolution (USR) for the entity referenced by the given cursor.
  ## 
  ##  A Unified Symbol Resolution (USR) is a string that identifies a particular entity (function, class, variable, etc.) within a program. USRs can be compared across translation units to determine, e.g., when references in one translation refer to an entity defined in another translation unit.
proc constructUSR_ObjCClass*(class_name: cstring) {.cdecl, dynlib: libclang,
    importc: "clang_constructUSR_ObjCClass".}
  ## 
  ## 
  ##  Construct a USR for a specified Objective-C class.
proc constructUSR_ObjCCategory*(class_name: cstring; category_name: cstring) {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCCategory".}
  ## 
  ## 
  ##  Construct a USR for a specified Objective-C category.
proc constructUSR_ObjCProtocol*(protocol_name: cstring) {.cdecl, dynlib: libclang,
    importc: "clang_constructUSR_ObjCProtocol".}
  ## 
  ## 
  ##  Construct a USR for a specified Objective-C protocol.
proc constructUSR_ObjCIvar*(name: cstring; classUSR: CXString) {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCIvar".}
  ## 
  ## 
  ##  Construct a USR for a specified Objective-C instance variable and   the USR for its containing class.
proc constructUSR_ObjCMethod*(name: cstring; isInstanceMethod: cuint;
                             classUSR: CXString) {.cdecl, dynlib: libclang,
    importc: "clang_constructUSR_ObjCMethod".}
  ## 
  ## 
  ##  Construct a USR for a specified Objective-C method and   the USR for its containing class.
proc constructUSR_ObjCProperty*(property: cstring; classUSR: CXString) {.cdecl,
    dynlib: libclang, importc: "clang_constructUSR_ObjCProperty".}
  ## 
  ## 
  ##  Construct a USR for a specified Objective-C property and the USR  for its containing class.
proc getCursorSpelling*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorSpelling".}
  ## 
  ## 
  ##  Retrieve a name for the entity referenced by this cursor.
proc getSpellingNameRange*(argCXCursor: CXCursor; pieceIndex: cuint; options: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_Cursor_getSpellingNameRange".}
  ## 
  ## 
  ##  Retrieve a range for a piece that forms the cursors spelling name. Most of the times there is only one range for the complete spelling but for Objective-C methods and Objective-C message expressions, there are multiple pieces for each selector identifier.
  ## 
  ##  **
  ## 
  ##  the index of the spelling name piece. If this is greater than the actual number of pieces, it will return a NULL (invalid) range.
  ## 
  ##  **
  ## 
  ##  Reserved.
type
  CXPrintingPolicy = distinct pointer
proc getProperty*(policy: CXPrintingPolicy; property: CXPrintingPolicyProperty) {.
    cdecl, dynlib: libclang, importc: "clang_PrintingPolicy_getProperty".}
  ## 
  ## 
  ##  Get a property value for the given printing policy.
proc setProperty*(policy: CXPrintingPolicy; property: CXPrintingPolicyProperty;
                 value: cuint) {.cdecl, dynlib: libclang,
                               importc: "clang_PrintingPolicy_setProperty".}
  ## 
  ## 
  ##  Set a property value for the given printing policy.
proc getCursorPrintingPolicy*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorPrintingPolicy".}
  ## 
  ## 
  ##  Retrieve the default policy for the cursor.
  ## 
  ##  The policy should be released after use with Error: cannot render: rnLiteralBlock 
proc dispose*(policy: CXPrintingPolicy) {.cdecl, dynlib: libclang,
                                       importc: "clang_PrintingPolicy_dispose".}
  ## 
  ## 
  ##  Release a printing policy.
proc getCursorPrettyPrinted*(cursor: CXCursor; policy: CXPrintingPolicy) {.cdecl,
    dynlib: libclang, importc: "clang_getCursorPrettyPrinted".}
  ## 
  ## 
  ##  Pretty print declarations.
  ## 
  ##  **
  ## 
  ##  The cursor representing a declaration.
  ## 
  ##  **
  ## 
  ##  The policy to control the entities being printed. If NULL, a default policy is used.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The pretty printed declaration or the empty string for other cursors.
proc getCursorDisplayName*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorDisplayName".}
  ## 
  ## 
  ##  Retrieve the display name for the entity referenced by this cursor.
  ## 
  ##  The display name contains extra information that helps identify the cursor, such as the parameters of a function or template or the arguments of a class template specialization.
proc getCursorReferenced*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorReferenced".}
  ## 
  ## 
  ##  For a cursor that is a reference, retrieve a cursor representing the entity that it references.
  ## 
  ##  Reference cursors refer to other entities in the AST. For example, an Objective-C superclass reference cursor refers to an Objective-C class. This function produces the cursor for the Objective-C class from the cursor for the superclass reference. If the input cursor is a declaration or definition, it returns that declaration or definition unchanged. Otherwise, returns the NULL cursor.
proc getCursorDefinition*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorDefinition".}
  ## 
  ## 
  ##   For a cursor that is either a reference to or a declaration  of some entity, retrieve a cursor that describes the definition of  that entity.
  ## 
  ##   Some entities can be declared multiple times within a translation  unit, but only one of those declarations can also be a  definition. For example, given:
  ## 
  ##   Error: cannot render: rnCodeBlock
  ## 
  ##   there are three declarations of the function "f", but only the  second one is a definition. The clang_getCursorDefinition()  function will take any cursor pointing to a declaration of "f"  (the first or fourth lines of the example) or a cursor referenced  that uses "f" (the call to "f' inside "g") and will return a  declaration cursor pointing to the definition (the second "f"  declaration).
  ## 
  ##   If given a cursor for which there is no corresponding definition,  e.g., because there is no definition of that entity within this  translation unit, returns a NULL cursor.
proc isCursorDefinition*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_isCursorDefinition".}
  ## 
  ## 
  ##  Determine whether the declaration pointed to by this cursor is also a definition of that entity.
proc getCanonicalCursor*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCanonicalCursor".}
  ## 
  ## 
  ##  Retrieve the canonical cursor corresponding to the given cursor.
  ## 
  ##  In the C family of languages, many kinds of entities can be declared several times within a single translation unit. For example, a structure type can be forward-declared (possibly multiple times) and later defined:
  ## 
  ##  Error: cannot render: rnCodeBlock
  ## 
  ##  The declarations and the definition of Error: cannot render: rnLiteralBlock are represented by three different cursors, all of which are declarations of the same underlying entity. One of these cursor is considered the "canonical" cursor, which is effectively the representative for the underlying entity. One can determine if two cursors are declarations of the same underlying entity by comparing their canonical cursors.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The canonical cursor for the entity referred to by the given cursor.
proc getObjCSelectorIndex*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCSelectorIndex".}
  ## 
  ## 
  ##  If the cursor points to a selector identifier in an Objective-C method or message expression, this returns the selector index.
  ## 
  ##  After getting a cursor with #clang_getCursor, this can be called to determine if the location points to a selector identifier.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The selector index if the cursor is an Objective-C method or message expression and the cursor is pointing to a selector identifier, or -1 otherwise.
proc isDynamicCall*(c: CXCursor) {.cdecl, dynlib: libclang,
                                importc: "clang_Cursor_isDynamicCall".}
  ## 
  ## 
  ##  Given a cursor pointing to a C++ method call or an Objective-C message, returns non-zero if the method/message is "dynamic", meaning:
  ## 
  ##  For a C++ method: the call is virtual. For an Objective-C message: the receiver is an object instance, not 'super' or a specific class.
  ## 
  ##  If the method/message is "static" or the cursor does not point to a method/message, it will return zero.
proc getReceiverType*(c: CXCursor) {.cdecl, dynlib: libclang,
                                  importc: "clang_Cursor_getReceiverType".}
  ## 
  ## 
  ##  Given a cursor pointing to an Objective-C message or property reference, or C++ method call, returns the CXType of the receiver.
type
  CXObjCPropertyAttrKind = enum
    ocpaknoattr = 0, ocpakreadonly = 1, ocpakgetter = 2, ocpakassign = 4,
    ocpakreadwrite = 8, ocpakretain = 16, ocpakcopy = 32, ocpaknonatomic = 64,
    ocpaksetter = 128, ocpakatomic = 256, ocpakweak = 512, ocpakstrong = 1024,
    ocpakunsafe_unretained = 2048, ocpakclass = 4096
proc getObjCPropertyAttributes*(c: CXCursor; reserved: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_getObjCPropertyAttributes".}
  ## 
  ## 
  ##  Given a cursor that represents a property declaration, return the associated property attributes. The bits are formed from Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  Reserved for future use, pass 0.
proc getObjCPropertyGetterName*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCPropertyGetterName".}
  ## 
  ## 
  ##  Given a cursor that represents a property declaration, return the name of the method that implements the getter.
proc getObjCPropertySetterName*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCPropertySetterName".}
  ## 
  ## 
  ##  Given a cursor that represents a property declaration, return the name of the method that implements the setter, if any.
type
  CXObjCDeclQualifierKind = enum
    ocdqkNone = 0, ocdqkIn = 1, ocdqkInout = 2, ocdqkOut = 4, ocdqkBycopy = 8, ocdqkByref = 16,
    ocdqkOneway = 32
proc getObjCDeclQualifiers*(c: CXCursor) {.cdecl, dynlib: libclang, importc: "clang_Cursor_getObjCDeclQualifiers".}
  ## 
  ## 
  ##  Given a cursor that represents an Objective-C method or parameter declaration, return the associated Objective-C qualifiers for the return type or the parameter respectively. The bits are formed from CXObjCDeclQualifierKind.
proc isObjCOptional*(c: CXCursor) {.cdecl, dynlib: libclang,
                                 importc: "clang_Cursor_isObjCOptional".}
  ## 
  ## 
  ##  Given a cursor that represents an Objective-C method or property declaration, return non-zero if the declaration was affected by "@optional". Returns zero if the cursor is not such a declaration or it is "@required".
proc isVariadic*(c: CXCursor) {.cdecl, dynlib: libclang,
                             importc: "clang_Cursor_isVariadic".}
  ## 
  ## 
  ##  Returns non-zero if the given cursor is a variadic function or method.
proc isExternalSymbol*(c: CXCursor; language: ptr[CXString];
                      definedIn: ptr[CXString]; isGenerated: ptr[cuint]) {.cdecl,
    dynlib: libclang, importc: "clang_Cursor_isExternalSymbol".}
  ## 
  ## 
  ##  Returns non-zero if the given cursor points to a symbol marked with external_source_symbol attribute.
  ## 
  ##  **
  ## 
  ##  If non-NULL, and the attribute is present, will be set to the 'language' string from the attribute.
  ## 
  ##  **
  ## 
  ##  If non-NULL, and the attribute is present, will be set to the 'definedIn' string from the attribute.
  ## 
  ##  **
  ## 
  ##  If non-NULL, and the attribute is present, will be set to non-zero if the 'generated_declaration' is set in the attribute.
proc getCommentRange*(c: CXCursor) {.cdecl, dynlib: libclang,
                                  importc: "clang_Cursor_getCommentRange".}
  ## 
  ## 
  ##  Given a cursor that represents a declaration, return the associated comment's source range.  The range may include multiple consecutive comments with whitespace in between.
proc getRawCommentText*(c: CXCursor) {.cdecl, dynlib: libclang,
                                    importc: "clang_Cursor_getRawCommentText".}
  ## 
  ## 
  ##  Given a cursor that represents a declaration, return the associated comment text, including comment markers.
proc getBriefCommentText*(c: CXCursor) {.cdecl, dynlib: libclang, importc: "clang_Cursor_getBriefCommentText".}
  ## 
  ## 
  ##  Given a cursor that represents a documentable entity (e.g., declaration), return the associated ``
  ## 
  ##  first paragraph.
proc getMangling*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
                                        importc: "clang_Cursor_getMangling".}
  ## 
  ## 
  ##  Retrieve the CXString representing the mangled name of the cursor.
proc getCXXManglings*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getCXXManglings".}
  ## 
  ## 
  ##  Retrieve the CXStrings representing the mangled symbols of the C++ constructor or destructor at the cursor.
proc getObjCManglings*(argCXCursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_Cursor_getObjCManglings".}
  ## 
  ## 
  ##  Retrieve the CXStrings representing the mangled symbols of the ObjC class interface or implementation at the cursor.
type
  CXModule = distinct pointer
proc getModule*(c: CXCursor) {.cdecl, dynlib: libclang,
                            importc: "clang_Cursor_getModule".}
  ## 
  ## 
  ##  Given a CXCursor_ModuleImportDecl cursor, return the associated module.
proc getModuleForFile*(argCXTranslationUnit: CXTranslationUnit; argCXFile: CXFile) {.
    cdecl, dynlib: libclang, importc: "clang_getModuleForFile".}
  ## 
  ## 
  ##  Given a CXFile header file, return the module that contains it, if one exists.
proc getASTFile*(module: CXModule) {.cdecl, dynlib: libclang,
                                  importc: "clang_Module_getASTFile".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the module file where the provided module object came from.
proc getParent*(module: CXModule) {.cdecl, dynlib: libclang,
                                 importc: "clang_Module_getParent".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the parent of a sub-module or NULL if the given module is top-level, e.g. for 'std.vector' it will return the 'std' module.
proc getName*(module: CXModule) {.cdecl, dynlib: libclang,
                               importc: "clang_Module_getName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the name of the module, e.g. for the 'std.vector' sub-module it will return "vector".
proc getFullName*(module: CXModule) {.cdecl, dynlib: libclang,
                                   importc: "clang_Module_getFullName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the full name of the module, e.g. "std.vector".
proc isSystem*(module: CXModule) {.cdecl, dynlib: libclang,
                                importc: "clang_Module_isSystem".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the module is a system one.
proc Module_getNumTopLevelHeaders*(argCXTranslationUnit: CXTranslationUnit;
                                  module: CXModule) {.cdecl, dynlib: libclang,
    importc: "clang_Module_getNumTopLevelHeaders".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the number of top level headers associated with this module.
proc Module_getTopLevelHeader*(argCXTranslationUnit: CXTranslationUnit;
                              module: CXModule; index: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_Module_getTopLevelHeader".}
  ## 
  ## 
  ##  **
  ## 
  ##  a module object.
  ## 
  ##  **
  ## 
  ##  top level header index (zero-based).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the specified top level header associated with the module.
proc CXXConstructor_isConvertingConstructor*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXXConstructor_isConvertingConstructor".}
  ## 
  ## 
  ##  Determine if a C++ constructor is a converting constructor.
proc CXXConstructor_isCopyConstructor*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXXConstructor_isCopyConstructor".}
  ## 
  ## 
  ##  Determine if a C++ constructor is a copy constructor.
proc CXXConstructor_isDefaultConstructor*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXXConstructor_isDefaultConstructor".}
  ## 
  ## 
  ##  Determine if a C++ constructor is the default constructor.
proc CXXConstructor_isMoveConstructor*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXXConstructor_isMoveConstructor".}
  ## 
  ## 
  ##  Determine if a C++ constructor is a move constructor.
proc CXXField_isMutable*(c: CXCursor) {.cdecl, dynlib: libclang,
                                     importc: "clang_CXXField_isMutable".}
  ## 
  ## 
  ##  Determine if a C++ field is declared 'mutable'.
proc CXXMethod_isDefaulted*(c: CXCursor) {.cdecl, dynlib: libclang,
                                        importc: "clang_CXXMethod_isDefaulted".}
  ## 
  ## 
  ##  Determine if a C++ method is declared '= default'.
proc CXXMethod_isPureVirtual*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_CXXMethod_isPureVirtual".}
  ## 
  ## 
  ##  Determine if a C++ member function or member function template is pure virtual.
proc CXXMethod_isStatic*(c: CXCursor) {.cdecl, dynlib: libclang,
                                     importc: "clang_CXXMethod_isStatic".}
  ## 
  ## 
  ##  Determine if a C++ member function or member function template is declared 'static'.
proc CXXMethod_isVirtual*(c: CXCursor) {.cdecl, dynlib: libclang,
                                      importc: "clang_CXXMethod_isVirtual".}
  ## 
  ## 
  ##  Determine if a C++ member function or member function template is explicitly declared 'virtual' or if it overrides a virtual method from one of the base classes.
proc CXXRecord_isAbstract*(c: CXCursor) {.cdecl, dynlib: libclang,
                                       importc: "clang_CXXRecord_isAbstract".}
  ## 
  ## 
  ##  Determine if a C++ record is abstract, i.e. whether a class or struct has a pure virtual member function.
proc EnumDecl_isScoped*(c: CXCursor) {.cdecl, dynlib: libclang,
                                    importc: "clang_EnumDecl_isScoped".}
  ## 
  ## 
  ##  Determine if an enum declaration refers to a scoped enum.
proc CXXMethod_isConst*(c: CXCursor) {.cdecl, dynlib: libclang,
                                    importc: "clang_CXXMethod_isConst".}
  ## 
  ## 
  ##  Determine if a C++ member function or member function template is declared 'const'.
proc getTemplateCursorKind*(c: CXCursor) {.cdecl, dynlib: libclang,
                                        importc: "clang_getTemplateCursorKind".}
  ## 
  ## 
  ##  Given a cursor that represents a template, determine the cursor kind of the specializations would be generated by instantiating the template.
  ## 
  ##  This routine can be used to determine what flavor of function template, class template, or class template partial specialization is stored in the cursor. For example, it can describe whether a class template cursor is declared with "struct", "class" or "union".
  ## 
  ##  **
  ## 
  ##  The cursor to query. This cursor should represent a template declaration.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The cursor kind of the specializations that would be generated by instantiating the template Error: cannot render: rnLiteralBlock If Error: cannot render: rnLiteralBlock is not a template, returns Error: cannot render: rnLiteralBlock 
proc getSpecializedCursorTemplate*(c: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getSpecializedCursorTemplate".}
  ## 
  ## 
  ##  Given a cursor that may represent a specialization or instantiation of a template, retrieve the cursor that represents the template that it specializes or from which it was instantiated.
  ## 
  ##  This routine determines the template involved both for explicit specializations of templates and for implicit instantiations of the template, both of which are referred to as "specializations". For a class template specialization (e.g., Error: cannot render: rnLiteralBlock this routine will return either the primary template (Error: cannot render: rnLiteralBlock or, if the specialization was instantiated from a class template partial specialization, the class template partial specialization. For a class template partial specialization and a function template specialization (including instantiations), this this routine will return the specialized template.
  ## 
  ##  For members of a class template (e.g., member functions, member classes, or static data members), returns the specialized or instantiated member. Although not strictly "templates" in the C++ language, members of class templates have the same notions of specializations and instantiations that templates do, so this routine treats them similarly.
  ## 
  ##  **
  ## 
  ##  A cursor that may be a specialization of a template or a member of a template.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  If the given cursor is a specialization or instantiation of a template or a member thereof, the template or member that it specializes or from which it was instantiated. Otherwise, returns a NULL cursor.
proc getCursorReferenceNameRange*(c: CXCursor; nameFlags: cuint; pieceIndex: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_getCursorReferenceNameRange".}
  ## 
  ## 
  ##  Given a cursor that references something else, return the source range covering that reference.
  ## 
  ##  **
  ## 
  ##  A cursor pointing to a member reference, a declaration reference, or an operator call. **
  ## 
  ##  A bitset with three independent flags: CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and CXNameRange_WantSinglePiece. **
  ## 
  ##  For contiguous names or when passing the flag CXNameRange_WantSinglePiece, only one piece with index 0 is available. When the CXNameRange_WantSinglePiece flag is not passed for a non-contiguous names, this index can be used to retrieve the individual pieces of the name. See also CXNameRange_WantSinglePiece.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The piece of the name pointed to by the given cursor. If there is no name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
type
  CXTokenKind = enum
    tkPunctuation,            ## 
                  ## 
                  ##  A token that contains some kind of punctuation.
    tkKeyword,                ## 
              ## 
              ##  A language keyword.
    tkIdentifier,             ## 
                 ## 
                 ##  An identifier (that is not a keyword).
    tkLiteral,                ## 
              ## 
              ##  A numeric, string, or character literal.
    tkComment                 ## 
             ## 
             ##  A comment.
type
  CXToken = object
    int_data*: array[4, cuint]
    ptr_data*: pointer

proc getToken*(tU: CXTranslationUnit; location: CXSourceLocation) {.cdecl,
    dynlib: libclang, importc: "clang_getToken".}
  ## 
  ## 
  ##  Get the raw lexical token starting with the given location.
  ## 
  ##  **
  ## 
  ##  the translation unit whose text is being tokenized.
  ## 
  ##  **
  ## 
  ##  the source location with which the token starts.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The token starting with the given location or NULL if no such token exist. The returned pointer must be freed with clang_disposeTokens before the translation unit is destroyed.
proc getTokenKind*(argCXToken: CXToken) {.cdecl, dynlib: libclang,
                                       importc: "clang_getTokenKind".}
  ## 
  ## 
  ##  Determine the kind of the given token.
proc getTokenSpelling*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken) {.
    cdecl, dynlib: libclang, importc: "clang_getTokenSpelling".}
  ## 
  ## 
  ##  Determine the spelling of the given token.
  ## 
  ##  The spelling of a token is the textual representation of that token, e.g., the text of an identifier or keyword.
proc getTokenLocation*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken) {.
    cdecl, dynlib: libclang, importc: "clang_getTokenLocation".}
  ## 
  ## 
  ##  Retrieve the source location of the given token.
proc getTokenExtent*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken) {.
    cdecl, dynlib: libclang, importc: "clang_getTokenExtent".}
  ## 
  ## 
  ##  Retrieve a source range that covers the given token.
proc tokenize*(tU: CXTranslationUnit; cxrange: CXSourceRange;
              tokens: ptr[ptr[CXToken]]; numTokens: ptr[cuint]) {.cdecl,
    dynlib: libclang, importc: "clang_tokenize".}
  ## 
  ## 
  ##  Tokenize the source code described by the given range into raw lexical tokens.
  ## 
  ##  **
  ## 
  ##  the translation unit whose text is being tokenized.
  ## 
  ##  **
  ## 
  ##  the source range in which text should be tokenized. All of the tokens produced by tokenization will fall within this source range,
  ## 
  ##  **
  ## 
  ##  this pointer will be set to point to the array of tokens that occur within the given source range. The returned pointer must be freed with clang_disposeTokens() before the translation unit is destroyed.
  ## 
  ##  **
  ## 
  ##  will be set to the number of tokens in the Error: cannot render: rnLiteralBlock array.
  ## 
  ##  
proc annotateTokens*(tU: CXTranslationUnit; tokens: ptr[CXToken]; numTokens: cuint;
                    cursors: ptr[CXCursor]) {.cdecl, dynlib: libclang,
    importc: "clang_annotateTokens".}
  ## 
  ## 
  ##  Annotate the given set of tokens by providing cursors for each token that can be mapped to a specific entity within the abstract syntax tree.
  ## 
  ##  This token-annotation routine is equivalent to invoking clang_getCursor() for the source locations of each of the tokens. The cursors provided are filtered, so that only those cursors that have a direct correspondence to the token are accepted. For example, given a function call Error: cannot render: rnLiteralBlock clang_getCursor() would provide the following cursors:
  ## 
  ##    * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.   * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.   * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'.
  ## 
  ##  Only the first and last of these cursors will occur within the annotate, since the tokens "f" and "x' directly refer to a function and a variable, respectively, but the parentheses are just a small part of the full syntax of the function call expression, which is not provided as an annotation.
  ## 
  ##  **
  ## 
  ##  the translation unit that owns the given tokens.
  ## 
  ##  **
  ## 
  ##  the set of tokens to annotate.
  ## 
  ##  **
  ## 
  ##  the number of tokens in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  an array of Error: cannot render: rnLiteralBlock cursors, whose contents will be replaced with the cursors corresponding to each token.
proc disposeTokens*(tU: CXTranslationUnit; tokens: ptr[CXToken]; numTokens: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_disposeTokens".}
  ## 
  ## 
  ##  Free the given set of tokens.
proc getCursorKindSpelling*(kind: CXCursorKind) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorKindSpelling".}
  ## 
  ## 
  ##  ``
  ## 
  ##  These routines are used for testing and debugging, only, and should not be relied upon.
  ## 
  ##  @{
proc getDefinitionSpellingAndExtent*(argCXCursor: CXCursor; startBuf: ptr[cstring];
                                    endBuf: ptr[cstring]; startLine: ptr[cuint];
                                    startColumn: ptr[cuint]; endLine: ptr[cuint];
                                    endColumn: ptr[cuint]) {.cdecl,
    dynlib: libclang, importc: "clang_getDefinitionSpellingAndExtent".}
proc enableStackTraces*() {.cdecl, dynlib: libclang,
                          importc: "clang_enableStackTraces".}
proc executeOnThread*(fn: ptr[proc (a0: pointer): void {.cdecl.}]; user_data: pointer;
                     stack_size: cuint) {.cdecl, dynlib: libclang,
                                        importc: "clang_executeOnThread".}
type
  CXCompletionString = distinct pointer
type
  CXCompletionResult = object
    cursorKind*: CXCursorKind
    completionString*: CXCompletionString

proc getCompletionChunkKind*(completion_string: CXCompletionString;
                            chunk_number: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getCompletionChunkKind".}
  ## 
  ## 
  ##  Determine the kind of a particular chunk within a completion string.
  ## 
  ##  **
  ## 
  ##  the completion string to query.
  ## 
  ##  **
  ## 
  ##  the 0-based index of the chunk in the completion string.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the kind of the chunk at the index Error: cannot render: rnLiteralBlock 
proc getCompletionChunkText*(completion_string: CXCompletionString;
                            chunk_number: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getCompletionChunkText".}
  ## 
  ## 
  ##  Retrieve the text associated with a particular chunk within a completion string.
  ## 
  ##  **
  ## 
  ##  the completion string to query.
  ## 
  ##  **
  ## 
  ##  the 0-based index of the chunk in the completion string.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the text associated with the chunk at index Error: cannot render: rnLiteralBlock 
proc getCompletionChunkCompletionString*(completion_string: CXCompletionString;
                                        chunk_number: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionChunkCompletionString".}
  ## 
  ## 
  ##  Retrieve the completion string associated with a particular chunk within a completion string.
  ## 
  ##  **
  ## 
  ##  the completion string to query.
  ## 
  ##  **
  ## 
  ##  the 0-based index of the chunk in the completion string.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the completion string associated with the chunk at index Error: cannot render: rnLiteralBlock 
proc getNumCompletionChunks*(completion_string: CXCompletionString) {.cdecl,
    dynlib: libclang, importc: "clang_getNumCompletionChunks".}
  ## 
  ## 
  ##  Retrieve the number of chunks in the given code-completion string.
proc getCompletionPriority*(completion_string: CXCompletionString) {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionPriority".}
  ## 
  ## 
  ##  Determine the priority of this code completion.
  ## 
  ##  The priority of a code completion indicates how likely it is that this particular completion is the completion that the user will select. The priority is selected by various internal heuristics.
  ## 
  ##  **
  ## 
  ##  The completion string to query.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The priority of this completion string. Smaller values indicate higher-priority (more likely) completions.
proc getCompletionAvailability*(completion_string: CXCompletionString) {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionAvailability".}
  ## 
  ## 
  ##  Determine the availability of the entity that this code-completion string refers to.
  ## 
  ##  **
  ## 
  ##  The completion string to query.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The availability of the completion string.
proc getCompletionNumAnnotations*(completion_string: CXCompletionString) {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionNumAnnotations".}
  ## 
  ## 
  ##  Retrieve the number of annotations associated with the given completion string.
  ## 
  ##  **
  ## 
  ##  the completion string to query.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the number of annotations associated with the given completion string.
proc getCompletionAnnotation*(completion_string: CXCompletionString;
                             annotation_number: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getCompletionAnnotation".}
  ## 
  ## 
  ##  Retrieve the annotation associated with the given completion string.
  ## 
  ##  **
  ## 
  ##  the completion string to query.
  ## 
  ##  **
  ## 
  ##  the 0-based index of the annotation of the completion string.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  annotation string associated with the completion at index Error: cannot render: rnLiteralBlock or a NULL string if that annotation is not available.
proc getCompletionParent*(completion_string: CXCompletionString;
                         kind: ptr[CXCursorKind]) {.cdecl, dynlib: libclang,
    importc: "clang_getCompletionParent".}
  ## 
  ## 
  ##  Retrieve the parent context of the given completion string.
  ## 
  ##  The parent context of a completion string is the semantic parent of the declaration (if any) that the code completion represents. For example, a code completion for an Objective-C method would have the method's class or protocol as its context.
  ## 
  ##  **
  ## 
  ##  The code completion string whose parent is being queried.
  ## 
  ##  **
  ## 
  ##  DEPRECATED: always set to CXCursor_NotImplemented if non-NULL.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The name of the completion parent, e.g., "NSObject" if the completion string represents a method in the NSObject class.
proc getCompletionBriefComment*(completion_string: CXCompletionString) {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionBriefComment".}
  ## 
  ## 
  ##  Retrieve the brief documentation comment attached to the declaration that corresponds to the given completion string.
proc getCursorCompletionString*(cursor: CXCursor) {.cdecl, dynlib: libclang,
    importc: "clang_getCursorCompletionString".}
  ## 
  ## 
  ##  Retrieve a completion string for an arbitrary declaration or macro definition cursor.
  ## 
  ##  **
  ## 
  ##  The cursor to query.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  A non-context-sensitive completion string for declaration and macro definition cursors, or NULL for other kinds of cursors.
type
  CXCodeCompleteResults = object
    results*: ptr[CXCompletionResult]
    numResults*: cuint

proc getCompletionNumFixIts*(results: ptr[CXCodeCompleteResults];
                            completion_index: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_getCompletionNumFixIts".}
  ## 
  ## 
  ##  Retrieve the number of fix-its for the given completion index.
  ## 
  ##  Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts option was set.
  ## 
  ##  **
  ## 
  ##  The structure keeping all completion results
  ## 
  ##  **
  ## 
  ##  The index of the completion
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The number of fix-its which must be applied before the completion at completion_index can be applied
proc getCompletionFixIt*(results: ptr[CXCodeCompleteResults];
                        completion_index: cuint; fixit_index: cuint;
                        replacement_range: ptr[CXSourceRange]) {.cdecl,
    dynlib: libclang, importc: "clang_getCompletionFixIt".}
  ## 
  ## 
  ##  Fix-its that *must* be applied before inserting the text for the corresponding completion.
  ## 
  ##  By default, clang_codeCompleteAt() only returns completions with empty fix-its. Extra completions with non-empty fix-its should be explicitly requested by setting CXCodeComplete_IncludeCompletionsWithFixIts.
  ## 
  ##  For the clients to be able to compute position of the cursor after applying fix-its, the following conditions are guaranteed to hold for replacement_range of the stored fix-its:  - Ranges in the fix-its are guaranteed to never contain the completion  point (or identifier under completion point, if any) inside them, except  at the start or at the end of the range.  - If a fix-it range starts or ends with completion point (or starts or  ends after the identifier under completion point), it will contain at  least one character. It allows to unambiguously recompute completion  point after applying the fix-it.
  ## 
  ##  The intuition is that provided fix-its change code around the identifier we complete, but are not allowed to touch the identifier itself or the completion point. One example of completions with corrections are the ones replacing '.' with '->' and vice versa:
  ## 
  ##  std::unique_ptr<std::vector<int>> vec_ptr; In 'vec_ptr.^', one of the completions is 'push_back', it requires replacing '.' with '->'. In 'vec_ptr->^', one of the completions is 'release', it requires replacing '->' with '.'.
  ## 
  ##  **
  ## 
  ##  The structure keeping all completion results
  ## 
  ##  **
  ## 
  ##  The index of the completion
  ## 
  ##  **
  ## 
  ##  The index of the fix-it for the completion at completion_index
  ## 
  ##  **
  ## 
  ##  The fix-it range that must be replaced before the completion at completion_index can be applied
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  The fix-it string that must replace the code at replacement_range before the completion at completion_index can be applied
proc defaultCodeCompleteOptions*() {.cdecl, dynlib: libclang,
                                   importc: "clang_defaultCodeCompleteOptions".}
  ## 
  ## 
  ##  Returns a default set of code-completion options that can be passed toError: cannot render: rnLiteralBlock 
proc codeCompleteAt*(tU: CXTranslationUnit; complete_filename: cstring;
                    complete_line: cuint; complete_column: cuint;
                    unsaved_files: ptr[CXUnsavedFile]; num_unsaved_files: cuint;
                    options: cuint) {.cdecl, dynlib: libclang,
                                    importc: "clang_codeCompleteAt".}
  ## 
  ## 
  ##  Perform code completion at a given location in a translation unit.
  ## 
  ##  This function performs code completion at a particular file, line, and column within source code, providing results that suggest potential code snippets based on the context of the completion. The basic model for code completion is that Clang will parse a complete source file, performing syntax checking up to the location where code-completion has been requested. At that point, a special code-completion token is passed to the parser, which recognizes this token and determines, based on the current location in the C/Objective-C/C++ grammar and the state of semantic analysis, what completions to provide. These completions are returned via a new Error: cannot render: rnLiteralBlock structure.
  ## 
  ##  Code completion itself is meant to be triggered by the client when the user types punctuation characters or whitespace, at which point the code-completion location will coincide with the cursor. For example, if Error: cannot render: rnLiteralBlock is a pointer, code-completion might be triggered after the "-" and then after the ">" in Error: cannot render: rnLiteralBlock When the code-completion location is after the ">", the completion results will provide, e.g., the members of the struct that "p" points to. The client is responsible for placing the cursor at the beginning of the token currently being typed, then filtering the results based on the contents of the token. For example, when code-completing for the expression Error: cannot render: rnLiteralBlock the client should provide the location just after the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the client can filter the results based on the current token text ("get"), only showing those results that start with "get". The intent of this interface is to separate the relatively high-latency acquisition of code-completion results from the filtering of results on a per-character basis, which must have a lower latency.
  ## 
  ##  **
  ## 
  ##  The translation unit in which code-completion should occur. The source files for this translation unit need not be completely up-to-date (and the contents of those source files may be overridden via Error: cannot render: rnLiteralBlock Cursors referring into the translation unit may be invalidated by this invocation.
  ## 
  ##  **
  ## 
  ##  The name of the source file where code completion should be performed. This filename may be any file included in the translation unit.
  ## 
  ##  **
  ## 
  ##  The line at which code-completion should occur.
  ## 
  ##  **
  ## 
  ##  The column at which code-completion should occur. Note that the column should point just after the syntactic construct that initiated code completion, and not in the middle of a lexical token.
  ## 
  ##  **
  ## 
  ##  the Files that have not yet been saved to disk but may be required for parsing or code completion, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns.
  ## 
  ##  **
  ## 
  ##  The number of unsaved file entries in Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  Extra options that control the behavior of code completion, expressed as a bitwise OR of the enumerators of the CXCodeComplete_Flags enumeration. The Error: cannot render: rnLiteralBlock function returns a default set of code-completion options.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  If successful, a new Error: cannot render: rnLiteralBlock structure containing code-completion results, which should eventually be freed with Error: cannot render: rnLiteralBlock If code completion fails, returns NULL.
proc sortCodeCompletionResults*(results: ptr[CXCompletionResult]; numResults: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_sortCodeCompletionResults".}
  ## 
  ## 
  ##  Sort the code-completion results in case-insensitive alphabetical order.
  ## 
  ##  **
  ## 
  ##  The set of results to sort. **
  ## 
  ##  The number of results in Error: cannot render: rnLiteralBlock 
proc disposeCodeCompleteResults*(results: ptr[CXCodeCompleteResults]) {.cdecl,
    dynlib: libclang, importc: "clang_disposeCodeCompleteResults".}
  ## 
  ## 
  ##  Free the given set of code-completion results.
proc codeCompleteGetNumDiagnostics*(results: ptr[CXCodeCompleteResults]) {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteGetNumDiagnostics".}
  ## 
  ## 
  ##  Determine the number of diagnostics produced prior to the location where code completion was performed.
proc codeCompleteGetDiagnostic*(results: ptr[CXCodeCompleteResults]; index: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_codeCompleteGetDiagnostic".}
  ## 
  ## 
  ##  Retrieve a diagnostic associated with the given code completion.
  ## 
  ##  **
  ## 
  ##  the code completion results to query. **
  ## 
  ##  the zero-based diagnostic number to retrieve.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
proc codeCompleteGetContexts*(results: ptr[CXCodeCompleteResults]) {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteGetContexts".}
  ## 
  ## 
  ##  Determines what completions are appropriate for the context the given code completion.
  ## 
  ##  **
  ## 
  ##  the code completion results to query
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the kinds of completions that are appropriate for use along with the given code completion results.
proc codeCompleteGetContainerKind*(results: ptr[CXCodeCompleteResults];
                                  isIncomplete: ptr[cuint]) {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteGetContainerKind".}
  ## 
  ## 
  ##  Returns the cursor kind for the container for the current code completion context. The container is only guaranteed to be set for contexts where a container exists (i.e. member accesses or Objective-C message sends); if there is not a container, this function will return CXCursor_InvalidCode.
  ## 
  ##  **
  ## 
  ##  the code completion results to query
  ## 
  ##  **
  ## 
  ##  on return, this value will be false if Clang has complete information about the container. If Clang does not have complete information, this value will be true.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the container kind, or CXCursor_InvalidCode if there is not a container
proc codeCompleteGetContainerUSR*(results: ptr[CXCodeCompleteResults]) {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteGetContainerUSR".}
  ## 
  ## 
  ##  Returns the USR for the container for the current code completion context. If there is not a container for the current context, this function will return the empty string.
  ## 
  ##  **
  ## 
  ##  the code completion results to query
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the USR for the container
proc codeCompleteGetObjCSelector*(results: ptr[CXCodeCompleteResults]) {.cdecl,
    dynlib: libclang, importc: "clang_codeCompleteGetObjCSelector".}
  ## 
  ## 
  ##  Returns the currently-entered selector for an Objective-C message send, formatted like "initWithFoo:bar:". Only guaranteed to return a non-empty string for CXCompletionContext_ObjCInstanceMessage and CXCompletionContext_ObjCClassMessage.
  ## 
  ##  **
  ## 
  ##  the code completion results to query
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the selector (or partial selector) that has been entered thus far for an Objective-C message send.
proc getClangVersion*() {.cdecl, dynlib: libclang, importc: "clang_getClangVersion".}
  ## 
  ## 
  ##  Return a version string, suitable for showing to a user, but not        intended to be parsed (the format is not guaranteed to be stable).
proc toggleCrashRecovery*(isEnabled: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_toggleCrashRecovery".}
  ## 
  ## 
  ##  Enable/disable crash recovery.
  ## 
  ##  **
  ## 
  ##  Flag to indicate if crash recovery is enabled.  A non-zero        value enables crash recovery, while 0 disables it.
type
  CXInclusionVisitor = distinct ptr[proc (a0: pointer; a1: ptr[CXSourceLocation];
                                      a2: cuint; a3: pointer): void {.cdecl.}]
proc getInclusions*(tu: CXTranslationUnit; visitor: CXInclusionVisitor;
                   client_data: CXClientData) {.cdecl, dynlib: libclang,
    importc: "clang_getInclusions".}
  ## 
  ## 
  ##  Visit the set of preprocessor inclusions in a translation unit.   The visitor function is called with the provided data for every included   file.  This does not include headers included by the PCH file (unless one   is inspecting the inclusions in the PCH file itself).
type
  CXEvalResultKind = enum
    erkInt = 1, erkFloat = 2, erkObjCStrLiteral = 3, erkStrLiteral = 4, erkCFStr = 5,
    erkOther = 6, erkUnExposed = 0
type
  CXEvalResult = distinct pointer
proc Evaluate*(c: CXCursor) {.cdecl, dynlib: libclang,
                           importc: "clang_Cursor_Evaluate".}
  ## 
  ## 
  ##  If cursor is a statement declaration tries to evaluate the statement and if its variable, tries to evaluate its initializer, into its corresponding type.
proc getKind*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                              importc: "clang_EvalResult_getKind".}
  ## 
  ## 
  ##  Returns the kind of the evaluated result.
proc getAsInt*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                               importc: "clang_EvalResult_getAsInt".}
  ## 
  ## 
  ##  Returns the evaluation result as integer if the kind is Int.
proc getAsLongLong*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                                    importc: "clang_EvalResult_getAsLongLong".}
  ## 
  ## 
  ##  Returns the evaluation result as a long long integer if the kind is Int. This prevents overflows that may happen if the result is returned with clang_EvalResult_getAsInt.
proc isUnsignedInt*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                                    importc: "clang_EvalResult_isUnsignedInt".}
  ## 
  ## 
  ##  Returns a non-zero value if the kind is Int and the evaluation result resulted in an unsigned integer.
proc getAsUnsigned*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                                    importc: "clang_EvalResult_getAsUnsigned".}
  ## 
  ## 
  ##  Returns the evaluation result as an unsigned integer if the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
proc getAsDouble*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                                  importc: "clang_EvalResult_getAsDouble".}
  ## 
  ## 
  ##  Returns the evaluation result as double if the kind is double.
proc getAsStr*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                               importc: "clang_EvalResult_getAsStr".}
  ## 
  ## 
  ##  Returns the evaluation result as a constant string if the kind is other than Int or float. User must not free this pointer, instead call clang_EvalResult_dispose on the CXEvalResult returned by clang_Cursor_Evaluate.
proc dispose*(e: CXEvalResult) {.cdecl, dynlib: libclang,
                              importc: "clang_EvalResult_dispose".}
  ## 
  ## 
  ##  Disposes the created Eval memory.
type
  CXRemapping = distinct pointer
proc getRemappings*(path: cstring) {.cdecl, dynlib: libclang,
                                  importc: "clang_getRemappings".}
  ## 
  ## 
  ##  Retrieve a remapping.
  ## 
  ##  **
  ## 
  ##  the path that contains metadata about remappings.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the requested remapping. This remapping must be freed via a call to Error: cannot render: rnLiteralBlock Can return NULL if an error occurred.
proc getRemappingsFromFileList*(filePaths: ptr[cstring]; numFiles: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_getRemappingsFromFileList".}
  ## 
  ## 
  ##  Retrieve a remapping.
  ## 
  ##  **
  ## 
  ##  pointer to an array of file paths containing remapping info.
  ## 
  ##  **
  ## 
  ##  number of file paths.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the requested remapping. This remapping must be freed via a call to Error: cannot render: rnLiteralBlock Can return NULL if an error occurred.
proc remap_getNumFiles*(argCXRemapping: CXRemapping) {.cdecl, dynlib: libclang,
    importc: "clang_remap_getNumFiles".}
  ## 
  ## 
  ##  Determine the number of remappings.
proc remap_getFilenames*(argCXRemapping: CXRemapping; index: cuint;
                        original: ptr[CXString]; transformed: ptr[CXString]) {.
    cdecl, dynlib: libclang, importc: "clang_remap_getFilenames".}
  ## 
  ## 
  ##  Get the original and the associated filename from the remapping.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to the original filename.
  ## 
  ##  **
  ## 
  ##  If non-NULL, will be set to the filename that the original is associated with.
proc remap_dispose*(argCXRemapping: CXRemapping) {.cdecl, dynlib: libclang,
    importc: "clang_remap_dispose".}
  ## 
  ## 
  ##  Dispose the remapping.
type
  CXCursorAndRangeVisitor = object
    context*: pointer
    visit*: ptr[proc (a0: pointer; a1: CXCursor; a2: CXSourceRange): CXVisitorResult {.
        cdecl.}]

type
  CXResult = enum
    rSuccess = 0,               ## 
               ## 
               ##  Function returned successfully.
    rInvalid = 1,               ## 
               ## 
               ##  One of the parameters was invalid for the function.
    rVisitBreak = 2             ## 
                 ## 
                 ##  The function was terminated by a callback (e.g. it returned CXVisit_Break)
proc findReferencesInFile*(cursor: CXCursor; file: CXFile;
                          visitor: CXCursorAndRangeVisitor) {.cdecl,
    dynlib: libclang, importc: "clang_findReferencesInFile".}
  ## 
  ## 
  ##  Find references of a declaration in a specific file.
  ## 
  ##  **
  ## 
  ##  pointing to a declaration or a reference of one.
  ## 
  ##  **
  ## 
  ##  to search for references.
  ## 
  ##  **
  ## 
  ##  callback that will receive pairs of CXCursor/CXSourceRange for each reference found. The CXSourceRange will point inside the file; if the reference is inside a macro (and not a macro argument) the CXSourceRange will be invalid.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  one of the CXResult enumerators.
proc findIncludesInFile*(tU: CXTranslationUnit; file: CXFile;
                        visitor: CXCursorAndRangeVisitor) {.cdecl,
    dynlib: libclang, importc: "clang_findIncludesInFile".}
  ## 
  ## 
  ##  Find #import/#include directives in a specific file.
  ## 
  ##  **
  ## 
  ##  translation unit containing the file to query.
  ## 
  ##  **
  ## 
  ##  to search for #import/#include directives.
  ## 
  ##  **
  ## 
  ##  callback that will receive pairs of CXCursor/CXSourceRange for each directive found.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  one of the CXResult enumerators.
type
  CXIdxClientFile = distinct pointer
type
  CXIdxClientEntity = distinct pointer
type
  CXIdxClientContainer = distinct pointer
type
  CXIdxClientASTFile = distinct pointer
type
  CXIdxLoc = object
    ptr_data*: array[2, pointer]
    int_data*: cuint

type
  CXIdxIncludedFileInfo = object
    hashLoc*: CXIdxLoc
    filename*: cstring
    file*: CXFile
    isImport*: int
    isAngled*: int
    isModuleImport*: int

type
  CXIdxImportedASTFileInfo = object
    file*: CXFile
    module*: CXModule
    loc*: CXIdxLoc
    isImplicit*: int

type
  CXIdxEntityKind = enum
    iekUnexposed = 0, iekTypedef = 1, iekFunction = 2, iekVariable = 3, iekField = 4,
    iekEnumConstant = 5, iekObjCClass = 6, iekObjCProtocol = 7, iekObjCCategory = 8,
    iekObjCInstanceMethod = 9, iekObjCClassMethod = 10, iekObjCProperty = 11,
    iekObjCIvar = 12, iekEnum = 13, iekStruct = 14, iekUnion = 15, iekCXXClass = 16,
    iekCXXNamespace = 17, iekCXXNamespaceAlias = 18, iekCXXStaticVariable = 19,
    iekCXXStaticMethod = 20, iekCXXInstanceMethod = 21, iekCXXConstructor = 22,
    iekCXXDestructor = 23, iekCXXConversionFunction = 24, iekCXXTypeAlias = 25,
    iekCXXInterface = 26
type
  CXIdxEntityLanguage = enum
    ielNone = 0, ielC = 1, ielObjC = 2, ielCXX = 3, ielSwift = 4
type
  CXIdxEntityCXXTemplateKind = enum
    ietkNonTemplate = 0, ietkTemplate = 1, ietkTemplatePartialSpecialization = 2,
    ietkTemplateSpecialization = 3
type
  CXIdxAttrKind = enum
    iakUnexposed = 0, iakIBAction = 1, iakIBOutlet = 2, iakIBOutletCollection = 3
type
  CXIdxAttrInfo = object
    kind*: CXIdxAttrKind
    cursor*: CXCursor
    loc*: CXIdxLoc

type
  CXIdxEntityInfo = object
    kind*: CXIdxEntityKind
    templateKind*: CXIdxEntityCXXTemplateKind
    lang*: CXIdxEntityLanguage
    name*: cstring
    uSR*: cstring
    cursor*: CXCursor
    attributes*: ptr[ptr[const CXIdxAttrInfo]]
    numAttributes*: cuint

type
  CXIdxContainerInfo = object
    cursor*: CXCursor

type
  CXIdxIBOutletCollectionAttrInfo = object
    attrInfo*: ptr[const CXIdxAttrInfo]
    objcClass*: ptr[const CXIdxEntityInfo]
    classCursor*: CXCursor
    classLoc*: CXIdxLoc

type
  CXIdxDeclInfoFlags = enum
    idifFlag_Skipped = 1
type
  CXIdxDeclInfo = object
    entityInfo*: ptr[const CXIdxEntityInfo]
    cursor*: CXCursor
    loc*: CXIdxLoc
    semanticContainer*: ptr[const CXIdxContainerInfo]
    lexicalContainer*: ptr[const CXIdxContainerInfo]
    isRedeclaration*: int
    isDefinition*: int
    isContainer*: int
    declAsContainer*: ptr[const CXIdxContainerInfo]
    isImplicit*: int
    attributes*: ptr[ptr[const CXIdxAttrInfo]]
    numAttributes*: cuint
    flags*: cuint

type
  CXIdxObjCContainerKind = enum
    iocckForwardRef = 0, iocckInterface = 1, iocckImplementation = 2
type
  CXIdxObjCContainerDeclInfo = object
    declInfo*: ptr[const CXIdxDeclInfo]
    kind*: CXIdxObjCContainerKind

type
  CXIdxBaseClassInfo = object
    base*: ptr[const CXIdxEntityInfo]
    cursor*: CXCursor
    loc*: CXIdxLoc

type
  CXIdxObjCProtocolRefInfo = object
    protocol*: ptr[const CXIdxEntityInfo]
    cursor*: CXCursor
    loc*: CXIdxLoc

type
  CXIdxObjCProtocolRefListInfo = object
    protocols*: ptr[ptr[const CXIdxObjCProtocolRefInfo]]
    numProtocols*: cuint

type
  CXIdxObjCInterfaceDeclInfo = object
    containerInfo*: ptr[const CXIdxObjCContainerDeclInfo]
    superInfo*: ptr[const CXIdxBaseClassInfo]
    protocols*: ptr[const CXIdxObjCProtocolRefListInfo]

type
  CXIdxObjCCategoryDeclInfo = object
    containerInfo*: ptr[const CXIdxObjCContainerDeclInfo]
    objcClass*: ptr[const CXIdxEntityInfo]
    classCursor*: CXCursor
    classLoc*: CXIdxLoc
    protocols*: ptr[const CXIdxObjCProtocolRefListInfo]

type
  CXIdxObjCPropertyDeclInfo = object
    declInfo*: ptr[const CXIdxDeclInfo]
    getter*: ptr[const CXIdxEntityInfo]
    setter*: ptr[const CXIdxEntityInfo]

type
  CXIdxCXXClassDeclInfo = object
    declInfo*: ptr[const CXIdxDeclInfo]
    bases*: ptr[ptr[const CXIdxBaseClassInfo]]
    numBases*: cuint

type
  CXIdxEntityRefKind = enum
    ierkDirect = 1,             ## 
                 ## 
                 ##  The entity is referenced directly in user's code.
    ierkImplicit = 2            ## 
                  ## 
                  ##  An implicit reference, e.g. a reference of an Objective-C method via the dot syntax.
type
  CXSymbolRole = enum
    srNone = 0, srDeclaration, srDefinition, srReference, srRead, srWrite, srCall,
    srDynamic, srAddressOf, srImplicit
type
  CXIdxEntityRefInfo = object
    kind*: CXIdxEntityRefKind
    cursor*: CXCursor
    loc*: CXIdxLoc
    referencedEntity*: ptr[const CXIdxEntityInfo]
    parentEntity*: ptr[const CXIdxEntityInfo]
    container*: ptr[const CXIdxContainerInfo]
    role*: CXSymbolRole

type
  IndexerCallbacks = object
    abortQuery*: ptr[proc (a0: CXClientData; a1: pointer): int {.cdecl.}]
    diagnostic*: ptr[proc (a0: CXClientData; a1: CXDiagnosticSet; a2: pointer): void {.
        cdecl.}]
    enteredMainFile*: ptr[proc (a0: CXClientData; a1: CXFile; a2: pointer): CXIdxClientFile {.
        cdecl.}]
    ppIncludedFile*: ptr[proc (a0: CXClientData;
                             a1: ptr[const CXIdxIncludedFileInfo]): CXIdxClientFile {.
        cdecl.}]
    importedASTFile*: ptr[proc (a0: CXClientData;
                              a1: ptr[const CXIdxImportedASTFileInfo]): CXIdxClientASTFile {.
        cdecl.}]
    startedTranslationUnit*: ptr[proc (a0: CXClientData; a1: pointer): CXIdxClientContainer {.
        cdecl.}]
    indexDeclaration*: ptr[proc (a0: CXClientData; a1: ptr[const CXIdxDeclInfo]): void {.
        cdecl.}]
    indexEntityReference*: ptr[proc (a0: CXClientData;
                                   a1: ptr[const CXIdxEntityRefInfo]): void {.cdecl.}]

proc index_isEntityObjCContainerKind*(argCXIdxEntityKind: CXIdxEntityKind) {.cdecl,
    dynlib: libclang, importc: "clang_index_isEntityObjCContainerKind".}
proc index_getObjCContainerDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getObjCContainerDeclInfo".}
proc index_getObjCInterfaceDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getObjCInterfaceDeclInfo".}
proc index_getObjCCategoryDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getObjCCategoryDeclInfo".}
proc index_getObjCProtocolRefListInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getObjCProtocolRefListInfo".}
proc index_getObjCPropertyDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getObjCPropertyDeclInfo".}
proc index_getIBOutletCollectionAttrInfo*(
    argCXIdxAttrInfo: ptr[const CXIdxAttrInfo]) {.cdecl, dynlib: libclang,
    importc: "clang_index_getIBOutletCollectionAttrInfo".}
proc index_getCXXClassDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getCXXClassDeclInfo".}
proc index_getClientContainer*(argCXIdxContainerInfo: ptr[const CXIdxContainerInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getClientContainer".}
  ## 
  ## 
  ##  For retrieving a custom CXIdxClientContainer attached to a container.
proc index_setClientContainer*(argCXIdxContainerInfo: ptr[const CXIdxContainerInfo];
                              argCXIdxClientContainer: CXIdxClientContainer) {.
    cdecl, dynlib: libclang, importc: "clang_index_setClientContainer".}
  ## 
  ## 
  ##  For setting a custom CXIdxClientContainer attached to a container.
proc index_getClientEntity*(argCXIdxEntityInfo: ptr[const CXIdxEntityInfo]) {.
    cdecl, dynlib: libclang, importc: "clang_index_getClientEntity".}
  ## 
  ## 
  ##  For retrieving a custom CXIdxClientEntity attached to an entity.
proc index_setClientEntity*(argCXIdxEntityInfo: ptr[const CXIdxEntityInfo];
                           argCXIdxClientEntity: CXIdxClientEntity) {.cdecl,
    dynlib: libclang, importc: "clang_index_setClientEntity".}
  ## 
  ## 
  ##  For setting a custom CXIdxClientEntity attached to an entity.
type
  CXIndexAction = distinct pointer
proc IndexAction_create*(cIdx: CXIndex) {.cdecl, dynlib: libclang,
                                       importc: "clang_IndexAction_create".}
  ## 
  ## 
  ##  An indexing action/session, to be applied to one or multiple translation units.
  ## 
  ##  **
  ## 
  ##  The index object with which the index action will be associated.
proc dispose*(argCXIndexAction: CXIndexAction) {.cdecl, dynlib: libclang,
    importc: "clang_IndexAction_dispose".}
  ## 
  ## 
  ##  Destroy the given index action.
  ## 
  ##  The index action must not be destroyed until all of the translation units created within that index action have been destroyed.
type
  CXIndexOptFlags = enum
    iofNone = 0,                ## 
              ## 
              ##  Used to indicate that no special indexing options are needed.
    iofSuppressRedundantRefs = 1, ## 
                               ## 
                               ##  Used to indicate that IndexerCallbacks#indexEntityReference should be invoked for only one reference of an entity per source file that does not also include a declaration/definition of the entity.
    iofIndexFunctionLocalSymbols = 2, ## 
                                   ## 
                                   ##  Function-local symbols should be indexed. If this is not set function-local symbols will be ignored.
    iofIndexImplicitTemplateInstantiations = 4, ## 
                                             ## 
                                             ##  Implicit function/class template instantiations should be indexed. If this is not set, implicit instantiations will be ignored.
    iofSuppressWarnings = 8,    ## 
                          ## 
                          ##  Suppress all compiler warnings when parsing for indexing.
    iofSkipParsedBodiesInSession = 16 ## 
                                   ## 
                                   ##  Skip a function/method body that was already parsed during an indexing session associated with a Error: cannot render: rnLiteralBlock object. Bodies in system headers are always skipped.
proc indexSourceFile*(argCXIndexAction: CXIndexAction; client_data: CXClientData;
                     index_callbacks: ptr[IndexerCallbacks];
                     index_callbacks_size: cuint; index_options: cuint;
                     source_filename: cstring; command_line_args: ptr[cstring];
                     num_command_line_args: int;
                     unsaved_files: ptr[CXUnsavedFile]; num_unsaved_files: cuint;
                     out_TU: ptr[CXTranslationUnit]; tU_options: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_indexSourceFile".}
  ## 
  ## 
  ##  Index the given source file and the translation unit corresponding to that file via callbacks implemented through #IndexerCallbacks.
  ## 
  ##  **
  ## 
  ##  pointer data supplied by the client, which will be passed to the invoked callbacks.
  ## 
  ##  **
  ## 
  ##  Pointer to indexing callbacks that the client implements.
  ## 
  ##  **
  ## 
  ##  Size of #IndexerCallbacks structure that gets passed in index_callbacks.
  ## 
  ##  **
  ## 
  ##  A bitmask of options that affects how indexing is performed. This should be a bitwise OR of the CXIndexOpt_XXX flags.
  ## 
  ##  **
  ## 
  ##  pointer to store a Error: cannot render: rnLiteralBlock that can be reused after indexing is finished. Set to Error: cannot render: rnLiteralBlock if you do not require it.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  0 on success or if there were errors from which the compiler could recover.  If there is a failure from which there is no recovery, returns a non-zero Error: cannot render: rnLiteralBlock
  ## 
  ##  The rest of the parameters are the same as #clang_parseTranslationUnit.
proc indexSourceFileFullArgv*(argCXIndexAction: CXIndexAction;
                             client_data: CXClientData;
                             index_callbacks: ptr[IndexerCallbacks];
                             index_callbacks_size: cuint; index_options: cuint;
                             source_filename: cstring;
                             command_line_args: ptr[cstring];
                             num_command_line_args: int;
                             unsaved_files: ptr[CXUnsavedFile];
                             num_unsaved_files: cuint;
                             out_TU: ptr[CXTranslationUnit]; tU_options: cuint) {.
    cdecl, dynlib: libclang, importc: "clang_indexSourceFileFullArgv".}
  ## 
  ## 
  ##  Same as clang_indexSourceFile but requires a full command line for Error: cannot render: rnLiteralBlock including argv[0]. This is useful if the standard library paths are relative to the binary.
proc indexTranslationUnit*(argCXIndexAction: CXIndexAction;
                          client_data: CXClientData;
                          index_callbacks: ptr[IndexerCallbacks];
                          index_callbacks_size: cuint; index_options: cuint;
                          argCXTranslationUnit: CXTranslationUnit) {.cdecl,
    dynlib: libclang, importc: "clang_indexTranslationUnit".}
  ## 
  ## 
  ##  Index the given translation unit via callbacks implemented through #IndexerCallbacks.
  ## 
  ##  The order of callback invocations is not guaranteed to be the same as when indexing a source file. The high level order will be:
  ## 
  ##    -Preprocessor callbacks invocations   -Declaration/reference callbacks invocations   -Diagnostic callback invocations
  ## 
  ##  The parameters are the same as #clang_indexSourceFile.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  If there is a failure from which there is no recovery, returns non-zero, otherwise returns 0.
proc indexLoc_getFileLocation*(loc: CXIdxLoc; indexFile: ptr[CXIdxClientFile];
                              file: ptr[CXFile]; line: ptr[cuint];
                              column: ptr[cuint]; offset: ptr[cuint]) {.cdecl,
    dynlib: libclang, importc: "clang_indexLoc_getFileLocation".}
  ## 
  ## 
  ##  Retrieve the CXIdxFile, file, line, column, and offset represented by the given CXIdxLoc.
  ## 
  ##  If the location refers into a macro expansion, retrieves the location of the macro expansion and if it refers into a macro argument retrieves the location of the argument.
proc indexLoc_getCXSourceLocation*(loc: CXIdxLoc) {.cdecl, dynlib: libclang,
    importc: "clang_indexLoc_getCXSourceLocation".}
  ## 
  ## 
  ##  Retrieve the CXSourceLocation represented by the given CXIdxLoc.
type
  CXFieldVisitor = distinct ptr[proc (a0: CXCursor; a1: pointer): CXVisitorResult {.cdecl.}]
proc visitFields*(t: CXType; visitor: CXFieldVisitor; client_data: CXClientData) {.
    cdecl, dynlib: libclang, importc: "clang_Type_visitFields".}
  ## 
  ## 
  ##  Visit the fields of a particular type.
  ## 
  ##  This function visits all the direct fields of the given cursor, invoking the given Error: cannot render: rnLiteralBlock function with the cursors of each visited field. The traversal may be ended prematurely, if the visitor returns Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  the record type whose field may be visited.
  ## 
  ##  **
  ## 
  ##  the visitor function that will be invoked for each field of Error: cannot render: rnLiteralBlock
  ## 
  ##  **
  ## 
  ##  pointer data supplied by the client, which will be passed to the visitor each time it is invoked.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  a non-zero value if the traversal was terminated prematurely by the visitor returning Error: cannot render: rnLiteralBlock 
type
  CXComment = object
    aSTNode*: pointer
    translationUnit*: CXTranslationUnit

proc getParsedComment*(c: CXCursor) {.cdecl, dynlib: libclang,
                                   importc: "clang_Cursor_getParsedComment".}
  ## 
  ## 
  ##  Given a cursor that represents a documentable entity (e.g., declaration), return the associated parsed comment as a Error: cannot render: rnLiteralBlock AST node.
proc getKind*(comment: CXComment) {.cdecl, dynlib: libclang,
                                 importc: "clang_Comment_getKind".}
  ## 
  ## 
  ##  **
  ## 
  ##  AST node of any kind.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the type of the AST node.
proc getNumChildren*(comment: CXComment) {.cdecl, dynlib: libclang,
                                        importc: "clang_Comment_getNumChildren".}
  ## 
  ## 
  ##  **
  ## 
  ##  AST node of any kind.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  number of children of the AST node.
proc getChild*(comment: CXComment; childIdx: cuint) {.cdecl, dynlib: libclang,
    importc: "clang_Comment_getChild".}
  ## 
  ## 
  ##  **
  ## 
  ##  AST node of any kind.
  ## 
  ##  **
  ## 
  ##  child index (zero-based).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the specified child of the AST node.
proc isWhitespace*(comment: CXComment) {.cdecl, dynlib: libclang,
                                      importc: "clang_Comment_isWhitespace".}
  ## 
  ## 
  ##  A Error: cannot render: rnLiteralBlock node is considered whitespace if it contains only Error: cannot render: rnLiteralBlock nodes that are empty or whitespace.
  ## 
  ##  Other AST nodes (except Error: cannot render: rnLiteralBlock and Error: cannot render: rnLiteralBlock are never considered whitespace.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if Error: cannot render: rnLiteralBlock is whitespace.
proc InlineContentComment_hasTrailingNewline*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_InlineContentComment_hasTrailingNewline".}
  ## 
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if Error: cannot render: rnLiteralBlock is inline content and has a newline immediately following it in the comment text.  Newlines between paragraphs do not count.
proc TextComment_getText*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_TextComment_getText".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  text contained in the AST node.
proc InlineCommandComment_getCommandName*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_InlineCommandComment_getCommandName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  name of the inline command.
proc InlineCommandComment_getRenderKind*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_InlineCommandComment_getRenderKind".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  the most appropriate rendering mode, chosen on command semantics in Doxygen.
proc InlineCommandComment_getNumArgs*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_InlineCommandComment_getNumArgs".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  number of command arguments.
proc InlineCommandComment_getArgText*(comment: CXComment; argIdx: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_InlineCommandComment_getArgText".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  **
  ## 
  ##  argument index (zero-based).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  text of the specified argument.
proc HTMLTagComment_getTagName*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_HTMLTagComment_getTagName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock or Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  HTML tag name.
proc HTMLStartTagComment_isSelfClosing*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_HTMLStartTagComment_isSelfClosing".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if tag is self-closing (for example, <br />).
proc HTMLStartTag_getNumAttrs*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_HTMLStartTag_getNumAttrs".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  number of attributes (name-value pairs) attached to the start tag.
proc HTMLStartTag_getAttrName*(comment: CXComment; attrIdx: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_HTMLStartTag_getAttrName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  **
  ## 
  ##  attribute index (zero-based).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  name of the specified attribute.
proc HTMLStartTag_getAttrValue*(comment: CXComment; attrIdx: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_HTMLStartTag_getAttrValue".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  **
  ## 
  ##  attribute index (zero-based).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  value of the specified attribute.
proc BlockCommandComment_getCommandName*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_BlockCommandComment_getCommandName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  name of the block command.
proc BlockCommandComment_getNumArgs*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_BlockCommandComment_getNumArgs".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  number of word-like arguments.
proc BlockCommandComment_getArgText*(comment: CXComment; argIdx: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_BlockCommandComment_getArgText".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  **
  ## 
  ##  argument index (zero-based).
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  text of the specified word-like argument.
proc BlockCommandComment_getParagraph*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_BlockCommandComment_getParagraph".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock or Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  paragraph argument of the block command.
proc ParamCommandComment_getParamName*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_getParamName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  parameter name.
proc ParamCommandComment_isParamIndexValid*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_isParamIndexValid".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the parameter that this AST node represents was found in the function prototype and Error: cannot render: rnLiteralBlock function will return a meaningful value.
proc ParamCommandComment_getParamIndex*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_getParamIndex".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  zero-based parameter index in function prototype.
proc ParamCommandComment_isDirectionExplicit*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_isDirectionExplicit".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if parameter passing direction was specified explicitly in the comment.
proc ParamCommandComment_getDirection*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_ParamCommandComment_getDirection".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  parameter passing direction.
proc TParamCommandComment_getParamName*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_getParamName".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  template parameter name.
proc TParamCommandComment_isParamPositionValid*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_isParamPositionValid".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  non-zero if the parameter that this AST node represents was found in the template parameter list and Error: cannot render: rnLiteralBlock and Error: cannot render: rnLiteralBlock functions will return a meaningful value.
proc TParamCommandComment_getDepth*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_TParamCommandComment_getDepth".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  zero-based nesting depth of this parameter in the template parameter list.
  ## 
  ##  For example, Error: cannot render: rnCodeBlock
  ## 
  ##  for C and TT nesting depth is 0, for T nesting depth is 1.
proc TParamCommandComment_getIndex*(comment: CXComment; depth: cuint) {.cdecl,
    dynlib: libclang, importc: "clang_TParamCommandComment_getIndex".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  zero-based parameter index in the template parameter list at a given nesting depth.
  ## 
  ##  For example, Error: cannot render: rnCodeBlock
  ## 
  ##  for C and TT nesting depth is 0, so we can ask for index at depth 0: at depth 0 C's index is 0, TT's index is 1.
  ## 
  ##  For T nesting depth is 1, so we can ask for index at depth 0 and 1: at depth 0 T's index is 1 (same as TT's), at depth 1 T's index is 0.
proc VerbatimBlockLineComment_getText*(comment: CXComment) {.cdecl,
    dynlib: libclang, importc: "clang_VerbatimBlockLineComment_getText".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  text contained in the AST node.
proc VerbatimLineComment_getText*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_VerbatimLineComment_getText".}
  ## 
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  text contained in the AST node.
proc HTMLTagComment_getAsString*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_HTMLTagComment_getAsString".}
  ## 
  ## 
  ##  Convert an HTML tag AST node to string.
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock or Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  string containing an HTML tag.
proc FullComment_getAsHTML*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_FullComment_getAsHTML".}
  ## 
  ## 
  ##  Convert a given full parsed comment to an HTML fragment.
  ## 
  ##  Specific details of HTML layout are subject to change.  Don't try to parse this HTML back into an AST, use other APIs instead.
  ## 
  ##  Currently the following CSS classes are used: 
  ## 
  ## **
  ## 
  ##  "para-brief" for ``
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  "para-returns" for \\returns paragraph and equivalent commands; 
  ## 
  ## **
  ## 
  ##  "word-returns" for the "Returns" word in \\returns paragraph.
  ## 
  ##  Function argument documentation is rendered as a <dl> list with arguments sorted in function prototype order.  CSS classes used: 
  ## 
  ## **
  ## 
  ##  "param-name-index-NUMBER" for parameter name (<dt>); 
  ## 
  ## **
  ## 
  ##  "param-descr-index-NUMBER" for parameter description (<dd>); 
  ## 
  ## **
  ## 
  ##  "param-name-index-invalid" and "param-descr-index-invalid" are used if parameter index is invalid.
  ## 
  ##  Template parameter documentation is rendered as a <dl> list with parameters sorted in template parameter list order.  CSS classes used: 
  ## 
  ## **
  ## 
  ##  "tparam-name-index-NUMBER" for parameter name (<dt>); 
  ## 
  ## **
  ## 
  ##  "tparam-descr-index-NUMBER" for parameter description (<dd>); 
  ## 
  ## **
  ## 
  ##  "tparam-name-index-other" and "tparam-descr-index-other" are used for names inside template template parameters; 
  ## 
  ## **
  ## 
  ##  "tparam-name-index-invalid" and "tparam-descr-index-invalid" are used if parameter position is invalid.
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  string containing an HTML fragment.
proc FullComment_getAsXML*(comment: CXComment) {.cdecl, dynlib: libclang,
    importc: "clang_FullComment_getAsXML".}
  ## 
  ## 
  ##  Convert a given full parsed comment to an XML document.
  ## 
  ##  A Relax NG schema for the XML can be found in comment-xml-schema.rng file inside clang source tree.
  ## 
  ##  **
  ## 
  ##  a Error: cannot render: rnLiteralBlock AST node.
  ## 
  ##  
  ## 
  ## **
  ## 
  ##  string containing an XML document.