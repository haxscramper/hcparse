
type
  CXAvailabilityKind = enum
    akAvailable, akDeprecated, akNotAvailable, akNotAccessible
type
  CXCursor_ExceptionSpecificationKind = enum
    ceskNone, ceskDynamicNone, ceskDynamic, ceskMSAny, ceskBasicNoexcept,
    ceskComputedNoexcept, ceskUnevaluated, ceskUninstantiated, ceskUnparsed,
    ceskNoThrow
proc *(excludeDeclarationsFromPCH: int; displayDiagnostics: int): CXIndex =
  ##  Provides a shared context for creating translation units. It provides two options: - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local" declarations (when loading any new translation units). A "local" declaration is one that belongs in the translation unit itself and not in a precompiled header that was used by the translation unit. If zero, all declarations will be enumerated. Here is an example: Error: cannot render: rnCodeBlock This process of creating the 'pch', loading it separately, and using it (via -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks (which gives the indexer the same performance benefit as the compiler).
  impl

proc disposeIndex*(index: CXIndex): void =
  ##  Destroy the given index. The index must not be destroyed until all of the translation units created within that index have been destroyed.
  impl

type
  CXGlobalOptFlags = enum
    gofNone = 0, gofThreadBackgroundPriorityForIndexing = 1,
    gofThreadBackgroundPriorityForEditing = 2, gofThreadBackgroundPriorityForAll
type
  CXGlobalOptFlags = enum
    gofNone = 0, gofThreadBackgroundPriorityForIndexing = 1,
    gofThreadBackgroundPriorityForEditing = 2, gofThreadBackgroundPriorityForAll
proc setGlobalOptions*(argCXIndex: CXIndex; options: cuint): void =
  ##  Sets general options associated with a CXIndex. For example: Error: cannot render: rnCodeBlock ** A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
  impl

proc getGlobalOptions*(argCXIndex: CXIndex): cuint =
  ##  Gets the general options associated with a CXIndex. ** A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that are associated with the given CXIndex object.
  impl

proc setInvocationEmissionPathOption*(argCXIndex: CXIndex; Path: cstring): void =
  ##  Sets the invocation emission path option in a CXIndex. The invocation emission path specifies a path which will contain log files for certain libclang invocations. A null value (default) implies that libclang invocations are not logged..
  impl

proc getFileName*(SFile: CXFile): CXString =
  ##  Retrieve the complete file and path name of the given file.
  impl

proc getFileTime*(SFile: CXFile): time_t =
  ##  Retrieve the last modification time of the given file.
  impl

proc getFileUniqueID*(file: CXFile; outID: ptr[CXFileUniqueID]): int =
  ##  Retrieve the unique ID for the given Error: cannot render: rnLiteralBlock ** the file to get the ID for. ** stores the returned CXFileUniqueID. ** If there was a failure getting the unique ID, returns non-zero, otherwise returns 0.
  impl

proc isFileMultipleIncludeGuarded*(tu: CXTranslationUnit; file: CXFile): cuint =
  ##  Determine whether the given header is guarded against multiple inclusions, either with the conventional #ifndef/#define/#endif macro guards or with #pragma once.
  impl

proc getFile*(tu: CXTranslationUnit; file_name: cstring): CXFile =
  ##  Retrieve a file handle within the given translation unit. ** the translation unit ** the name of the file. ** the file handle for the named file in the translation unit Error: cannot render: rnLiteralBlock or a NULL file handle if the file was not a part of this translation unit.
  impl

proc getFileContents*(tu: CXTranslationUnit; file: CXFile; size: ptr[int]): cstring =
  impl

proc isEqual*(file1: CXFile; file2: CXFile): int =
  ##  Returns non-zero if the Error: cannot render: rnLiteralBlock and Error: cannot render: rnLiteralBlock point to the same file, or they are both NULL.
  impl

proc tryGetRealPathName*(file: CXFile): CXString =
  ##  Returns the real path name of Error: cannot render: rnLiteralBlock An empty string may be returned. Use Error: cannot render: rnLiteralBlock in that case.
  impl

proc getNullLocation*(): CXSourceLocation =
  ##  Retrieve a NULL (invalid) source location.
  impl

proc equalLocations*(loc1: CXSourceLocation; loc2: CXSourceLocation): cuint =
  ##  Determine whether two source locations, which must refer into the same translation unit, refer to exactly the same point in the source code. ** non-zero if the source locations refer to the same location, zero if they refer to different locations.
  impl

proc getLocation*(tu: CXTranslationUnit; file: CXFile; line: cuint; column: cuint): CXSourceLocation =
  ##  Retrieves the source location associated with a given file/line/column in a particular translation unit.
  impl

proc getLocationForOffset*(tu: CXTranslationUnit; file: CXFile; offset: cuint): CXSourceLocation =
  ##  Retrieves the source location associated with a given character offset in a particular translation unit.
  impl

proc Location_isInSystemHeader*(location: CXSourceLocation): int =
  ##  Returns non-zero if the given source location is in a system header.
  impl

proc Location_isFromMainFile*(location: CXSourceLocation): int =
  ##  Returns non-zero if the given source location is in the main file of the corresponding translation unit.
  impl

proc getNullRange*(): CXSourceRange =
  ##  Retrieve a NULL (invalid) source range.
  impl

proc getRange*(begin: CXSourceLocation; end: CXSourceLocation): CXSourceRange =
  ##  Retrieve a source range given the beginning and ending source locations.
  impl

proc equalRanges*(range1: CXSourceRange; range2: CXSourceRange): cuint =
  ##  Determine whether two ranges are equivalent. ** non-zero if the ranges are the same, zero if they differ.
  impl

proc Range_isNull*(range: CXSourceRange): int =
  ##  Returns non-zero if Error: cannot render: rnLiteralBlock is null.
  impl

proc getExpansionLocation*(location: CXSourceLocation; file: ptr[CXFile];
                          line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]): void =
  ##  Retrieve the file, line, column, and offset represented by the given source location. If the location refers into a macro expansion, retrieves the location of the macro expansion. ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the file to which the given source location points. ** [out] if non-NULL, will be set to the line to which the given source location points. ** [out] if non-NULL, will be set to the column to which the given source location points. ** [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
  impl

proc getPresumedLocation*(location: CXSourceLocation; filename: ptr[CXString];
                         line: ptr[cuint]; column: ptr[cuint]): void =
  ##  Retrieve the file, line and column represented by the given source location, as specified in a # line directive. Example: given the following source code in a file somefile.c Error: cannot render: rnCodeBlock the location information returned by this function would be File: dummy.c Line: 124 Column: 12 whereas clang_getExpansionLocation would have returned File: somefile.c Line: 3 Column: 12 ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the filename of the source location. Note that filenames returned will be for "virtual" files, which don't necessarily exist on the machine running clang - e.g. when parsing preprocessed output obtained from a different environment. If a non-NULL value is passed in, remember to dispose of the returned value using Error: cannot render: rnLiteralBlock once you've finished with it. For an invalid source location, an empty string is returned. ** [out] if non-NULL, will be set to the line number of the source location. For an invalid source location, zero is returned. ** [out] if non-NULL, will be set to the column number of the source location. For an invalid source location, zero is returned.
  impl

proc getInstantiationLocation*(location: CXSourceLocation; file: ptr[CXFile];
                              line: ptr[cuint]; column: ptr[cuint];
                              offset: ptr[cuint]): void =
  ##  Legacy API to retrieve the file, line, column, and offset represented by the given source location. This interface has been replaced by the newer interface #clang_getExpansionLocation(). See that interface's documentation for details.
  impl

proc getSpellingLocation*(location: CXSourceLocation; file: ptr[CXFile];
                         line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]): void =
  ##  Retrieve the file, line, column, and offset represented by the given source location. If the location refers into a macro instantiation, return where the location was originally spelled in the source file. ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the file to which the given source location points. ** [out] if non-NULL, will be set to the line to which the given source location points. ** [out] if non-NULL, will be set to the column to which the given source location points. ** [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
  impl

proc getFileLocation*(location: CXSourceLocation; file: ptr[CXFile];
                     line: ptr[cuint]; column: ptr[cuint]; offset: ptr[cuint]): void =
  ##  Retrieve the file, line, column, and offset represented by the given source location. If the location refers into a macro expansion, return where the macro was expanded or where the macro argument was written, if the location points at a macro argument. ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the file to which the given source location points. ** [out] if non-NULL, will be set to the line to which the given source location points. ** [out] if non-NULL, will be set to the column to which the given source location points. ** [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
  impl

proc getRangeStart*(range: CXSourceRange): CXSourceLocation =
  ##  Retrieve a source location representing the first character within a source range.
  impl

proc getRangeEnd*(range: CXSourceRange): CXSourceLocation =
  ##  Retrieve a source location representing the last character within a source range.
  impl

proc getSkippedRanges*(tu: CXTranslationUnit; file: CXFile): ptr[CXSourceRangeList] =
  ##  Retrieve all ranges that were skipped by the preprocessor. The preprocessor will skip lines when they are surrounded by an if/ifdef/ifndef directive whose condition does not evaluate to true.
  impl

proc getAllSkippedRanges*(tu: CXTranslationUnit): ptr[CXSourceRangeList] =
  ##  Retrieve all ranges from all files that were skipped by the preprocessor. The preprocessor will skip lines when they are surrounded by an if/ifdef/ifndef directive whose condition does not evaluate to true.
  impl

proc disposeSourceRangeList*(ranges: ptr[CXSourceRangeList]): void =
  ##  Destroy the given Error: cannot render: rnLiteralBlock 
  impl

type
  CXDiagnosticSeverity = enum
    dsIgnored = 0, dsNote = 1, dsWarning = 2, dsError = 3, dsFatal = 4
proc getNumDiagnosticsInSet*(Diags: CXDiagnosticSet): cuint =
  ##  Determine the number of diagnostics in a CXDiagnosticSet.
  impl

proc getDiagnosticInSet*(Diags: CXDiagnosticSet; Index: cuint): CXDiagnostic =
  ##  Retrieve a diagnostic associated with the given CXDiagnosticSet. ** the CXDiagnosticSet to query. ** the zero-based diagnostic number to retrieve. ** the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
  impl

type
  CXLoadDiag_Error = enum
    ldeNone = 0, ldeUnknown = 1, ldeCannotLoad = 2, ldeInvalidFile = 3
proc *(file: cstring; error: ptr[CXLoadDiag_Error]; errorString: ptr[CXString]): CXDiagnosticSet =
  ##  Deserialize a set of diagnostics from a Clang diagnostics bitcode file. ** The name of the file to deserialize. ** A pointer to a enum value recording if there was a problem        deserializing the diagnostics. ** A pointer to a CXString for recording the error string        if the file was not successfully loaded. ** A loaded CXDiagnosticSet if successful, and NULL otherwise.  These diagnostics should be released using clang_disposeDiagnosticSet().
  impl

proc disposeDiagnosticSet*(Diags: CXDiagnosticSet): void =
  ##  Release a CXDiagnosticSet and all of its contained diagnostics.
  impl

proc getChildDiagnostics*(D: CXDiagnostic): CXDiagnosticSet =
  ##  Retrieve the child diagnostics of a CXDiagnostic. This CXDiagnosticSet does not need to be released by clang_disposeDiagnosticSet.
  impl

proc getNumDiagnostics*(Unit: CXTranslationUnit): cuint =
  ##  Determine the number of diagnostics produced for the given translation unit.
  impl

proc getDiagnostic*(Unit: CXTranslationUnit; Index: cuint): CXDiagnostic =
  ##  Retrieve a diagnostic associated with the given translation unit. ** the translation unit to query. ** the zero-based diagnostic number to retrieve. ** the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
  impl

proc getDiagnosticSetFromTU*(Unit: CXTranslationUnit): CXDiagnosticSet =
  ##  Retrieve the complete set of diagnostics associated with a        translation unit. ** the translation unit to query.
  impl

proc disposeDiagnostic*(Diagnostic: CXDiagnostic): void =
  ##  Destroy a diagnostic.
  impl

type
  CXDiagnosticDisplayOptions = enum
    ddoDisplaySourceLocation = 1, ddoDisplayColumn = 2, ddoDisplaySourceRanges = 4,
    ddoDisplayOption = 8, ddoDisplayCategoryId = 16, ddoDisplayCategoryName = 32
proc formatDiagnostic*(Diagnostic: CXDiagnostic; Options: cuint): CXString =
  ##  Format the given diagnostic in a manner that is suitable for display. This routine will format the given diagnostic to a string, rendering the diagnostic according to the various options given. The Error: cannot render: rnLiteralBlock function returns the set of options that most closely mimics the behavior of the clang compiler. ** The diagnostic to print. ** A set of options that control the diagnostic display, created by combining Error: cannot render: rnLiteralBlock values. ** A new string containing for formatted diagnostic.
  impl

proc defaultDiagnosticDisplayOptions*(): cuint =
  ##  Retrieve the set of display options most similar to the default behavior of the clang compiler. ** A set of display options suitable for use with Error: cannot render: rnLiteralBlock 
  impl

proc getDiagnosticSeverity*(argCXDiagnostic: CXDiagnostic): CXDiagnosticSeverity =
  ##  Determine the severity of the given diagnostic.
  impl

proc getDiagnosticLocation*(argCXDiagnostic: CXDiagnostic): CXSourceLocation =
  ##  Retrieve the source location of the given diagnostic. This location is where Clang would print the caret ('^') when displaying the diagnostic on the command line.
  impl

proc getDiagnosticSpelling*(argCXDiagnostic: CXDiagnostic): CXString =
  ##  Retrieve the text of the given diagnostic.
  impl

proc getDiagnosticOption*(Diag: CXDiagnostic; Disable: ptr[CXString]): CXString =
  ##  Retrieve the name of the command-line option that enabled this diagnostic. ** The diagnostic to be queried. ** If non-NULL, will be set to the option that disables this diagnostic (if any). ** A string that contains the command-line option used to enable this warning, such as "-Wconversion" or "-pedantic".
  impl

proc getDiagnosticCategory*(argCXDiagnostic: CXDiagnostic): cuint =
  ##  Retrieve the category number for this diagnostic. Diagnostics can be categorized into groups along with other, related diagnostics (e.g., diagnostics under the same warning flag). This routine retrieves the category number for the given diagnostic. ** The number of the category that contains this diagnostic, or zero if this diagnostic is uncategorized.
  impl

proc *(Category: cuint): CXString =
  ##  Retrieve the name of a particular diagnostic category.  This  is now deprecated.  Use clang_getDiagnosticCategoryText()  instead. ** A diagnostic category number, as returned by Error: cannot render: rnLiteralBlock ** The name of the given diagnostic category.
  impl

proc getDiagnosticCategoryText*(argCXDiagnostic: CXDiagnostic): CXString =
  ##  Retrieve the diagnostic category text for a given diagnostic. ** The text of the given diagnostic category.
  impl

proc getDiagnosticNumRanges*(argCXDiagnostic: CXDiagnostic): cuint =
  ##  Determine the number of source ranges associated with the given diagnostic.
  impl

proc getDiagnosticRange*(Diagnostic: CXDiagnostic; Range: cuint): CXSourceRange =
  ##  Retrieve a source range associated with the diagnostic. A diagnostic's source ranges highlight important elements in the source code. On the command line, Clang displays source ranges by underlining them with '~' characters. ** the diagnostic whose range is being extracted. ** the zero-based index specifying which range to ** the requested source range.
  impl

proc getDiagnosticNumFixIts*(Diagnostic: CXDiagnostic): cuint =
  ##  Determine the number of fix-it hints associated with the given diagnostic.
  impl

proc getDiagnosticFixIt*(Diagnostic: CXDiagnostic; FixIt: cuint;
                        ReplacementRange: ptr[CXSourceRange]): CXString =
  ##  Retrieve the replacement information for a given fix-it. Fix-its are described in terms of a source range whose contents should be replaced by a string. This approach generalizes over three kinds of operations: removal of source code (the range covers the code to be removed and the replacement string is empty), replacement of source code (the range covers the code to be replaced and the replacement string provides the new code), and insertion (both the start and end of the range point at the insertion location, and the replacement string provides the text to insert). ** The diagnostic whose fix-its are being queried. ** The zero-based index of the fix-it. ** The source range whose contents will be replaced with the returned replacement string. Note that source ranges are half-open ranges [a, b), so the source code should be replaced from a and up to (but not including) b. ** A string containing text that should be replace the source code indicated by the Error: cannot render: rnLiteralBlock 
  impl

proc getTranslationUnitSpelling*(CTUnit: CXTranslationUnit): CXString =
  ##  Get the original translation unit source file name.
  impl

proc createTranslationUnitFromSourceFile*(CIdx: CXIndex; source_filename: cstring;
    num_clang_command_line_args: int; clang_command_line_args: ptr[cstring];
    num_unsaved_files: cuint; unsaved_files: ptr[CXUnsavedFile]): CXTranslationUnit =
  ##  Return the CXTranslationUnit for a given source file and the provided command line arguments one would pass to the compiler. Note: The 'source_filename' argument is optional.  If the caller provides a NULL pointer, the name of the source file is expected to reside in the specified command line arguments. Note: When encountered in 'clang_command_line_args', the following options are ignored:   '-c'   '-emit-ast'   '-fsyntax-only'   '-o <output file>'  (both '-o' and '<output file>' are ignored) ** The index object with which the translation unit will be associated. ** The name of the source file to load, or NULL if the source file is included in Error: cannot render: rnLiteralBlock ** The number of command-line arguments in Error: cannot render: rnLiteralBlock ** The command-line arguments that would be passed to the Error: cannot render: rnLiteralBlock executable if it were being invoked out-of-process. These command-line options will be parsed and will affect how the translation unit is parsed. Note that the following options are ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default), and '-o <output file>'. ** the number of unsaved file entries in Error: cannot render: rnLiteralBlock ** the files that have not yet been saved to disk but may be required for code completion, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns.
  impl

proc createTranslationUnit*(CIdx: CXIndex; ast_filename: cstring): CXTranslationUnit =
  ##  Same as Error: cannot render: rnLiteralBlock but returns the Error: cannot render: rnLiteralBlock instead of an error code.  In case of an error this routine returns a Error: cannot render: rnLiteralBlock Error: cannot render: rnLiteralBlock without further detailed error codes.
  impl

proc createTranslationUnit2*(CIdx: CXIndex; ast_filename: cstring;
                            out_TU: ptr[CXTranslationUnit]): CXErrorCode =
  ##  Create a translation unit from an AST file (Error: cannot render: rnLiteralBlock ** A non-NULL pointer to store the created Error: cannot render: rnLiteralBlock ** Zero on success, otherwise returns an error code.
  impl

type
  CXTranslationUnit_Flags = enum
    tufNone = 0, tufDetailedPreprocessingRecord = 1, tufIncomplete = 2,
    tufPrecompiledPreamble = 4, tufCacheCompletionResults = 8,
    tufForSerialization = 16, tufCXXChainedPCH = 32, tufSkipFunctionBodies = 64,
    tufIncludeBriefCommentsInCodeCompletion = 128,
    tufCreatePreambleOnFirstParse = 256, tufKeepGoing = 512,
    tufSingleFileParse = 1024, tufLimitSkipFunctionBodiesToPreamble = 2048,
    tufIncludeAttributedTypes = 4096, tufVisitImplicitAttributes = 8192,
    tufIgnoreNonErrorsFromIncludedFiles = 16384,
    tufRetainExcludedConditionalBlocks = 32768
proc defaultEditingTranslationUnitOptions*(): cuint =
  ##  Returns the set of flags that is suitable for parsing a translation unit that is being edited. The set of flags returned provide options for Error: cannot render: rnLiteralBlock to indicate that the translation unit is likely to be reparsed many times, either explicitly (via Error: cannot render: rnLiteralBlock or implicitly (e.g., by code completion (Error: cannot render: rnLiteralBlock The returned flag set contains an unspecified set of optimizations (e.g., the precompiled preamble) geared toward improving the performance of these routines. The set of optimizations enabled may change from one version to the next.
  impl

proc parseTranslationUnit*(CIdx: CXIndex; source_filename: cstring;
                          command_line_args: ptr[cstring];
                          num_command_line_args: int;
                          unsaved_files: ptr[CXUnsavedFile];
                          num_unsaved_files: cuint; options: cuint): CXTranslationUnit =
  ##  Same as Error: cannot render: rnLiteralBlock but returns the Error: cannot render: rnLiteralBlock instead of an error code.  In case of an error this routine returns a Error: cannot render: rnLiteralBlock Error: cannot render: rnLiteralBlock without further detailed error codes.
  impl

proc parseTranslationUnit2*(CIdx: CXIndex; source_filename: cstring;
                           command_line_args: ptr[cstring];
                           num_command_line_args: int;
                           unsaved_files: ptr[CXUnsavedFile];
                           num_unsaved_files: cuint; options: cuint;
                           out_TU: ptr[CXTranslationUnit]): CXErrorCode =
  ##  Parse the given source file and the translation unit corresponding to that file. This routine is the main entry point for the Clang C API, providing the ability to parse a source file into a translation unit that can then be queried by other functions in the API. This routine accepts a set of command-line arguments so that the compilation can be configured in the same way that the compiler is configured on the command line. ** The index object with which the translation unit will be associated. ** The name of the source file to load, or NULL if the source file is included in Error: cannot render: rnLiteralBlock ** The command-line arguments that would be passed to the Error: cannot render: rnLiteralBlock executable if it were being invoked out-of-process. These command-line options will be parsed and will affect how the translation unit is parsed. Note that the following options are ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default), and '-o <output file>'. ** The number of command-line arguments in Error: cannot render: rnLiteralBlock ** the files that have not yet been saved to disk but may be required for parsing, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns. ** the number of unsaved file entries in Error: cannot render: rnLiteralBlock ** A bitmask of options that affects how the translation unit is managed but not its compilation. This should be a bitwise OR of the CXTranslationUnit_XXX flags. ** A non-NULL pointer to store the created Error: cannot render: rnLiteralBlock describing the parsed code and containing any diagnostics produced by the compiler. ** Zero on success, otherwise returns an error code.
  impl

proc parseTranslationUnit2FullArgv*(CIdx: CXIndex; source_filename: cstring;
                                   command_line_args: ptr[cstring];
                                   num_command_line_args: int;
                                   unsaved_files: ptr[CXUnsavedFile];
                                   num_unsaved_files: cuint; options: cuint;
                                   out_TU: ptr[CXTranslationUnit]): CXErrorCode =
  ##  Same as clang_parseTranslationUnit2 but requires a full command line for Error: cannot render: rnLiteralBlock including argv[0]. This is useful if the standard library paths are relative to the binary.
  impl

type
  CXSaveTranslationUnit_Flags = enum
    stufNone = 0
proc defaultSaveOptions*(TU: CXTranslationUnit): cuint =
  ##  Returns the set of flags that is suitable for saving a translation unit. The set of flags returned provide options for Error: cannot render: rnLiteralBlock by default. The returned flag set contains an unspecified set of options that save translation units with the most commonly-requested data.
  impl

type
  CXSaveError = enum
    seNone = 0, seUnknown = 1, seTranslationErrors = 2, seInvalidTU = 3
proc saveTranslationUnit*(TU: CXTranslationUnit; FileName: cstring; options: cuint): int =
  ##  Saves a translation unit into a serialized representation of that translation unit on disk. Any translation unit that was parsed without error can be saved into a file. The translation unit can then be deserialized into a new Error: cannot render: rnLiteralBlock with Error: cannot render: rnLiteralBlock or, if it is an incomplete translation unit that corresponds to a header, used as a precompiled header when parsing other translation units. ** The translation unit to save. ** The file to which the translation unit will be saved. ** A bitmask of options that affects how the translation unit is saved. This should be a bitwise OR of the CXSaveTranslationUnit_XXX flags. ** A value that will match one of the enumerators of the CXSaveError enumeration. Zero (CXSaveError_None) indicates that the translation unit was saved successfully, while a non-zero value indicates that a problem occurred.
  impl

proc suspendTranslationUnit*(argCXTranslationUnit: CXTranslationUnit): cuint =
  ##  Suspend a translation unit in order to free memory associated with it. A suspended translation unit uses significantly less memory but on the other side does not support any other calls than Error: cannot render: rnLiteralBlock to resume it or Error: cannot render: rnLiteralBlock to dispose it completely.
  impl

proc disposeTranslationUnit*(argCXTranslationUnit: CXTranslationUnit): void =
  ##  Destroy the specified CXTranslationUnit object.
  impl

type
  CXReparse_Flags = enum
    rfNone = 0
proc defaultReparseOptions*(TU: CXTranslationUnit): cuint =
  ##  Returns the set of flags that is suitable for reparsing a translation unit. The set of flags returned provide options for Error: cannot render: rnLiteralBlock by default. The returned flag set contains an unspecified set of optimizations geared toward common uses of reparsing. The set of optimizations enabled may change from one version to the next.
  impl

proc reparseTranslationUnit*(TU: CXTranslationUnit; num_unsaved_files: cuint;
                            unsaved_files: ptr[CXUnsavedFile]; options: cuint): int =
  ##  Reparse the source files that produced this translation unit. This routine can be used to re-parse the source files that originally created the given translation unit, for example because those source files have changed (either on disk or as passed via Error: cannot render: rnLiteralBlock The source code will be reparsed with the same command-line options as it was originally parsed. Reparsing a translation unit invalidates all cursors and source locations that refer into that translation unit. This makes reparsing a translation unit semantically equivalent to destroying the translation unit and then creating a new translation unit with the same command-line arguments. However, it may be more efficient to reparse a translation unit using this routine. ** The translation unit whose contents will be re-parsed. The translation unit must originally have been built with Error: cannot render: rnLiteralBlock ** The number of unsaved file entries in Error: cannot render: rnLiteralBlock ** The files that have not yet been saved to disk but may be required for parsing, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns. ** A bitset of options composed of the flags in CXReparse_Flags. The function Error: cannot render: rnLiteralBlock produces a default set of options recommended for most uses, based on the translation unit. ** 0 if the sources could be reparsed.  A non-zero error code will be returned if reparsing was impossible, such that the translation unit is invalid. In such cases, the only valid call for Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock  The error codes returned by this routine are described by the Error: cannot render: rnLiteralBlock enum.
  impl

type
  CXTUResourceUsageKind = enum
    turukAST = 1, turukIdentifiers = 2, turukSelectors = 3,
    turukGlobalCompletionResults = 4, turukSourceManagerContentCache = 5,
    turukAST_SideTables = 6, turukSourceManager_Membuffer_Malloc = 7,
    turukSourceManager_Membuffer_MMap = 8,
    turukExternalASTSource_Membuffer_Malloc = 9,
    turukExternalASTSource_Membuffer_MMap = 10, turukPreprocessor = 11,
    turukPreprocessingRecord = 12, turukSourceManager_DataStructures = 13,
    turukPreprocessor_HeaderSearch = 14, turukMEMORY_IN_BYTES_BEGIN,
    turukMEMORY_IN_BYTES_END, turukFirst, turukLast
proc getTUResourceUsageName*(kind: CXTUResourceUsageKind): cstring =
  ##  Returns the human-readable null-terminated C string that represents  the name of the memory category.  This string should never be freed.
  impl

proc getCXTUResourceUsage*(TU: CXTranslationUnit): CXTUResourceUsage =
  ##  Return the memory usage of a translation unit.  This object  should be released with clang_disposeCXTUResourceUsage().
  impl

proc disposeCXTUResourceUsage*(usage: CXTUResourceUsage): void =
  impl

proc getTranslationUnitTargetInfo*(CTUnit: CXTranslationUnit): CXTargetInfo =
  ##  Get target information for this translation unit. The CXTargetInfo object cannot outlive the CXTranslationUnit object.
  impl

proc dispose*(Info: CXTargetInfo): void =
  ##  Destroy the CXTargetInfo object.
  impl

proc getTriple*(Info: CXTargetInfo): CXString =
  ##  Get the normalized target triple as a string. Returns the empty string in case of any error.
  impl

proc getPointerWidth*(Info: CXTargetInfo): int =
  ##  Get the pointer width of the target in bits. Returns -1 in case of error.
  impl

type
  CXCursorKind = enum
    ckUnexposedDecl = 1, ckStructDecl = 2, ckUnionDecl = 3, ckClassDecl = 4, ckEnumDecl = 5,
    ckFieldDecl = 6, ckEnumConstantDecl = 7, ckFunctionDecl = 8, ckVarDecl = 9,
    ckParmDecl = 10, ckObjCInterfaceDecl = 11, ckObjCCategoryDecl = 12,
    ckObjCProtocolDecl = 13, ckObjCPropertyDecl = 14, ckObjCIvarDecl = 15,
    ckObjCInstanceMethodDecl = 16, ckObjCClassMethodDecl = 17,
    ckObjCImplementationDecl = 18, ckObjCCategoryImplDecl = 19, ckTypedefDecl = 20,
    ckCXXMethod = 21, ckNamespace = 22, ckLinkageSpec = 23, ckConstructor = 24,
    ckDestructor = 25, ckConversionFunction = 26, ckTemplateTypeParameter = 27,
    ckNonTypeTemplateParameter = 28, ckTemplateTemplateParameter = 29,
    ckFunctionTemplate = 30, ckClassTemplate = 31,
    ckClassTemplatePartialSpecialization = 32, ckNamespaceAlias = 33,
    ckUsingDirective = 34, ckUsingDeclaration = 35, ckTypeAliasDecl = 36,
    ckObjCSynthesizeDecl = 37, ckObjCDynamicDecl = 38, ckCXXAccessSpecifier = 39,
    ckFirstDecl, ckLastDecl, ckFirstRef = 40, ckObjCSuperClassRef = 40,
    ckObjCProtocolRef = 41, ckObjCClassRef = 42, ckTypeRef = 43, ckCXXBaseSpecifier = 44,
    ckTemplateRef = 45, ckNamespaceRef = 46, ckMemberRef = 47, ckLabelRef = 48,
    ckOverloadedDeclRef = 49, ckVariableRef = 50, ckLastRef, ckFirstInvalid = 70,
    ckInvalidFile = 70, ckNoDeclFound = 71, ckNotImplemented = 72, ckInvalidCode = 73,
    ckLastInvalid, ckFirstExpr = 100, ckUnexposedExpr = 100, ckDeclRefExpr = 101,
    ckMemberRefExpr = 102, ckCallExpr = 103, ckObjCMessageExpr = 104, ckBlockExpr = 105,
    ckIntegerLiteral = 106, ckFloatingLiteral = 107, ckImaginaryLiteral = 108,
    ckStringLiteral = 109, ckCharacterLiteral = 110, ckParenExpr = 111,
    ckUnaryOperator = 112, ckArraySubscriptExpr = 113, ckBinaryOperator = 114,
    ckCompoundAssignOperator = 115, ckConditionalOperator = 116,
    ckCStyleCastExpr = 117, ckCompoundLiteralExpr = 118, ckInitListExpr = 119,
    ckAddrLabelExpr = 120, ckStmtExpr = 121, ckGenericSelectionExpr = 122,
    ckGNUNullExpr = 123, ckCXXStaticCastExpr = 124, ckCXXDynamicCastExpr = 125,
    ckCXXReinterpretCastExpr = 126, ckCXXConstCastExpr = 127,
    ckCXXFunctionalCastExpr = 128, ckCXXTypeidExpr = 129, ckCXXBoolLiteralExpr = 130,
    ckCXXNullPtrLiteralExpr = 131, ckCXXThisExpr = 132, ckCXXThrowExpr = 133,
    ckCXXNewExpr = 134, ckCXXDeleteExpr = 135, ckUnaryExpr = 136,
    ckObjCStringLiteral = 137, ckObjCEncodeExpr = 138, ckObjCSelectorExpr = 139,
    ckObjCProtocolExpr = 140, ckObjCBridgedCastExpr = 141, ckPackExpansionExpr = 142,
    ckSizeOfPackExpr = 143, ckLambdaExpr = 144, ckObjCBoolLiteralExpr = 145,
    ckObjCSelfExpr = 146, ckOMPArraySectionExpr = 147,
    ckObjCAvailabilityCheckExpr = 148, ckFixedPointLiteral = 149, ckLastExpr,
    ckFirstStmt = 200, ckUnexposedStmt = 200, ckLabelStmt = 201, ckCompoundStmt = 202,
    ckCaseStmt = 203, ckDefaultStmt = 204, ckIfStmt = 205, ckSwitchStmt = 206,
    ckWhileStmt = 207, ckDoStmt = 208, ckForStmt = 209, ckGotoStmt = 210,
    ckIndirectGotoStmt = 211, ckContinueStmt = 212, ckBreakStmt = 213,
    ckReturnStmt = 214, ckGCCAsmStmt = 215, ckAsmStmt, ckObjCAtTryStmt = 216,
    ckObjCAtCatchStmt = 217, ckObjCAtFinallyStmt = 218, ckObjCAtThrowStmt = 219,
    ckObjCAtSynchronizedStmt = 220, ckObjCAutoreleasePoolStmt = 221,
    ckObjCForCollectionStmt = 222, ckCXXCatchStmt = 223, ckCXXTryStmt = 224,
    ckCXXForRangeStmt = 225, ckSEHTryStmt = 226, ckSEHExceptStmt = 227,
    ckSEHFinallyStmt = 228, ckMSAsmStmt = 229, ckNullStmt = 230, ckDeclStmt = 231,
    ckOMPParallelDirective = 232, ckOMPSimdDirective = 233, ckOMPForDirective = 234,
    ckOMPSectionsDirective = 235, ckOMPSectionDirective = 236,
    ckOMPSingleDirective = 237, ckOMPParallelForDirective = 238,
    ckOMPParallelSectionsDirective = 239, ckOMPTaskDirective = 240,
    ckOMPMasterDirective = 241, ckOMPCriticalDirective = 242,
    ckOMPTaskyieldDirective = 243, ckOMPBarrierDirective = 244,
    ckOMPTaskwaitDirective = 245, ckOMPFlushDirective = 246, ckSEHLeaveStmt = 247,
    ckOMPOrderedDirective = 248, ckOMPAtomicDirective = 249,
    ckOMPForSimdDirective = 250, ckOMPParallelForSimdDirective = 251,
    ckOMPTargetDirective = 252, ckOMPTeamsDirective = 253,
    ckOMPTaskgroupDirective = 254, ckOMPCancellationPointDirective = 255,
    ckOMPCancelDirective = 256, ckOMPTargetDataDirective = 257,
    ckOMPTaskLoopDirective = 258, ckOMPTaskLoopSimdDirective = 259,
    ckOMPDistributeDirective = 260, ckOMPTargetEnterDataDirective = 261,
    ckOMPTargetExitDataDirective = 262, ckOMPTargetParallelDirective = 263,
    ckOMPTargetParallelForDirective = 264, ckOMPTargetUpdateDirective = 265,
    ckOMPDistributeParallelForDirective = 266,
    ckOMPDistributeParallelForSimdDirective = 267,
    ckOMPDistributeSimdDirective = 268, ckOMPTargetParallelForSimdDirective = 269,
    ckOMPTargetSimdDirective = 270, ckOMPTeamsDistributeDirective = 271,
    ckOMPTeamsDistributeSimdDirective = 272,
    ckOMPTeamsDistributeParallelForSimdDirective = 273,
    ckOMPTeamsDistributeParallelForDirective = 274,
    ckOMPTargetTeamsDirective = 275, ckOMPTargetTeamsDistributeDirective = 276,
    ckOMPTargetTeamsDistributeParallelForDirective = 277,
    ckOMPTargetTeamsDistributeParallelForSimdDirective = 278,
    ckOMPTargetTeamsDistributeSimdDirective = 279, ckBuiltinBitCastExpr = 280,
    ckOMPMasterTaskLoopDirective = 281, ckOMPParallelMasterTaskLoopDirective = 282,
    ckOMPMasterTaskLoopSimdDirective = 283,
    ckOMPParallelMasterTaskLoopSimdDirective = 284,
    ckOMPParallelMasterDirective = 285, ckLastStmt, ckTranslationUnit = 300,
    ckFirstAttr = 400, ckUnexposedAttr = 400, ckIBActionAttr = 401, ckIBOutletAttr = 402,
    ckIBOutletCollectionAttr = 403, ckCXXFinalAttr = 404, ckCXXOverrideAttr = 405,
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
    ckLastAttr, ckPreprocessingDirective = 500, ckMacroDefinition = 501,
    ckMacroExpansion = 502, ckMacroInstantiation, ckInclusionDirective = 503,
    ckFirstPreprocessing, ckLastPreprocessing, ckModuleImportDecl = 600,
    ckTypeAliasTemplateDecl = 601, ckStaticAssert = 602, ckFriendDecl = 603,
    ckFirstExtraDecl, ckLastExtraDecl, ckOverloadCandidate = 700
proc getNullCursor*(): CXCursor =
  ##  Retrieve the NULL cursor, which represents no entity.
  impl

proc getTranslationUnitCursor*(argCXTranslationUnit: CXTranslationUnit): CXCursor =
  ##  Retrieve the cursor that represents the given translation unit. The translation unit cursor can be used to start traversing the various declarations within the given translation unit.
  impl

proc equalCursors*(argCXCursor: CXCursor; argCXCursor: CXCursor): cuint =
  ##  Determine whether two cursors are equivalent.
  impl

proc isNull*(cursor: CXCursor): int =
  ##  Returns non-zero if Error: cannot render: rnLiteralBlock is null.
  impl

proc hashCursor*(argCXCursor: CXCursor): cuint =
  ##  Compute a hash value for the given cursor.
  impl

proc getCursorKind*(argCXCursor: CXCursor): CXCursorKind =
  ##  Retrieve the kind of the given cursor.
  impl

proc isDeclaration*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents a declaration.
  impl

proc isInvalidDeclaration*(argCXCursor: CXCursor): cuint =
  ##  Determine whether the given declaration is invalid. A declaration is invalid if it could not be parsed successfully. ** non-zero if the cursor represents a declaration and it is invalid, otherwise NULL.
  impl

proc isReference*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents a simple reference. Note that other kinds of cursors (such as expressions) can also refer to other cursors. Use clang_getCursorReferenced() to determine whether a particular cursor refers to another entity.
  impl

proc isExpression*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents an expression.
  impl

proc isStatement*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents a statement.
  impl

proc isAttribute*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents an attribute.
  impl

proc hasAttrs*(C: CXCursor): cuint =
  ##  Determine whether the given cursor has any attributes.
  impl

proc isInvalid*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents an invalid cursor.
  impl

proc isTranslationUnit*(argCXCursorKind: CXCursorKind): cuint =
  ##  Determine whether the given cursor kind represents a translation unit.
  impl

proc isPreprocessing*(argCXCursorKind: CXCursorKind): cuint =
  ## * Determine whether the given cursor represents a preprocessing element, such as a preprocessor directive or macro instantiation.
  impl

proc isUnexposed*(argCXCursorKind: CXCursorKind): cuint =
  ## * Determine whether the given cursor represents a currently  unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
  impl

type
  CXLinkageKind = enum
    lkInvalid, lkNoLinkage, lkInternal, lkUniqueExternal, lkExternal
proc getCursorLinkage*(cursor: CXCursor): CXLinkageKind =
  ##  Determine the linkage of the entity referred to by a given cursor.
  impl

type
  CXVisibilityKind = enum
    vkInvalid, vkHidden, vkProtected, vkDefault
proc getCursorVisibility*(cursor: CXCursor): CXVisibilityKind =
  ##  Describe the visibility of the entity referred to by a cursor. This returns the default visibility if not explicitly specified by a visibility attribute. The default visibility may be changed by commandline arguments. ** The cursor to query. ** The visibility of the cursor.
  impl

proc getCursorAvailability*(cursor: CXCursor): CXAvailabilityKind =
  ##  Determine the availability of the entity that this cursor refers to, taking the current target platform into account. ** The cursor to query. ** The availability of the cursor.
  impl

proc getCursorPlatformAvailability*(cursor: CXCursor; always_deprecated: ptr[int];
                                   deprecated_message: ptr[CXString];
                                   always_unavailable: ptr[int];
                                   unavailable_message: ptr[CXString];
                                   availability: ptr[CXPlatformAvailability];
                                   availability_size: int): int =
  ##  Determine the availability of the entity that this cursor refers to on any platforms for which availability information is known. ** The cursor to query. ** If non-NULL, will be set to indicate whether the entity is deprecated on all platforms. ** If non-NULL, will be set to the message text provided along with the unconditional deprecation of this entity. The client is responsible for deallocating this string. ** If non-NULL, will be set to indicate whether the entity is unavailable on all platforms. ** If non-NULL, will be set to the message text provided along with the unconditional unavailability of this entity. The client is responsible for deallocating this string. ** If non-NULL, an array of CXPlatformAvailability instances that will be populated with platform availability information, up to either the number of platforms for which availability information is available (as returned by this function) or Error: cannot render: rnLiteralBlock whichever is smaller. ** The number of elements available in the Error: cannot render: rnLiteralBlock array. ** The number of platforms (N) for which availability information is available (which is unrelated to Error: cannot render: rnLiteralBlock Note that the client is responsible for calling Error: cannot render: rnLiteralBlock to free each of the platform-availability structures returned. There are Error: cannot render: rnLiteralBlock availability_size) such structures.
  impl

proc disposeCXPlatformAvailability*(availability: ptr[CXPlatformAvailability]): void =
  ##  Free the memory associated with a Error: cannot render: rnLiteralBlock structure.
  impl

type
  CXLanguageKind = enum
    lkInvalid = 0, lkC, lkObjC, lkCPlusPlus
proc getCursorLanguage*(cursor: CXCursor): CXLanguageKind =
  ##  Determine the "language" of the entity referred to by a given cursor.
  impl

type
  CXTLSKind = enum
    tlskNone = 0, tlskDynamic, tlskStatic
proc getCursorTLSKind*(cursor: CXCursor): CXTLSKind =
  ##  Determine the "thread-local storage (TLS) kind" of the declaration referred to by a cursor.
  impl

proc getTranslationUnit*(argCXCursor: CXCursor): CXTranslationUnit =
  ##  Returns the translation unit that a cursor originated from.
  impl

proc createCXCursorSet*(): CXCursorSet =
  ##  Creates an empty CXCursorSet.
  impl

proc disposeCXCursorSet*(cset: CXCursorSet): void =
  ##  Disposes a CXCursorSet and releases its associated memory.
  impl

proc contains*(cset: CXCursorSet; cursor: CXCursor): cuint =
  ##  Queries a CXCursorSet to see if it contains a specific CXCursor. ** non-zero if the set contains the specified cursor.
  impl

proc insert*(cset: CXCursorSet; cursor: CXCursor): cuint =
  ##  Inserts a CXCursor into a CXCursorSet. ** zero if the CXCursor was already in the set, and non-zero otherwise.
  impl

proc getCursorSemanticParent*(cursor: CXCursor): CXCursor =
  ##  Determine the semantic parent of the given cursor. The semantic parent of a cursor is the cursor that semantically contains the given Error: cannot render: rnLiteralBlock For many declarations, the lexical and semantic parents are equivalent (the lexical parent is returned by Error: cannot render: rnLiteralBlock They diverge when declarations or definitions are provided out-of-line. For example: Error: cannot render: rnCodeBlock In the out-of-line definition of Error: cannot render: rnLiteralBlock the semantic parent is the class Error: cannot render: rnLiteralBlock of which this function is a member. The lexical parent is the place where the declaration actually occurs in the source code; in this case, the definition occurs in the translation unit. In general, the lexical parent for a given entity can change without affecting the semantics of the program, and the lexical parent of different declarations of the same entity may be different. Changing the semantic parent of a declaration, on the other hand, can have a major impact on semantics, and redeclarations of a particular entity should all have the same semantic context. In the example above, both declarations of Error: cannot render: rnLiteralBlock have Error: cannot render: rnLiteralBlock as their semantic context, while the lexical context of the first Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock and the lexical context of the second Error: cannot render: rnLiteralBlock is the translation unit. For global declarations, the semantic parent is the translation unit.
  impl

proc getCursorLexicalParent*(cursor: CXCursor): CXCursor =
  ##  Determine the lexical parent of the given cursor. The lexical parent of a cursor is the cursor in which the given Error: cannot render: rnLiteralBlock was actually written. For many declarations, the lexical and semantic parents are equivalent (the semantic parent is returned by Error: cannot render: rnLiteralBlock They diverge when declarations or definitions are provided out-of-line. For example: Error: cannot render: rnCodeBlock In the out-of-line definition of Error: cannot render: rnLiteralBlock the semantic parent is the class Error: cannot render: rnLiteralBlock of which this function is a member. The lexical parent is the place where the declaration actually occurs in the source code; in this case, the definition occurs in the translation unit. In general, the lexical parent for a given entity can change without affecting the semantics of the program, and the lexical parent of different declarations of the same entity may be different. Changing the semantic parent of a declaration, on the other hand, can have a major impact on semantics, and redeclarations of a particular entity should all have the same semantic context. In the example above, both declarations of Error: cannot render: rnLiteralBlock have Error: cannot render: rnLiteralBlock as their semantic context, while the lexical context of the first Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock and the lexical context of the second Error: cannot render: rnLiteralBlock is the translation unit. For declarations written in the global scope, the lexical parent is the translation unit.
  impl

proc getOverriddenCursors*(cursor: CXCursor; overridden: ptr[ptr[CXCursor]];
                          num_overridden: ptr[cuint]): void =
  ##  Determine the set of methods that are overridden by the given method. In both Objective-C and C++, a method (aka virtual member function, in C++) can override a virtual method in a base class. For Objective-C, a method is said to override any method in the class's base class, its protocols, or its categories' protocols, that has the same selector and is of the same kind (class or instance). If no such method exists, the search continues to the class's superclass, its protocols, and its categories, and so on. A method from an Objective-C implementation is considered to override the same methods as its corresponding method in the interface. For C++, a virtual member function overrides any virtual member function with the same signature that occurs in its base classes. With multiple inheritance, a virtual member function can override several virtual member functions coming from different base classes. In all cases, this function determines the immediate overridden method, rather than all of the overridden methods. For example, if a method is originally declared in a class A, then overridden in B (which in inherits from A) and also in C (which inherited from B), then the only overridden method returned from this function when invoked on C's method will be B's method. The client may then invoke this function again, given the previously-found overridden methods, to map out the complete method-override set. ** A cursor representing an Objective-C or C++ method. This routine will compute the set of methods that this method overrides. ** A pointer whose pointee will be replaced with a pointer to an array of cursors, representing the set of overridden methods. If there are no overridden methods, the pointee will be set to NULL. The pointee must be freed via a call to Error: cannot render: rnLiteralBlock ** A pointer to the number of overridden functions, will be set to the number of overridden functions in the array pointed to by Error: cannot render: rnLiteralBlock 
  impl

proc disposeOverriddenCursors*(overridden: ptr[CXCursor]): void =
  ##  Free the set of overridden cursors returned by Error: cannot render: rnLiteralBlock 
  impl

proc getIncludedFile*(cursor: CXCursor): CXFile =
  ##  Retrieve the file that is included by the given inclusion directive cursor.
  impl

proc getCursor*(argCXTranslationUnit: CXTranslationUnit;
               argCXSourceLocation: CXSourceLocation): CXCursor =
  ##  Map a source location to the cursor that describes the entity at that location in the source code. clang_getCursor() maps an arbitrary source location within a translation unit down to the most specific cursor that describes the entity at that location. For example, given an expression Error: cannot render: rnLiteralBlock + y, invoking clang_getCursor() with a source location pointing to "x" will return the cursor for "x"; similarly for "y". If the cursor points anywhere between "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor() will return a cursor referring to the "+" expression. ** a cursor representing the entity at the given source location, or a NULL cursor if no such entity can be found.
  impl

proc getCursorLocation*(argCXCursor: CXCursor): CXSourceLocation =
  ##  Retrieve the physical location of the source constructor referenced by the given cursor. The location of a declaration is typically the location of the name of that declaration, where the name of that declaration would occur if it is unnamed, or some keyword that introduces that particular declaration. The location of a reference is where that reference occurs within the source code.
  impl

proc getCursorExtent*(argCXCursor: CXCursor): CXSourceRange =
  ##  Retrieve the physical extent of the source construct referenced by the given cursor. The extent of a cursor starts with the file/line/column pointing at the first character within the source construct that the cursor refers to and ends with the last character within that source construct. For a declaration, the extent covers the declaration itself. For a reference, the extent covers the location of the reference (e.g., where the referenced entity was actually used).
  impl

type
  CXTypeKind = enum
    tkInvalid = 0, tkUnexposed = 1, tkVoid = 2, tkBool = 3, tkChar_U = 4, tkUChar = 5,
    tkChar16 = 6, tkChar32 = 7, tkUShort = 8, tkUInt = 9, tkULong = 10, tkULongLong = 11,
    tkUInt128 = 12, tkChar_S = 13, tkSChar = 14, tkWChar = 15, tkShort = 16, tkInt = 17,
    tkLong = 18, tkLongLong = 19, tkInt128 = 20, tkFloat = 21, tkDouble = 22,
    tkLongDouble = 23, tkNullPtr = 24, tkOverload = 25, tkDependent = 26, tkObjCId = 27,
    tkObjCClass = 28, tkObjCSel = 29, tkFloat128 = 30, tkHalf = 31, tkFloat16 = 32,
    tkShortAccum = 33, tkAccum = 34, tkLongAccum = 35, tkUShortAccum = 36, tkUAccum = 37,
    tkULongAccum = 38, tkFirstBuiltin, tkLastBuiltin, tkComplex = 100, tkPointer = 101,
    tkBlockPointer = 102, tkLValueReference = 103, tkRValueReference = 104,
    tkRecord = 105, tkEnum = 106, tkTypedef = 107, tkObjCInterface = 108,
    tkObjCObjectPointer = 109, tkFunctionNoProto = 110, tkFunctionProto = 111,
    tkConstantArray = 112, tkVector = 113, tkIncompleteArray = 114,
    tkVariableArray = 115, tkDependentSizedArray = 116, tkMemberPointer = 117,
    tkAuto = 118, tkElaborated = 119, tkPipe = 120, tkOCLImage1dRO = 121,
    tkOCLImage1dArrayRO = 122, tkOCLImage1dBufferRO = 123, tkOCLImage2dRO = 124,
    tkOCLImage2dArrayRO = 125, tkOCLImage2dDepthRO = 126,
    tkOCLImage2dArrayDepthRO = 127, tkOCLImage2dMSAARO = 128,
    tkOCLImage2dArrayMSAARO = 129, tkOCLImage2dMSAADepthRO = 130,
    tkOCLImage2dArrayMSAADepthRO = 131, tkOCLImage3dRO = 132, tkOCLImage1dWO = 133,
    tkOCLImage1dArrayWO = 134, tkOCLImage1dBufferWO = 135, tkOCLImage2dWO = 136,
    tkOCLImage2dArrayWO = 137, tkOCLImage2dDepthWO = 138,
    tkOCLImage2dArrayDepthWO = 139, tkOCLImage2dMSAAWO = 140,
    tkOCLImage2dArrayMSAAWO = 141, tkOCLImage2dMSAADepthWO = 142,
    tkOCLImage2dArrayMSAADepthWO = 143, tkOCLImage3dWO = 144, tkOCLImage1dRW = 145,
    tkOCLImage1dArrayRW = 146, tkOCLImage1dBufferRW = 147, tkOCLImage2dRW = 148,
    tkOCLImage2dArrayRW = 149, tkOCLImage2dDepthRW = 150,
    tkOCLImage2dArrayDepthRW = 151, tkOCLImage2dMSAARW = 152,
    tkOCLImage2dArrayMSAARW = 153, tkOCLImage2dMSAADepthRW = 154,
    tkOCLImage2dArrayMSAADepthRW = 155, tkOCLImage3dRW = 156, tkOCLSampler = 157,
    tkOCLEvent = 158, tkOCLQueue = 159, tkOCLReserveID = 160, tkObjCObject = 161,
    tkObjCTypeParam = 162, tkAttributed = 163, tkOCLIntelSubgroupAVCMcePayload = 164,
    tkOCLIntelSubgroupAVCImePayload = 165, tkOCLIntelSubgroupAVCRefPayload = 166,
    tkOCLIntelSubgroupAVCSicPayload = 167, tkOCLIntelSubgroupAVCMceResult = 168,
    tkOCLIntelSubgroupAVCImeResult = 169, tkOCLIntelSubgroupAVCRefResult = 170,
    tkOCLIntelSubgroupAVCSicResult = 171,
    tkOCLIntelSubgroupAVCImeResultSingleRefStreamout = 172,
    tkOCLIntelSubgroupAVCImeResultDualRefStreamout = 173,
    tkOCLIntelSubgroupAVCImeSingleRefStreamin = 174,
    tkOCLIntelSubgroupAVCImeDualRefStreamin = 175, tkExtVector = 176
type
  CXCallingConv = enum
    ccDefault = 0, ccC = 1, ccX86StdCall = 2, ccX86FastCall = 3, ccX86ThisCall = 4,
    ccX86Pascal = 5, ccAAPCS = 6, ccAAPCS_VFP = 7, ccX86RegCall = 8, ccIntelOclBicc = 9,
    ccWin64 = 10, ccX86_64Win64, ccX86_64SysV = 11, ccX86VectorCall = 12, ccSwift = 13,
    ccPreserveMost = 14, ccPreserveAll = 15, ccAArch64VectorCall = 16, ccInvalid = 100,
    ccUnexposed = 200
proc getCursorType*(C: CXCursor): CXType =
  ##  Retrieve the type of a CXCursor (if any).
  impl

proc getTypeSpelling*(CT: CXType): CXString =
  ##  Pretty-print the underlying type using the rules of the language of the translation unit from which it came. If the type is invalid, an empty string is returned.
  impl

proc getTypedefDeclUnderlyingType*(C: CXCursor): CXType =
  ##  Retrieve the underlying type of a typedef declaration. If the cursor does not reference a typedef declaration, an invalid type is returned.
  impl

proc getEnumDeclIntegerType*(C: CXCursor): CXType =
  ##  Retrieve the integer type of an enum declaration. If the cursor does not reference an enum declaration, an invalid type is returned.
  impl

proc getEnumConstantDeclValue*(C: CXCursor): clonglong =
  ##  Retrieve the integer value of an enum constant declaration as a signed  long long. If the cursor does not reference an enum constant declaration, LLONG_MIN is returned. Since this is also potentially a valid constant value, the kind of the cursor must be verified before calling this function.
  impl

proc getEnumConstantDeclUnsignedValue*(C: CXCursor): culonglong =
  ##  Retrieve the integer value of an enum constant declaration as an unsigned  long long. If the cursor does not reference an enum constant declaration, ULLONG_MAX is returned. Since this is also potentially a valid constant value, the kind of the cursor must be verified before calling this function.
  impl

proc getFieldDeclBitWidth*(C: CXCursor): int =
  ##  Retrieve the bit width of a bit field declaration as an integer. If a cursor that is not a bit field declaration is passed in, -1 is returned.
  impl

proc getNumArguments*(C: CXCursor): int =
  ##  Retrieve the number of non-variadic arguments associated with a given cursor. The number of arguments can be determined for calls as well as for declarations of functions or methods. For other cursors -1 is returned.
  impl

proc getArgument*(C: CXCursor; i: cuint): CXCursor =
  ##  Retrieve the argument cursor of a function or method. The argument cursor can be determined for calls as well as for declarations of functions or methods. For other cursors and for invalid indices, an invalid cursor is returned.
  impl

type
  CXTemplateArgumentKind = enum
    takNull, takType, takDeclaration, takNullPtr, takIntegral, takTemplate,
    takTemplateExpansion, takExpression, takPack, takInvalid
proc getNumTemplateArguments*(C: CXCursor): int =
  ## Returns the number of template args of a function decl representing a template specialization. If the argument cursor cannot be converted into a template function declaration, -1 is returned. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); The value 3 would be returned from this call.
  impl

proc getTemplateArgumentKind*(C: CXCursor; I: cuint): CXTemplateArgumentKind =
  ##  Retrieve the kind of the I'th template argument of the CXCursor C. If the argument CXCursor does not represent a FunctionDecl, an invalid template argument kind is returned. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); For I = 0, 1, and 2, Type, Integral, and Integral will be returned, respectively.
  impl

proc getTemplateArgumentType*(C: CXCursor; I: cuint): CXType =
  ##  Retrieve a CXType representing the type of a TemplateArgument of a  function decl representing a template specialization. If the argument CXCursor does not represent a FunctionDecl whose I'th template argument has a kind of CXTemplateArgKind_Integral, an invalid type is returned. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); If called with I = 0, "float", will be returned. Invalid types will be returned for I == 1 or 2.
  impl

proc getTemplateArgumentValue*(C: CXCursor; I: cuint): clonglong =
  ##  Retrieve the value of an Integral TemplateArgument (of a function  decl representing a template specialization) as a signed long long. It is undefined to call this function on a CXCursor that does not represent a FunctionDecl or whose I'th template argument is not an integral value. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); If called with I = 1 or 2, -7 or true will be returned, respectively. For I == 0, this function's behavior is undefined.
  impl

proc getTemplateArgumentUnsignedValue*(C: CXCursor; I: cuint): culonglong =
  ##  Retrieve the value of an Integral TemplateArgument (of a function  decl representing a template specialization) as an unsigned long long. It is undefined to call this function on a CXCursor that does not represent a FunctionDecl or whose I'th template argument is not an integral value. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, 2147483649, true>(); If called with I = 1 or 2, 2147483649 or true will be returned, respectively. For I == 0, this function's behavior is undefined.
  impl

proc equalTypes*(A: CXType; B: CXType): cuint =
  ##  Determine whether two CXTypes represent the same type. ** non-zero if the CXTypes represent the same type and          zero otherwise.
  impl

proc getCanonicalType*(T: CXType): CXType =
  ##  Return the canonical type for a CXType. Clang's type system explicitly models typedefs and all the ways a specific type can be represented.  The canonical type is the underlying type with all the "sugar" removed.  For example, if 'T' is a typedef for 'int', the canonical type for 'T' would be 'int'.
  impl

proc isConstQualifiedType*(T: CXType): cuint =
  ##  Determine whether a CXType has the "const" qualifier set, without looking through typedefs that may have added "const" at a different level.
  impl

proc isMacroFunctionLike*(C: CXCursor): cuint =
  ##  Determine whether a  CXCursor that is a macro, is function like.
  impl

proc isMacroBuiltin*(C: CXCursor): cuint =
  ##  Determine whether a  CXCursor that is a macro, is a builtin one.
  impl

proc isFunctionInlined*(C: CXCursor): cuint =
  ##  Determine whether a  CXCursor that is a function declaration, is an inline declaration.
  impl

proc isVolatileQualifiedType*(T: CXType): cuint =
  ##  Determine whether a CXType has the "volatile" qualifier set, without looking through typedefs that may have added "volatile" at a different level.
  impl

proc isRestrictQualifiedType*(T: CXType): cuint =
  ##  Determine whether a CXType has the "restrict" qualifier set, without looking through typedefs that may have added "restrict" at a different level.
  impl

proc getAddressSpace*(T: CXType): cuint =
  ##  Returns the address space of the given type.
  impl

proc getTypedefName*(CT: CXType): CXString =
  ##  Returns the typedef name of the given type.
  impl

proc getPointeeType*(T: CXType): CXType =
  ##  For pointer types, returns the type of the pointee.
  impl

proc getTypeDeclaration*(T: CXType): CXCursor =
  ##  Return the cursor for the declaration of the given type.
  impl

proc getDeclObjCTypeEncoding*(C: CXCursor): CXString =
  ##  Returns the Objective-C type encoding for the specified declaration.
  impl

proc getObjCEncoding*(type: CXType): CXString =
  ##  Returns the Objective-C type encoding for the specified CXType.
  impl

proc getTypeKindSpelling*(K: CXTypeKind): CXString =
  ##  Retrieve the spelling of a given CXTypeKind.
  impl

proc getFunctionTypeCallingConv*(T: CXType): CXCallingConv =
  ##  Retrieve the calling convention associated with a function type. If a non-function type is passed in, CXCallingConv_Invalid is returned.
  impl

proc getResultType*(T: CXType): CXType =
  ##  Retrieve the return type associated with a function type. If a non-function type is passed in, an invalid type is returned.
  impl

proc getExceptionSpecificationType*(T: CXType): int =
  ##  Retrieve the exception specification type associated with a function type. This is a value of type CXCursor_ExceptionSpecificationKind. If a non-function type is passed in, an error code of -1 is returned.
  impl

proc getNumArgTypes*(T: CXType): int =
  ##  Retrieve the number of non-variadic parameters associated with a function type. If a non-function type is passed in, -1 is returned.
  impl

proc getArgType*(T: CXType; i: cuint): CXType =
  ##  Retrieve the type of a parameter of a function type. If a non-function type is passed in or the function does not have enough parameters, an invalid type is returned.
  impl

proc getObjCObjectBaseType*(T: CXType): CXType =
  ##  Retrieves the base type of the ObjCObjectType. If the type is not an ObjC object, an invalid type is returned.
  impl

proc getNumObjCProtocolRefs*(T: CXType): cuint =
  ##  Retrieve the number of protocol references associated with an ObjC object/id. If the type is not an ObjC object, 0 is returned.
  impl

proc getObjCProtocolDecl*(T: CXType; i: cuint): CXCursor =
  ##  Retrieve the decl for a protocol reference for an ObjC object/id. If the type is not an ObjC object or there are not enough protocol references, an invalid cursor is returned.
  impl

proc getNumObjCTypeArgs*(T: CXType): cuint =
  ##  Retreive the number of type arguments associated with an ObjC object. If the type is not an ObjC object, 0 is returned.
  impl

proc getObjCTypeArg*(T: CXType; i: cuint): CXType =
  ##  Retrieve a type argument associated with an ObjC object. If the type is not an ObjC or the index is not valid, an invalid type is returned.
  impl

proc isFunctionTypeVariadic*(T: CXType): cuint =
  ##  Return 1 if the CXType is a variadic function type, and 0 otherwise.
  impl

proc getCursorResultType*(C: CXCursor): CXType =
  ##  Retrieve the return type associated with a given cursor. This only returns a valid type if the cursor refers to a function or method.
  impl

proc getCursorExceptionSpecificationType*(C: CXCursor): int =
  ##  Retrieve the exception specification type associated with a given cursor. This is a value of type CXCursor_ExceptionSpecificationKind. This only returns a valid result if the cursor refers to a function or method.
  impl

proc isPODType*(T: CXType): cuint =
  ##  Return 1 if the CXType is a POD (plain old data) type, and 0  otherwise.
  impl

proc getElementType*(T: CXType): CXType =
  ##  Return the element type of an array, complex, or vector type. If a type is passed in that is not an array, complex, or vector type, an invalid type is returned.
  impl

proc getNumElements*(T: CXType): clonglong =
  ##  Return the number of elements of an array or vector type. If a type is passed in that is not an array or vector type, -1 is returned.
  impl

proc getArrayElementType*(T: CXType): CXType =
  ##  Return the element type of an array type. If a non-array type is passed in, an invalid type is returned.
  impl

proc getArraySize*(T: CXType): clonglong =
  ##  Return the array size of a constant array. If a non-array type is passed in, -1 is returned.
  impl

proc getNamedType*(T: CXType): CXType =
  ##  Retrieve the type named by the qualified-id. If a non-elaborated type is passed in, an invalid type is returned.
  impl

proc isTransparentTagTypedef*(T: CXType): cuint =
  ##  Determine if a typedef is 'transparent' tag. A typedef is considered 'transparent' if it shares a name and spelling location with its underlying tag type, as is the case with the NS_ENUM macro. ** non-zero if transparent and zero otherwise.
  impl

type
  CXTypeNullabilityKind = enum
    tnkNonNull = 0, tnkNullable = 1, tnkUnspecified = 2, tnkInvalid = 3
proc getNullability*(T: CXType): CXTypeNullabilityKind =
  ##  Retrieve the nullability kind of a pointer type.
  impl

type
  CXTypeLayoutError = enum
    tleInvalid, tleIncomplete, tleDependent, tleNotConstantSize,
    tleInvalidFieldName, tleUndeduced
proc getAlignOf*(T: CXType): clonglong =
  ##  Return the alignment of a type in bytes as per C++[expr.alignof]   standard. If the type declaration is invalid, CXTypeLayoutError_Invalid is returned. If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete   is returned. If the type declaration is a dependent type, CXTypeLayoutError_Dependent is   returned. If the type declaration is not a constant size type,   CXTypeLayoutError_NotConstantSize is returned.
  impl

proc getClassType*(T: CXType): CXType =
  ##  Return the class type of an member pointer type. If a non-member-pointer type is passed in, an invalid type is returned.
  impl

proc getSizeOf*(T: CXType): clonglong =
  ##  Return the size of a type in bytes as per C++[expr.sizeof] standard. If the type declaration is invalid, CXTypeLayoutError_Invalid is returned. If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete   is returned. If the type declaration is a dependent type, CXTypeLayoutError_Dependent is   returned.
  impl

proc getOffsetOf*(T: CXType; S: cstring): clonglong =
  ##  Return the offset of a field named S in a record of type T in bits   as it would be returned by __offsetof__ as per C++11[18.2p4] If the cursor is not a record field declaration, CXTypeLayoutError_Invalid   is returned. If the field's type declaration is an incomplete type,   CXTypeLayoutError_Incomplete is returned. If the field's type declaration is a dependent type,   CXTypeLayoutError_Dependent is returned. If the field's name S is not found,   CXTypeLayoutError_InvalidFieldName is returned.
  impl

proc getModifiedType*(T: CXType): CXType =
  ##  Return the type that was modified by this attributed type. If the type is not an attributed type, an invalid type is returned.
  impl

proc getOffsetOfField*(C: CXCursor): clonglong =
  ##  Return the offset of the field represented by the Cursor. If the cursor is not a field declaration, -1 is returned. If the cursor semantic parent is not a record field declaration,   CXTypeLayoutError_Invalid is returned. If the field's type declaration is an incomplete type,   CXTypeLayoutError_Incomplete is returned. If the field's type declaration is a dependent type,   CXTypeLayoutError_Dependent is returned. If the field's name S is not found,   CXTypeLayoutError_InvalidFieldName is returned.
  impl

proc isAnonymous*(C: CXCursor): cuint =
  ##  Determine whether the given cursor represents an anonymous tag or namespace
  impl

proc isAnonymousRecordDecl*(C: CXCursor): cuint =
  ##  Determine whether the given cursor represents an anonymous record declaration.
  impl

proc isInlineNamespace*(C: CXCursor): cuint =
  ##  Determine whether the given cursor represents an inline namespace declaration.
  impl

type
  CXRefQualifierKind = enum
    rqkNone = 0, rqkLValue, rqkRValue
proc getNumTemplateArguments*(T: CXType): int =
  ##  Returns the number of template arguments for given template specialization, or -1 if type Error: cannot render: rnLiteralBlock is not a template specialization.
  impl

proc getTemplateArgumentAsType*(T: CXType; i: cuint): CXType =
  ##  Returns the type template argument of a template class specialization at given index. This function only returns template type arguments and does not handle template template arguments or variadic packs.
  impl

proc getCXXRefQualifier*(T: CXType): CXRefQualifierKind =
  ##  Retrieve the ref-qualifier kind of a function or method. The ref-qualifier is returned for C++ functions or methods. For other types or non-C++ declarations, CXRefQualifier_None is returned.
  impl

proc isBitField*(C: CXCursor): cuint =
  ##  Returns non-zero if the cursor specifies a Record member that is a   bitfield.
  impl

proc isVirtualBase*(argCXCursor: CXCursor): cuint =
  ##  Returns 1 if the base class specified by the cursor with kind   CX_CXXBaseSpecifier is virtual.
  impl

type
  CX_CXXAccessSpecifier = enum
    asInvalidAccessSpecifier, asPublic, asProtected, asPrivate
proc getCXXAccessSpecifier*(argCXCursor: CXCursor): CX_CXXAccessSpecifier =
  ##  Returns the access control level for the referenced object. If the cursor refers to a C++ declaration, its access control level within its parent scope is returned. Otherwise, if the cursor refers to a base specifier or access specifier, the specifier itself is returned.
  impl

type
  CX_StorageClass = enum
    scC_Invalid, scC_None, scC_Extern, scC_Static, scC_PrivateExtern,
    scC_OpenCLWorkGroupLocal, scC_Auto, scC_Register
proc getStorageClass*(argCXCursor: CXCursor): CX_StorageClass =
  ##  Returns the storage class for a function or variable declaration. If the passed in Cursor is not a function or variable declaration, CX_SC_Invalid is returned else the storage class.
  impl

proc getNumOverloadedDecls*(cursor: CXCursor): cuint =
  ##  Determine the number of overloaded declarations referenced by a Error: cannot render: rnLiteralBlock cursor. ** The cursor whose overloaded declarations are being queried. ** The number of overloaded declarations referenced by Error: cannot render: rnLiteralBlock If it is not a Error: cannot render: rnLiteralBlock cursor, returns 0.
  impl

proc getOverloadedDecl*(cursor: CXCursor; index: cuint): CXCursor =
  ##  Retrieve a cursor for one of the overloaded declarations referenced by a Error: cannot render: rnLiteralBlock cursor. ** The cursor whose overloaded declarations are being queried. ** The zero-based index into the set of overloaded declarations in the cursor. ** A cursor representing the declaration referenced by the given Error: cannot render: rnLiteralBlock at the specified Error: cannot render: rnLiteralBlock If the cursor does not have an associated set of overloaded declarations, or if the index is out of bounds, returns Error: cannot render: rnLiteralBlock 
  impl

proc getIBOutletCollectionType*(argCXCursor: CXCursor): CXType =
  ##  For cursors representing an iboutletcollection attribute,  this function returns the collection element type. 
  impl

type
  CXChildVisitResult = enum
    cvrBreak, cvrContinue, cvrRecurse
proc visitChildren*(parent: CXCursor; visitor: CXCursorVisitor;
                   client_data: CXClientData): cuint =
  ##  Visit the children of a particular cursor. This function visits all the direct children of the given cursor, invoking the given Error: cannot render: rnLiteralBlock function with the cursors of each visited child. The traversal may be recursive, if the visitor returns Error: cannot render: rnLiteralBlock The traversal may also be ended prematurely, if the visitor returns Error: cannot render: rnLiteralBlock ** the cursor whose child may be visited. All kinds of cursors can be visited, including invalid cursors (which, by definition, have no children). ** the visitor function that will be invoked for each child of Error: cannot render: rnLiteralBlock ** pointer data supplied by the client, which will be passed to the visitor each time it is invoked. ** a non-zero value if the traversal was terminated prematurely by the visitor returning Error: cannot render: rnLiteralBlock 
  impl

proc getCursorUSR*(argCXCursor: CXCursor): CXString =
  ##  Retrieve a Unified Symbol Resolution (USR) for the entity referenced by the given cursor. A Unified Symbol Resolution (USR) is a string that identifies a particular entity (function, class, variable, etc.) within a program. USRs can be compared across translation units to determine, e.g., when references in one translation refer to an entity defined in another translation unit.
  impl

proc *(class_name: cstring): CXString =
  ##  Construct a USR for a specified Objective-C class.
  impl

proc *(class_name: cstring; category_name: cstring): CXString =
  ##  Construct a USR for a specified Objective-C category.
  impl

proc *(protocol_name: cstring): CXString =
  ##  Construct a USR for a specified Objective-C protocol.
  impl

proc *(name: cstring; classUSR: CXString): CXString =
  ##  Construct a USR for a specified Objective-C instance variable and   the USR for its containing class.
  impl

proc *(name: cstring; isInstanceMethod: cuint; classUSR: CXString): CXString =
  ##  Construct a USR for a specified Objective-C method and   the USR for its containing class.
  impl

proc *(property: cstring; classUSR: CXString): CXString =
  ##  Construct a USR for a specified Objective-C property and the USR  for its containing class.
  impl

proc getCursorSpelling*(argCXCursor: CXCursor): CXString =
  ##  Retrieve a name for the entity referenced by this cursor.
  impl

proc getSpellingNameRange*(argCXCursor: CXCursor; pieceIndex: cuint; options: cuint): CXSourceRange =
  ##  Retrieve a range for a piece that forms the cursors spelling name. Most of the times there is only one range for the complete spelling but for Objective-C methods and Objective-C message expressions, there are multiple pieces for each selector identifier. ** the index of the spelling name piece. If this is greater than the actual number of pieces, it will return a NULL (invalid) range. ** Reserved.
  impl

type
  CXPrintingPolicyProperty = enum
    pppIndentation, pppSuppressSpecifiers, pppSuppressTagKeyword,
    pppIncludeTagDefinition, pppSuppressScope, pppSuppressUnwrittenScope,
    pppSuppressInitializers, pppConstantArraySizeAsWritten,
    pppAnonymousTagLocations, pppSuppressStrongLifetime,
    pppSuppressLifetimeQualifiers, pppSuppressTemplateArgsInCXXConstructors,
    pppBool, pppRestrict, pppAlignof, pppUnderscoreAlignof, pppUseVoidForZeroParams,
    pppTerseOutput, pppPolishForDeclaration, pppHalf, pppMSWChar,
    pppIncludeNewlines, pppMSVCFormatting, pppConstantsAsWritten,
    pppSuppressImplicitBase, pppFullyQualifiedName, pppLastProperty
proc getProperty*(Policy: CXPrintingPolicy; Property: CXPrintingPolicyProperty): cuint =
  ##  Get a property value for the given printing policy.
  impl

proc setProperty*(Policy: CXPrintingPolicy; Property: CXPrintingPolicyProperty;
                 Value: cuint): void =
  ##  Set a property value for the given printing policy.
  impl

proc getCursorPrintingPolicy*(argCXCursor: CXCursor): CXPrintingPolicy =
  ##  Retrieve the default policy for the cursor. The policy should be released after use with Error: cannot render: rnLiteralBlock 
  impl

proc dispose*(Policy: CXPrintingPolicy): void =
  ##  Release a printing policy.
  impl

proc getCursorPrettyPrinted*(Cursor: CXCursor; Policy: CXPrintingPolicy): CXString =
  ##  Pretty print declarations. ** The cursor representing a declaration. ** The policy to control the entities being printed. If NULL, a default policy is used. ** The pretty printed declaration or the empty string for other cursors.
  impl

proc getCursorDisplayName*(argCXCursor: CXCursor): CXString =
  ##  Retrieve the display name for the entity referenced by this cursor. The display name contains extra information that helps identify the cursor, such as the parameters of a function or template or the arguments of a class template specialization.
  impl

proc getCursorReferenced*(argCXCursor: CXCursor): CXCursor =
  ##  For a cursor that is a reference, retrieve a cursor representing the entity that it references. Reference cursors refer to other entities in the AST. For example, an Objective-C superclass reference cursor refers to an Objective-C class. This function produces the cursor for the Objective-C class from the cursor for the superclass reference. If the input cursor is a declaration or definition, it returns that declaration or definition unchanged. Otherwise, returns the NULL cursor.
  impl

proc getCursorDefinition*(argCXCursor: CXCursor): CXCursor =
  ##   For a cursor that is either a reference to or a declaration  of some entity, retrieve a cursor that describes the definition of  that entity.  Some entities can be declared multiple times within a translation  unit, but only one of those declarations can also be a  definition. For example, given:  Error: cannot render: rnCodeBlock  there are three declarations of the function "f", but only the  second one is a definition. The clang_getCursorDefinition()  function will take any cursor pointing to a declaration of "f"  (the first or fourth lines of the example) or a cursor referenced  that uses "f" (the call to "f' inside "g") and will return a  declaration cursor pointing to the definition (the second "f"  declaration).  If given a cursor for which there is no corresponding definition,  e.g., because there is no definition of that entity within this  translation unit, returns a NULL cursor.
  impl

proc isCursorDefinition*(argCXCursor: CXCursor): cuint =
  ##  Determine whether the declaration pointed to by this cursor is also a definition of that entity.
  impl

proc getCanonicalCursor*(argCXCursor: CXCursor): CXCursor =
  ##  Retrieve the canonical cursor corresponding to the given cursor. In the C family of languages, many kinds of entities can be declared several times within a single translation unit. For example, a structure type can be forward-declared (possibly multiple times) and later defined: Error: cannot render: rnCodeBlock The declarations and the definition of Error: cannot render: rnLiteralBlock are represented by three different cursors, all of which are declarations of the same underlying entity. One of these cursor is considered the "canonical" cursor, which is effectively the representative for the underlying entity. One can determine if two cursors are declarations of the same underlying entity by comparing their canonical cursors. ** The canonical cursor for the entity referred to by the given cursor.
  impl

proc getObjCSelectorIndex*(argCXCursor: CXCursor): int =
  ##  If the cursor points to a selector identifier in an Objective-C method or message expression, this returns the selector index. After getting a cursor with #clang_getCursor, this can be called to determine if the location points to a selector identifier. ** The selector index if the cursor is an Objective-C method or message expression and the cursor is pointing to a selector identifier, or -1 otherwise.
  impl

proc isDynamicCall*(C: CXCursor): int =
  ##  Given a cursor pointing to a C++ method call or an Objective-C message, returns non-zero if the method/message is "dynamic", meaning: For a C++ method: the call is virtual. For an Objective-C message: the receiver is an object instance, not 'super' or a specific class. If the method/message is "static" or the cursor does not point to a method/message, it will return zero.
  impl

proc getReceiverType*(C: CXCursor): CXType =
  ##  Given a cursor pointing to an Objective-C message or property reference, or C++ method call, returns the CXType of the receiver.
  impl

type
  CXObjCPropertyAttrKind = enum
    ocpaknoattr = 0, ocpakreadonly = 1, ocpakgetter = 2, ocpakassign = 4,
    ocpakreadwrite = 8, ocpakretain = 16, ocpakcopy = 32, ocpaknonatomic = 64,
    ocpaksetter = 128, ocpakatomic = 256, ocpakweak = 512, ocpakstrong = 1024,
    ocpakunsafe_unretained = 2048, ocpakclass = 4096
type
  CXObjCPropertyAttrKind = enum
    ocpaknoattr = 0, ocpakreadonly = 1, ocpakgetter = 2, ocpakassign = 4,
    ocpakreadwrite = 8, ocpakretain = 16, ocpakcopy = 32, ocpaknonatomic = 64,
    ocpaksetter = 128, ocpakatomic = 256, ocpakweak = 512, ocpakstrong = 1024,
    ocpakunsafe_unretained = 2048, ocpakclass = 4096
proc getObjCPropertyAttributes*(C: CXCursor; reserved: cuint): cuint =
  ##  Given a cursor that represents a property declaration, return the associated property attributes. The bits are formed from Error: cannot render: rnLiteralBlock ** Reserved for future use, pass 0.
  impl

proc getObjCPropertyGetterName*(C: CXCursor): CXString =
  ##  Given a cursor that represents a property declaration, return the name of the method that implements the getter.
  impl

proc getObjCPropertySetterName*(C: CXCursor): CXString =
  ##  Given a cursor that represents a property declaration, return the name of the method that implements the setter, if any.
  impl

type
  CXObjCDeclQualifierKind = enum
    ocdqkNone = 0, ocdqkIn = 1, ocdqkInout = 2, ocdqkOut = 4, ocdqkBycopy = 8, ocdqkByref = 16,
    ocdqkOneway = 32
type
  CXObjCDeclQualifierKind = enum
    ocdqkNone = 0, ocdqkIn = 1, ocdqkInout = 2, ocdqkOut = 4, ocdqkBycopy = 8, ocdqkByref = 16,
    ocdqkOneway = 32
proc getObjCDeclQualifiers*(C: CXCursor): cuint =
  ##  Given a cursor that represents an Objective-C method or parameter declaration, return the associated Objective-C qualifiers for the return type or the parameter respectively. The bits are formed from CXObjCDeclQualifierKind.
  impl

proc isObjCOptional*(C: CXCursor): cuint =
  ##  Given a cursor that represents an Objective-C method or property declaration, return non-zero if the declaration was affected by "@optional". Returns zero if the cursor is not such a declaration or it is "@required".
  impl

proc isVariadic*(C: CXCursor): cuint =
  ##  Returns non-zero if the given cursor is a variadic function or method.
  impl

proc isExternalSymbol*(C: CXCursor; language: ptr[CXString];
                      definedIn: ptr[CXString]; isGenerated: ptr[cuint]): cuint =
  ##  Returns non-zero if the given cursor points to a symbol marked with external_source_symbol attribute. ** If non-NULL, and the attribute is present, will be set to the 'language' string from the attribute. ** If non-NULL, and the attribute is present, will be set to the 'definedIn' string from the attribute. ** If non-NULL, and the attribute is present, will be set to non-zero if the 'generated_declaration' is set in the attribute.
  impl

proc getCommentRange*(C: CXCursor): CXSourceRange =
  ##  Given a cursor that represents a declaration, return the associated comment's source range.  The range may include multiple consecutive comments with whitespace in between.
  impl

proc getRawCommentText*(C: CXCursor): CXString =
  ##  Given a cursor that represents a declaration, return the associated comment text, including comment markers.
  impl

proc getBriefCommentText*(C: CXCursor): CXString =
  ##  Given a cursor that represents a documentable entity (e.g., declaration), return the associated `` first paragraph.
  impl

proc getMangling*(argCXCursor: CXCursor): CXString =
  ##  Retrieve the CXString representing the mangled name of the cursor.
  impl

proc getCXXManglings*(argCXCursor: CXCursor): ptr[CXStringSet] =
  ##  Retrieve the CXStrings representing the mangled symbols of the C++ constructor or destructor at the cursor.
  impl

proc getObjCManglings*(argCXCursor: CXCursor): ptr[CXStringSet] =
  ##  Retrieve the CXStrings representing the mangled symbols of the ObjC class interface or implementation at the cursor.
  impl

proc getModule*(C: CXCursor): CXModule =
  ##  Given a CXCursor_ModuleImportDecl cursor, return the associated module.
  impl

proc getModuleForFile*(argCXTranslationUnit: CXTranslationUnit; argCXFile: CXFile): CXModule =
  ##  Given a CXFile header file, return the module that contains it, if one exists.
  impl

proc getASTFile*(Module: CXModule): CXFile =
  ##  ** a module object. ** the module file where the provided module object came from.
  impl

proc getParent*(Module: CXModule): CXModule =
  ##  ** a module object. ** the parent of a sub-module or NULL if the given module is top-level, e.g. for 'std.vector' it will return the 'std' module.
  impl

proc getName*(Module: CXModule): CXString =
  ##  ** a module object. ** the name of the module, e.g. for the 'std.vector' sub-module it will return "vector".
  impl

proc getFullName*(Module: CXModule): CXString =
  ##  ** a module object. ** the full name of the module, e.g. "std.vector".
  impl

proc isSystem*(Module: CXModule): int =
  ##  ** a module object. ** non-zero if the module is a system one.
  impl

proc Module_getNumTopLevelHeaders*(argCXTranslationUnit: CXTranslationUnit;
                                  Module: CXModule): cuint =
  ##  ** a module object. ** the number of top level headers associated with this module.
  impl

proc Module_getTopLevelHeader*(argCXTranslationUnit: CXTranslationUnit;
                              Module: CXModule; Index: cuint): CXFile =
  ##  ** a module object. ** top level header index (zero-based). ** the specified top level header associated with the module.
  impl

proc CXXConstructor_isConvertingConstructor*(C: CXCursor): cuint =
  ##  Determine if a C++ constructor is a converting constructor.
  impl

proc CXXConstructor_isCopyConstructor*(C: CXCursor): cuint =
  ##  Determine if a C++ constructor is a copy constructor.
  impl

proc CXXConstructor_isDefaultConstructor*(C: CXCursor): cuint =
  ##  Determine if a C++ constructor is the default constructor.
  impl

proc CXXConstructor_isMoveConstructor*(C: CXCursor): cuint =
  ##  Determine if a C++ constructor is a move constructor.
  impl

proc CXXField_isMutable*(C: CXCursor): cuint =
  ##  Determine if a C++ field is declared 'mutable'.
  impl

proc CXXMethod_isDefaulted*(C: CXCursor): cuint =
  ##  Determine if a C++ method is declared '= default'.
  impl

proc CXXMethod_isPureVirtual*(C: CXCursor): cuint =
  ##  Determine if a C++ member function or member function template is pure virtual.
  impl

proc CXXMethod_isStatic*(C: CXCursor): cuint =
  ##  Determine if a C++ member function or member function template is declared 'static'.
  impl

proc CXXMethod_isVirtual*(C: CXCursor): cuint =
  ##  Determine if a C++ member function or member function template is explicitly declared 'virtual' or if it overrides a virtual method from one of the base classes.
  impl

proc CXXRecord_isAbstract*(C: CXCursor): cuint =
  ##  Determine if a C++ record is abstract, i.e. whether a class or struct has a pure virtual member function.
  impl

proc EnumDecl_isScoped*(C: CXCursor): cuint =
  ##  Determine if an enum declaration refers to a scoped enum.
  impl

proc CXXMethod_isConst*(C: CXCursor): cuint =
  ##  Determine if a C++ member function or member function template is declared 'const'.
  impl

proc getTemplateCursorKind*(C: CXCursor): CXCursorKind =
  ##  Given a cursor that represents a template, determine the cursor kind of the specializations would be generated by instantiating the template. This routine can be used to determine what flavor of function template, class template, or class template partial specialization is stored in the cursor. For example, it can describe whether a class template cursor is declared with "struct", "class" or "union". ** The cursor to query. This cursor should represent a template declaration. ** The cursor kind of the specializations that would be generated by instantiating the template Error: cannot render: rnLiteralBlock If Error: cannot render: rnLiteralBlock is not a template, returns Error: cannot render: rnLiteralBlock 
  impl

proc getSpecializedCursorTemplate*(C: CXCursor): CXCursor =
  ##  Given a cursor that may represent a specialization or instantiation of a template, retrieve the cursor that represents the template that it specializes or from which it was instantiated. This routine determines the template involved both for explicit specializations of templates and for implicit instantiations of the template, both of which are referred to as "specializations". For a class template specialization (e.g., Error: cannot render: rnLiteralBlock this routine will return either the primary template (Error: cannot render: rnLiteralBlock or, if the specialization was instantiated from a class template partial specialization, the class template partial specialization. For a class template partial specialization and a function template specialization (including instantiations), this this routine will return the specialized template. For members of a class template (e.g., member functions, member classes, or static data members), returns the specialized or instantiated member. Although not strictly "templates" in the C++ language, members of class templates have the same notions of specializations and instantiations that templates do, so this routine treats them similarly. ** A cursor that may be a specialization of a template or a member of a template. ** If the given cursor is a specialization or instantiation of a template or a member thereof, the template or member that it specializes or from which it was instantiated. Otherwise, returns a NULL cursor.
  impl

proc getCursorReferenceNameRange*(C: CXCursor; NameFlags: cuint; PieceIndex: cuint): CXSourceRange =
  ##  Given a cursor that references something else, return the source range covering that reference. ** A cursor pointing to a member reference, a declaration reference, or an operator call. ** A bitset with three independent flags: CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and CXNameRange_WantSinglePiece. ** For contiguous names or when passing the flag CXNameRange_WantSinglePiece, only one piece with index 0 is available. When the CXNameRange_WantSinglePiece flag is not passed for a non-contiguous names, this index can be used to retrieve the individual pieces of the name. See also CXNameRange_WantSinglePiece. ** The piece of the name pointed to by the given cursor. If there is no name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
  impl

type
  CXNameRefFlags = enum
    nrfange_WantQualifier = 1, nrfange_WantTemplateArgs = 2,
    nrfange_WantSinglePiece = 4
type
  CXTokenKind = enum
    tkPunctuation, tkKeyword, tkIdentifier, tkLiteral, tkComment
type
  CXTokenKind = enum
    tkPunctuation, tkKeyword, tkIdentifier, tkLiteral, tkComment
proc getToken*(TU: CXTranslationUnit; Location: CXSourceLocation): ptr[CXToken] =
  ##  Get the raw lexical token starting with the given location. ** the translation unit whose text is being tokenized. ** the source location with which the token starts. ** The token starting with the given location or NULL if no such token exist. The returned pointer must be freed with clang_disposeTokens before the translation unit is destroyed.
  impl

proc getTokenKind*(argCXToken: CXToken): CXTokenKind =
  ##  Determine the kind of the given token.
  impl

proc getTokenSpelling*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken): CXString =
  ##  Determine the spelling of the given token. The spelling of a token is the textual representation of that token, e.g., the text of an identifier or keyword.
  impl

proc getTokenLocation*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken): CXSourceLocation =
  ##  Retrieve the source location of the given token.
  impl

proc getTokenExtent*(argCXTranslationUnit: CXTranslationUnit; argCXToken: CXToken): CXSourceRange =
  ##  Retrieve a source range that covers the given token.
  impl

proc tokenize*(TU: CXTranslationUnit; Range: CXSourceRange;
              Tokens: ptr[ptr[CXToken]]; NumTokens: ptr[cuint]): void =
  ##  Tokenize the source code described by the given range into raw lexical tokens. ** the translation unit whose text is being tokenized. ** the source range in which text should be tokenized. All of the tokens produced by tokenization will fall within this source range, ** this pointer will be set to point to the array of tokens that occur within the given source range. The returned pointer must be freed with clang_disposeTokens() before the translation unit is destroyed. ** will be set to the number of tokens in the Error: cannot render: rnLiteralBlock array. 
  impl

proc annotateTokens*(TU: CXTranslationUnit; Tokens: ptr[CXToken]; NumTokens: cuint;
                    Cursors: ptr[CXCursor]): void =
  ##  Annotate the given set of tokens by providing cursors for each token that can be mapped to a specific entity within the abstract syntax tree. This token-annotation routine is equivalent to invoking clang_getCursor() for the source locations of each of the tokens. The cursors provided are filtered, so that only those cursors that have a direct correspondence to the token are accepted. For example, given a function call Error: cannot render: rnLiteralBlock clang_getCursor() would provide the following cursors:   * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.   * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.   * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'. Only the first and last of these cursors will occur within the annotate, since the tokens "f" and "x' directly refer to a function and a variable, respectively, but the parentheses are just a small part of the full syntax of the function call expression, which is not provided as an annotation. ** the translation unit that owns the given tokens. ** the set of tokens to annotate. ** the number of tokens in Error: cannot render: rnLiteralBlock ** an array of Error: cannot render: rnLiteralBlock cursors, whose contents will be replaced with the cursors corresponding to each token.
  impl

proc disposeTokens*(TU: CXTranslationUnit; Tokens: ptr[CXToken]; NumTokens: cuint): void =
  ##  Free the given set of tokens.
  impl

proc getCursorKindSpelling*(Kind: CXCursorKind): CXString =
  ##  `` These routines are used for testing and debugging, only, and should not be relied upon. @{
  impl

proc getDefinitionSpellingAndExtent*(argCXCursor: CXCursor; startBuf: ptr[cstring];
                                    endBuf: ptr[cstring]; startLine: ptr[cuint];
                                    startColumn: ptr[cuint]; endLine: ptr[cuint];
                                    endColumn: ptr[cuint]): void =
  impl

proc enableStackTraces*(): void =
  impl

proc *(fn: ptr[!!!]; user_data: ptr[void]; stack_size: cuint): void =
  impl

type
  CXCompletionChunkKind = enum
    cckOptional, cckTypedText, cckText, cckPlaceholder, cckInformative,
    cckCurrentParameter, cckLeftParen, cckRightParen, cckLeftBracket,
    cckRightBracket, cckLeftBrace, cckRightBrace, cckLeftAngle, cckRightAngle,
    cckComma, cckResultType, cckColon, cckSemiColon, cckEqual, cckHorizontalSpace,
    cckVerticalSpace
proc getCompletionChunkKind*(completion_string: CXCompletionString;
                            chunk_number: cuint): CXCompletionChunkKind =
  ##  Determine the kind of a particular chunk within a completion string. ** the completion string to query. ** the 0-based index of the chunk in the completion string. ** the kind of the chunk at the index Error: cannot render: rnLiteralBlock 
  impl

proc getCompletionChunkText*(completion_string: CXCompletionString;
                            chunk_number: cuint): CXString =
  ##  Retrieve the text associated with a particular chunk within a completion string. ** the completion string to query. ** the 0-based index of the chunk in the completion string. ** the text associated with the chunk at index Error: cannot render: rnLiteralBlock 
  impl

proc getCompletionChunkCompletionString*(completion_string: CXCompletionString;
                                        chunk_number: cuint): CXCompletionString =
  ##  Retrieve the completion string associated with a particular chunk within a completion string. ** the completion string to query. ** the 0-based index of the chunk in the completion string. ** the completion string associated with the chunk at index Error: cannot render: rnLiteralBlock 
  impl

proc getNumCompletionChunks*(completion_string: CXCompletionString): cuint =
  ##  Retrieve the number of chunks in the given code-completion string.
  impl

proc getCompletionPriority*(completion_string: CXCompletionString): cuint =
  ##  Determine the priority of this code completion. The priority of a code completion indicates how likely it is that this particular completion is the completion that the user will select. The priority is selected by various internal heuristics. ** The completion string to query. ** The priority of this completion string. Smaller values indicate higher-priority (more likely) completions.
  impl

proc getCompletionAvailability*(completion_string: CXCompletionString): CXAvailabilityKind =
  ##  Determine the availability of the entity that this code-completion string refers to. ** The completion string to query. ** The availability of the completion string.
  impl

proc getCompletionNumAnnotations*(completion_string: CXCompletionString): cuint =
  ##  Retrieve the number of annotations associated with the given completion string. ** the completion string to query. ** the number of annotations associated with the given completion string.
  impl

proc getCompletionAnnotation*(completion_string: CXCompletionString;
                             annotation_number: cuint): CXString =
  ##  Retrieve the annotation associated with the given completion string. ** the completion string to query. ** the 0-based index of the annotation of the completion string. ** annotation string associated with the completion at index Error: cannot render: rnLiteralBlock or a NULL string if that annotation is not available.
  impl

proc getCompletionParent*(completion_string: CXCompletionString;
                         kind: ptr[CXCursorKind]): CXString =
  ##  Retrieve the parent context of the given completion string. The parent context of a completion string is the semantic parent of the declaration (if any) that the code completion represents. For example, a code completion for an Objective-C method would have the method's class or protocol as its context. ** The code completion string whose parent is being queried. ** DEPRECATED: always set to CXCursor_NotImplemented if non-NULL. ** The name of the completion parent, e.g., "NSObject" if the completion string represents a method in the NSObject class.
  impl

proc getCompletionBriefComment*(completion_string: CXCompletionString): CXString =
  ##  Retrieve the brief documentation comment attached to the declaration that corresponds to the given completion string.
  impl

proc getCursorCompletionString*(cursor: CXCursor): CXCompletionString =
  ##  Retrieve a completion string for an arbitrary declaration or macro definition cursor. ** The cursor to query. ** A non-context-sensitive completion string for declaration and macro definition cursors, or NULL for other kinds of cursors.
  impl

proc getCompletionNumFixIts*(results: ptr[CXCodeCompleteResults];
                            completion_index: cuint): cuint =
  ##  Retrieve the number of fix-its for the given completion index. Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts option was set. ** The structure keeping all completion results ** The index of the completion ** The number of fix-its which must be applied before the completion at completion_index can be applied
  impl

proc getCompletionFixIt*(results: ptr[CXCodeCompleteResults];
                        completion_index: cuint; fixit_index: cuint;
                        replacement_range: ptr[CXSourceRange]): CXString =
  ##  Fix-its that *must* be applied before inserting the text for the corresponding completion. By default, clang_codeCompleteAt() only returns completions with empty fix-its. Extra completions with non-empty fix-its should be explicitly requested by setting CXCodeComplete_IncludeCompletionsWithFixIts. For the clients to be able to compute position of the cursor after applying fix-its, the following conditions are guaranteed to hold for replacement_range of the stored fix-its:  - Ranges in the fix-its are guaranteed to never contain the completion  point (or identifier under completion point, if any) inside them, except  at the start or at the end of the range.  - If a fix-it range starts or ends with completion point (or starts or  ends after the identifier under completion point), it will contain at  least one character. It allows to unambiguously recompute completion  point after applying the fix-it. The intuition is that provided fix-its change code around the identifier we complete, but are not allowed to touch the identifier itself or the completion point. One example of completions with corrections are the ones replacing '.' with '->' and vice versa: std::unique_ptr<std::vector<int>> vec_ptr; In 'vec_ptr.^', one of the completions is 'push_back', it requires replacing '.' with '->'. In 'vec_ptr->^', one of the completions is 'release', it requires replacing '->' with '.'. ** The structure keeping all completion results ** The index of the completion ** The index of the fix-it for the completion at completion_index ** The fix-it range that must be replaced before the completion at completion_index can be applied ** The fix-it string that must replace the code at replacement_range before the completion at completion_index can be applied
  impl

type
  CXCodeComplete_Flags = enum
    ccfIncludeMacros = 1, ccfIncludeCodePatterns = 2, ccfIncludeBriefComments = 4,
    ccfSkipPreamble = 8, ccfIncludeCompletionsWithFixIts = 16
type
  CXCompletionContext = enum
    ccUnexposed = 0, ccAnyType, ccAnyValue, ccObjCObjectValue, ccObjCSelectorValue,
    ccCXXClassTypeValue, ccDotMemberAccess, ccArrowMemberAccess,
    ccObjCPropertyAccess, ccEnumTag, ccUnionTag, ccStructTag, ccClassTag,
    ccNamespace, ccNestedNameSpecifier, ccObjCInterface, ccObjCProtocol,
    ccObjCCategory, ccObjCInstanceMessage, ccObjCClassMessage, ccObjCSelectorName,
    ccMacroName, ccNaturalLanguage, ccIncludedFile, ccUnknown
proc defaultCodeCompleteOptions*(): cuint =
  ##  Returns a default set of code-completion options that can be passed toError: cannot render: rnLiteralBlock 
  impl

proc codeCompleteAt*(TU: CXTranslationUnit; complete_filename: cstring;
                    complete_line: cuint; complete_column: cuint;
                    unsaved_files: ptr[CXUnsavedFile]; num_unsaved_files: cuint;
                    options: cuint): ptr[CXCodeCompleteResults] =
  ##  Perform code completion at a given location in a translation unit. This function performs code completion at a particular file, line, and column within source code, providing results that suggest potential code snippets based on the context of the completion. The basic model for code completion is that Clang will parse a complete source file, performing syntax checking up to the location where code-completion has been requested. At that point, a special code-completion token is passed to the parser, which recognizes this token and determines, based on the current location in the C/Objective-C/C++ grammar and the state of semantic analysis, what completions to provide. These completions are returned via a new Error: cannot render: rnLiteralBlock structure. Code completion itself is meant to be triggered by the client when the user types punctuation characters or whitespace, at which point the code-completion location will coincide with the cursor. For example, if Error: cannot render: rnLiteralBlock is a pointer, code-completion might be triggered after the "-" and then after the ">" in Error: cannot render: rnLiteralBlock When the code-completion location is after the ">", the completion results will provide, e.g., the members of the struct that "p" points to. The client is responsible for placing the cursor at the beginning of the token currently being typed, then filtering the results based on the contents of the token. For example, when code-completing for the expression Error: cannot render: rnLiteralBlock the client should provide the location just after the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the client can filter the results based on the current token text ("get"), only showing those results that start with "get". The intent of this interface is to separate the relatively high-latency acquisition of code-completion results from the filtering of results on a per-character basis, which must have a lower latency. ** The translation unit in which code-completion should occur. The source files for this translation unit need not be completely up-to-date (and the contents of those source files may be overridden via Error: cannot render: rnLiteralBlock Cursors referring into the translation unit may be invalidated by this invocation. ** The name of the source file where code completion should be performed. This filename may be any file included in the translation unit. ** The line at which code-completion should occur. ** The column at which code-completion should occur. Note that the column should point just after the syntactic construct that initiated code completion, and not in the middle of a lexical token. ** the Files that have not yet been saved to disk but may be required for parsing or code completion, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns. ** The number of unsaved file entries in Error: cannot render: rnLiteralBlock ** Extra options that control the behavior of code completion, expressed as a bitwise OR of the enumerators of the CXCodeComplete_Flags enumeration. The Error: cannot render: rnLiteralBlock function returns a default set of code-completion options. ** If successful, a new Error: cannot render: rnLiteralBlock structure containing code-completion results, which should eventually be freed with Error: cannot render: rnLiteralBlock If code completion fails, returns NULL.
  impl

proc sortCodeCompletionResults*(Results: ptr[CXCompletionResult]; NumResults: cuint): void =
  ##  Sort the code-completion results in case-insensitive alphabetical order. ** The set of results to sort. ** The number of results in Error: cannot render: rnLiteralBlock 
  impl

proc disposeCodeCompleteResults*(Results: ptr[CXCodeCompleteResults]): void =
  ##  Free the given set of code-completion results.
  impl

proc codeCompleteGetNumDiagnostics*(Results: ptr[CXCodeCompleteResults]): cuint =
  ##  Determine the number of diagnostics produced prior to the location where code completion was performed.
  impl

proc codeCompleteGetDiagnostic*(Results: ptr[CXCodeCompleteResults]; Index: cuint): CXDiagnostic =
  ##  Retrieve a diagnostic associated with the given code completion. ** the code completion results to query. ** the zero-based diagnostic number to retrieve. ** the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
  impl

proc codeCompleteGetContexts*(Results: ptr[CXCodeCompleteResults]): culonglong =
  ##  Determines what completions are appropriate for the context the given code completion. ** the code completion results to query ** the kinds of completions that are appropriate for use along with the given code completion results.
  impl

proc codeCompleteGetContainerKind*(Results: ptr[CXCodeCompleteResults];
                                  IsIncomplete: ptr[cuint]): CXCursorKind =
  ##  Returns the cursor kind for the container for the current code completion context. The container is only guaranteed to be set for contexts where a container exists (i.e. member accesses or Objective-C message sends); if there is not a container, this function will return CXCursor_InvalidCode. ** the code completion results to query ** on return, this value will be false if Clang has complete information about the container. If Clang does not have complete information, this value will be true. ** the container kind, or CXCursor_InvalidCode if there is not a container
  impl

proc codeCompleteGetContainerUSR*(Results: ptr[CXCodeCompleteResults]): CXString =
  ##  Returns the USR for the container for the current code completion context. If there is not a container for the current context, this function will return the empty string. ** the code completion results to query ** the USR for the container
  impl

proc codeCompleteGetObjCSelector*(Results: ptr[CXCodeCompleteResults]): CXString =
  ##  Returns the currently-entered selector for an Objective-C message send, formatted like "initWithFoo:bar:". Only guaranteed to return a non-empty string for CXCompletionContext_ObjCInstanceMessage and CXCompletionContext_ObjCClassMessage. ** the code completion results to query ** the selector (or partial selector) that has been entered thus far for an Objective-C message send.
  impl

proc getClangVersion*(): CXString =
  ##  Return a version string, suitable for showing to a user, but not        intended to be parsed (the format is not guaranteed to be stable).
  impl

proc *(isEnabled: cuint): void =
  ##  Enable/disable crash recovery. ** Flag to indicate if crash recovery is enabled.  A non-zero        value enables crash recovery, while 0 disables it.
  impl

proc getInclusions*(tu: CXTranslationUnit; visitor: CXInclusionVisitor;
                   client_data: CXClientData): void =
  ##  Visit the set of preprocessor inclusions in a translation unit.   The visitor function is called with the provided data for every included   file.  This does not include headers included by the PCH file (unless one   is inspecting the inclusions in the PCH file itself).
  impl

type
  CXEvalResultKind = enum
    erkInt = 1, erkFloat = 2, erkObjCStrLiteral = 3, erkStrLiteral = 4, erkCFStr = 5,
    erkOther = 6, erkUnExposed = 0
type
  CXEvalResultKind = enum
    erkInt = 1, erkFloat = 2, erkObjCStrLiteral = 3, erkStrLiteral = 4, erkCFStr = 5,
    erkOther = 6, erkUnExposed = 0
proc Evaluate*(C: CXCursor): CXEvalResult =
  ##  If cursor is a statement declaration tries to evaluate the statement and if its variable, tries to evaluate its initializer, into its corresponding type.
  impl

proc getKind*(E: CXEvalResult): CXEvalResultKind =
  ##  Returns the kind of the evaluated result.
  impl

proc getAsInt*(E: CXEvalResult): int =
  ##  Returns the evaluation result as integer if the kind is Int.
  impl

proc getAsLongLong*(E: CXEvalResult): clonglong =
  ##  Returns the evaluation result as a long long integer if the kind is Int. This prevents overflows that may happen if the result is returned with clang_EvalResult_getAsInt.
  impl

proc isUnsignedInt*(E: CXEvalResult): cuint =
  ##  Returns a non-zero value if the kind is Int and the evaluation result resulted in an unsigned integer.
  impl

proc getAsUnsigned*(E: CXEvalResult): culonglong =
  ##  Returns the evaluation result as an unsigned integer if the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
  impl

proc getAsDouble*(E: CXEvalResult): cdouble =
  ##  Returns the evaluation result as double if the kind is double.
  impl

proc getAsStr*(E: CXEvalResult): cstring =
  ##  Returns the evaluation result as a constant string if the kind is other than Int or float. User must not free this pointer, instead call clang_EvalResult_dispose on the CXEvalResult returned by clang_Cursor_Evaluate.
  impl

proc dispose*(E: CXEvalResult): void =
  ##  Disposes the created Eval memory.
  impl

proc *(path: cstring): CXRemapping =
  ##  Retrieve a remapping. ** the path that contains metadata about remappings. ** the requested remapping. This remapping must be freed via a call to Error: cannot render: rnLiteralBlock Can return NULL if an error occurred.
  impl

proc *(filePaths: ptr[cstring]; numFiles: cuint): CXRemapping =
  ##  Retrieve a remapping. ** pointer to an array of file paths containing remapping info. ** number of file paths. ** the requested remapping. This remapping must be freed via a call to Error: cannot render: rnLiteralBlock Can return NULL if an error occurred.
  impl

proc remap_getNumFiles*(argCXRemapping: CXRemapping): cuint =
  ##  Determine the number of remappings.
  impl

proc remap_getFilenames*(argCXRemapping: CXRemapping; index: cuint;
                        original: ptr[CXString]; transformed: ptr[CXString]): void =
  ##  Get the original and the associated filename from the remapping. ** If non-NULL, will be set to the original filename. ** If non-NULL, will be set to the filename that the original is associated with.
  impl

proc remap_dispose*(argCXRemapping: CXRemapping): void =
  ##  Dispose the remapping.
  impl

type
  CXVisitorResult = enum
    vrBreak, vrContinue
type
  CXResult = enum
    rSuccess = 0, rInvalid = 1, rVisitBreak = 2
type
  CXResult = enum
    rSuccess = 0, rInvalid = 1, rVisitBreak = 2
proc findReferencesInFile*(cursor: CXCursor; file: CXFile;
                          visitor: CXCursorAndRangeVisitor): CXResult =
  ##  Find references of a declaration in a specific file. ** pointing to a declaration or a reference of one. ** to search for references. ** callback that will receive pairs of CXCursor/CXSourceRange for each reference found. The CXSourceRange will point inside the file; if the reference is inside a macro (and not a macro argument) the CXSourceRange will be invalid. ** one of the CXResult enumerators.
  impl

proc findIncludesInFile*(TU: CXTranslationUnit; file: CXFile;
                        visitor: CXCursorAndRangeVisitor): CXResult =
  ##  Find #import/#include directives in a specific file. ** translation unit containing the file to query. ** to search for #import/#include directives. ** callback that will receive pairs of CXCursor/CXSourceRange for each directive found. ** one of the CXResult enumerators.
  impl

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
  CXIdxEntityLanguage = enum
    ielNone = 0, ielC = 1, ielObjC = 2, ielCXX = 3, ielSwift = 4
type
  CXIdxEntityCXXTemplateKind = enum
    ietkNonTemplate = 0, ietkTemplate = 1, ietkTemplatePartialSpecialization = 2,
    ietkTemplateSpecialization = 3
type
  CXIdxEntityCXXTemplateKind = enum
    ietkNonTemplate = 0, ietkTemplate = 1, ietkTemplatePartialSpecialization = 2,
    ietkTemplateSpecialization = 3
type
  CXIdxAttrKind = enum
    iakUnexposed = 0, iakIBAction = 1, iakIBOutlet = 2, iakIBOutletCollection = 3
type
  CXIdxAttrKind = enum
    iakUnexposed = 0, iakIBAction = 1, iakIBOutlet = 2, iakIBOutletCollection = 3
type
  CXIdxDeclInfoFlags = enum
    idifFlag_Skipped = 1
type
  CXIdxDeclInfoFlags = enum
    idifFlag_Skipped = 1
type
  CXIdxObjCContainerKind = enum
    iocckForwardRef = 0, iocckInterface = 1, iocckImplementation = 2
type
  CXIdxObjCContainerKind = enum
    iocckForwardRef = 0, iocckInterface = 1, iocckImplementation = 2
type
  CXIdxEntityRefKind = enum
    ierkDirect = 1, ierkImplicit = 2
type
  CXIdxEntityRefKind = enum
    ierkDirect = 1, ierkImplicit = 2
type
  CXSymbolRole = enum
    srNone = 0, srDeclaration, srDefinition, srReference, srRead, srWrite, srCall,
    srDynamic, srAddressOf, srImplicit
type
  CXSymbolRole = enum
    srNone = 0, srDeclaration, srDefinition, srReference, srRead, srWrite, srCall,
    srDynamic, srAddressOf, srImplicit
proc index_isEntityObjCContainerKind*(argCXIdxEntityKind: CXIdxEntityKind): int =
  impl

proc index_getObjCContainerDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]): ptr[
    const CXIdxObjCContainerDeclInfo] =
  impl

proc index_getObjCInterfaceDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]): ptr[
    const CXIdxObjCInterfaceDeclInfo] =
  impl

proc index_getObjCCategoryDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]): ptr[
    const CXIdxObjCCategoryDeclInfo] =
  impl

proc index_getObjCProtocolRefListInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]): ptr[
    const CXIdxObjCProtocolRefListInfo] =
  impl

proc index_getObjCPropertyDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]): ptr[
    const CXIdxObjCPropertyDeclInfo] =
  impl

proc index_getIBOutletCollectionAttrInfo*(
    argCXIdxAttrInfo: ptr[const CXIdxAttrInfo]): ptr[
    const CXIdxIBOutletCollectionAttrInfo] =
  impl

proc index_getCXXClassDeclInfo*(argCXIdxDeclInfo: ptr[const CXIdxDeclInfo]): ptr[
    const CXIdxCXXClassDeclInfo] =
  impl

proc index_getClientContainer*(argCXIdxContainerInfo: ptr[const CXIdxContainerInfo]): CXIdxClientContainer =
  ##  For retrieving a custom CXIdxClientContainer attached to a container.
  impl

proc index_setClientContainer*(argCXIdxContainerInfo: ptr[const CXIdxContainerInfo];
                              argCXIdxClientContainer: CXIdxClientContainer): void =
  ##  For setting a custom CXIdxClientContainer attached to a container.
  impl

proc index_getClientEntity*(argCXIdxEntityInfo: ptr[const CXIdxEntityInfo]): CXIdxClientEntity =
  ##  For retrieving a custom CXIdxClientEntity attached to an entity.
  impl

proc index_setClientEntity*(argCXIdxEntityInfo: ptr[const CXIdxEntityInfo];
                           argCXIdxClientEntity: CXIdxClientEntity): void =
  ##  For setting a custom CXIdxClientEntity attached to an entity.
  impl

proc IndexAction_create*(CIdx: CXIndex): CXIndexAction =
  ##  An indexing action/session, to be applied to one or multiple translation units. ** The index object with which the index action will be associated.
  impl

proc dispose*(argCXIndexAction: CXIndexAction): void =
  ##  Destroy the given index action. The index action must not be destroyed until all of the translation units created within that index action have been destroyed.
  impl

type
  CXIndexOptFlags = enum
    iofNone = 0, iofSuppressRedundantRefs = 1, iofIndexFunctionLocalSymbols = 2,
    iofIndexImplicitTemplateInstantiations = 4, iofSuppressWarnings = 8,
    iofSkipParsedBodiesInSession = 16
type
  CXIndexOptFlags = enum
    iofNone = 0, iofSuppressRedundantRefs = 1, iofIndexFunctionLocalSymbols = 2,
    iofIndexImplicitTemplateInstantiations = 4, iofSuppressWarnings = 8,
    iofSkipParsedBodiesInSession = 16
proc indexSourceFile*(argCXIndexAction: CXIndexAction; client_data: CXClientData;
                     index_callbacks: ptr[IndexerCallbacks];
                     index_callbacks_size: cuint; index_options: cuint;
                     source_filename: cstring; command_line_args: ptr[cstring];
                     num_command_line_args: int;
                     unsaved_files: ptr[CXUnsavedFile]; num_unsaved_files: cuint;
                     out_TU: ptr[CXTranslationUnit]; TU_options: cuint): int =
  ##  Index the given source file and the translation unit corresponding to that file via callbacks implemented through #IndexerCallbacks. ** pointer data supplied by the client, which will be passed to the invoked callbacks. ** Pointer to indexing callbacks that the client implements. ** Size of #IndexerCallbacks structure that gets passed in index_callbacks. ** A bitmask of options that affects how indexing is performed. This should be a bitwise OR of the CXIndexOpt_XXX flags. ** pointer to store a Error: cannot render: rnLiteralBlock that can be reused after indexing is finished. Set to Error: cannot render: rnLiteralBlock if you do not require it. ** 0 on success or if there were errors from which the compiler could recover.  If there is a failure from which there is no recovery, returns a non-zero Error: cannot render: rnLiteralBlock The rest of the parameters are the same as #clang_parseTranslationUnit.
  impl

proc indexSourceFileFullArgv*(argCXIndexAction: CXIndexAction;
                             client_data: CXClientData;
                             index_callbacks: ptr[IndexerCallbacks];
                             index_callbacks_size: cuint; index_options: cuint;
                             source_filename: cstring;
                             command_line_args: ptr[cstring];
                             num_command_line_args: int;
                             unsaved_files: ptr[CXUnsavedFile];
                             num_unsaved_files: cuint;
                             out_TU: ptr[CXTranslationUnit]; TU_options: cuint): int =
  ##  Same as clang_indexSourceFile but requires a full command line for Error: cannot render: rnLiteralBlock including argv[0]. This is useful if the standard library paths are relative to the binary.
  impl

proc indexTranslationUnit*(argCXIndexAction: CXIndexAction;
                          client_data: CXClientData;
                          index_callbacks: ptr[IndexerCallbacks];
                          index_callbacks_size: cuint; index_options: cuint;
                          argCXTranslationUnit: CXTranslationUnit): int =
  ##  Index the given translation unit via callbacks implemented through #IndexerCallbacks. The order of callback invocations is not guaranteed to be the same as when indexing a source file. The high level order will be:   -Preprocessor callbacks invocations   -Declaration/reference callbacks invocations   -Diagnostic callback invocations The parameters are the same as #clang_indexSourceFile. ** If there is a failure from which there is no recovery, returns non-zero, otherwise returns 0.
  impl

proc indexLoc_getFileLocation*(loc: CXIdxLoc; indexFile: ptr[CXIdxClientFile];
                              file: ptr[CXFile]; line: ptr[cuint];
                              column: ptr[cuint]; offset: ptr[cuint]): void =
  ##  Retrieve the CXIdxFile, file, line, column, and offset represented by the given CXIdxLoc. If the location refers into a macro expansion, retrieves the location of the macro expansion and if it refers into a macro argument retrieves the location of the argument.
  impl

proc indexLoc_getCXSourceLocation*(loc: CXIdxLoc): CXSourceLocation =
  ##  Retrieve the CXSourceLocation represented by the given CXIdxLoc.
  impl

proc visitFields*(T: CXType; visitor: CXFieldVisitor; client_data: CXClientData): cuint =
  ##  Visit the fields of a particular type. This function visits all the direct fields of the given cursor, invoking the given Error: cannot render: rnLiteralBlock function with the cursors of each visited field. The traversal may be ended prematurely, if the visitor returns Error: cannot render: rnLiteralBlock ** the record type whose field may be visited. ** the visitor function that will be invoked for each field of Error: cannot render: rnLiteralBlock ** pointer data supplied by the client, which will be passed to the visitor each time it is invoked. ** a non-zero value if the traversal was terminated prematurely by the visitor returning Error: cannot render: rnLiteralBlock 
  impl
