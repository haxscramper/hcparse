import hmisc/wrappers/wraphelp

import ./boost_wave_wrap
export boost_wave_wrap

export boost_wave_wrap
import hmisc/core/all
import hmisc/other/oswrap

import std/[os, strformat, options]

{.passc:"-I" & currentSourcePath().splitFile().dir .}


type
  WaveContext* = object
    handle*: ptr WaveContextHandle
    str*: cstringArray

    evaluatedConditional*: EvaluatedConditionalExpressionImplTypeNim
    skippedToken*: SkippedTokenImplTypeNim

    wasSysInclude*: bool
    sysIncludes*, userIncludes*: seq[string] ## Boost wave does not support
    ## 'get_system_includes', so in order to be able to produce adequate
    ## error messages we maintain copy of the include paths for the
    ## context.

  WaveError* = object of CatchableError
    diag*: WaveDiagnostics

proc first*(ctx: WaveContext): ptr WaveIteratorHandle = ctx.handle.beginIterator()
proc last*(ctx: WaveContext): ptr WaveIteratorHandle = ctx.handle.endIterator()
proc getTok*(iter: ptr WaveIteratorHandle): ptr WaveTokenHandle = iter.iterGetTok()
proc advance*(iter: ptr WaveIteratorHandle) =
  iter.advanceIterator()

proc `!=`*(iter1, iter2: ptr WaveIteratorHandle): bool = neqIterator(iter1, iter2)
proc `==`*(iter1, iter2: ptr WaveIteratorHandle): bool {.error.}
proc getValue*(tok: ptr WaveTokenHandle): cstring = tok.tokGetValue()
proc unescapeInclude*(tok: cstring): string =
  let res = $unescapeIncludeToken(tok)
  var slice = 0 .. res.high
  if res[0] in {'<', '"'}: inc slice.a
  if res[^1] in {'>', '"'}: dec slice.b
  result = res[slice]


proc strVal*(tok: ptr WaveTokenHandle): string = $tok.tokGetValue()
proc kind*(tok: ptr WaveTokenHandle): WaveTokId = tok.tokGetId()
proc hasErrors*(c: var WaveContext): bool = contextHasErrors(c.handle)
proc hasWarnings*(c: var WaveContext): bool = contextHasWarnings(c.handle)
proc popDiag*(c: var WaveContext): WaveDiagnostics = contextPopDiagnostics(c.handle)

proc `$`*(t: ptr WaveTokenHandle): string =
  if not isNil(t):
    let val = t.getValue()
    if not isNil(val):
      return $val

proc formatIncludes(ctx: WaveContext): string =
  result.add "user:"
  if ?ctx.userIncludes:
    for path in ctx.userIncludes:
      result.add "\n  - " & path

  else:
    result.add "\n  - no user include directories"

  result.add "\nsystem:"
  if ?ctx.sysIncludes:
    for path in ctx.sysIncludes:
      result.add "\n  - " & path

  else:
    result.add "\n  - no system include directories"

proc raiseErrors*(ctx: var WaveContext) =
  while ctx.hasErrors():
    var diag = ctx.popDiag()
    if diag.level in {wslError, wslFatal}:
      var extra: string

      case diag.code:
        of wekBadIncludeFile:
          extra.add ". Include file configuration was\n" & formatIncludes(ctx)

        else:
          discard

      raise (ref WaveError)(
        diag: diag,
        msg: &"Input processing failed with {diag.code}." &
          &" Error was - '{diag.errorText}' at {diag.filename}:{diag.line}:{diag.column}{extra}"
      )

iterator items*(
    ctx: var WaveContext,
    ignoreHashLine: bool = true
  ): ptr WaveTokenHandle =
  ## - @arg{ignoreHashLine} :: Explicitly omit `#line` directives if they are
  ##   emitted by the preprocessing context. This is useful when plain source
  ##   code is needed, witout correct line position information.
  var inHashLine = false
  var first: ptr WaveIteratorHandle = ctx.first()
  var last: ptr WaveIteratorHandle =  ctx.last()
  while first != ctx.last():
    /// "Items iterator over wave tokens":
      raiseErrors(ctx)
      let tok = first.getTok()
      if tok.kind == tokIdPpLine and ignoreHashLine:
        inHashLine = true

      if inHashLine:
        if tok.kind == tokIdNewline:
          inHashLine = false

      else:
        yield tok

      # echov "advancing", $first.getTok()
      first.advanceIterator()
      # echov "ending advance"

  // "Finished items iterator"

proc skipAll*(ctx: var WaveContext) =
  for item in items(ctx):
    discard


proc getExpanded*(ctx: var WaveContext, ignoreHashLine: bool = true): string =
  for tok in items(ctx, ignoreHashLine):
    result.add $tok

proc `$`*(l: ptr WaveTokenListHandle): string = $tokenListToStr(l)
proc len*(l: ptr WaveTokenListHandle): int = tokenListLen(l)

proc len*(l: ptr WaveTokenVectorHandle): int = tokenVectorLen(l)
proc `[]`*(l: ptr WaveTokenVectorHandle, idx: int): ptr WaveTokenHandle =
  tokenVectorGetAt(l, cint(idx))

iterator items*(l: ptr WaveTokenVectorHandle): ptr WaveTokenHandle =
  for i in 0 ..< len(l):
    yield l[i]

proc first*(l: ptr WaveTokenListHandle): ptr WaveTokenListIteratorHandle = tokenListBeginIterator(l)
proc last*(l: ptr WaveTokenListHandle): ptr WaveTokenListIteratorHandle = tokenListEndIterator(l)
proc `!=`*(iter1, iter2: ptr WaveTokenListIteratorHandle): bool = neqListIterator(iter1, iter2)
proc `==`*(iter1, iter2: ptr WaveTokenListIteratorHandle): bool {.error.}
proc deref*(i: ptr WaveTokenListIteratorHandle): ptr WaveTokenHandle = listIterDeref(i)
proc advance*(i: ptr WaveTokenListIteratorHandle) = listIterAdvance(i)

iterator items*(l: ptr WaveTokenListHandle): ptr WaveTokenHandle =
  var iter1 = first(l)
  var iter2 = last(l)
  while iter1 != iter2:
    yield deref(iter1)
    advance(iter1)


proc firstMacro*(l: var WaveContext): ptr WaveMacroIteratorHandle = macroBeginIterator(l.handle)
proc lastMacro*(l: var WaveContext): ptr WaveMacroIteratorHandle = macroEndIterator(l.handle)
proc `!=`*(iter1, iter2: ptr WaveMacroIteratorHandle): bool = neqMacroIterator(iter1, iter2)
proc `==`*(iter1, iter2: ptr WaveMacroIteratorHandle): bool {.error.}
proc deref*(i: ptr WaveMacroIteratorHandle): cstring = macroIteratorDeref(i)
proc advance*(i: ptr WaveMacroIteratorHandle) = macroIteratorAdvance(i)

iterator macroNames*(l: var WaveContext): cstring =
  var iter1 = firstMacro(l)
  var iter2 = lastMacro(l)
  while iter1 != iter2:
    yield deref(iter1)
    advance(iter1)
    # macroIteratorAdvance(iter1)


# REVIEW
## #+begin_group

##[

The Wave library maintains two separate search paths for include files. A
search path for user include files and a search path for system include
files. Any directories specified with the add_include_path() function
before the function set_sysinclude_delimiter() is called are searched only
for the case of #include "..." directives, they are not searched for
#include <file> directives. I.e. these directories are added to the user
include search path.

If additional directories are specified with the add_include_path()
function after a call to the function set_sysinclude_delimiter(), these
directories are searched for all #include directives. I.e. these
directories are added to the system include search path.

In addition, a call to the function set_sysinclude_delimiter() inhibits the
use of the current reference directory as the first search directory for
#include "..." directives. Therefore, the current reference directory is
searched only, if it is requested explicitly with a call to the function
add_include_path(".").

Callig both functions, the set_sysinclude_delimiter() and
add_include_path(".") allows you to control precisely, which directories
are searched before the current one and which are searched after.

These functions are modelled after the command line behaviour implemented
by the popular gcc compiler.

]##

proc addIncludePath*(ctx: var WaveContext, path: string): bool =
  ## Adds the given file system path to the user include search paths.
  ## After a call to the set_sysinclude_delimiter() this function adds the
  ## given file system path to the system include search paths. Note
  ## though, that the given path is validated against the file system.
  ##
  ## If the given path string does not form a name of a valid file system
  ## directory item, the function returns false. If the given path was
  ## successfully added to the include search paths in question, the
  ## function returns true.
  ctx.userIncludes.add path
  return ctx.handle.addIncludePath(path.cstring)

proc addSysincludePath*(ctx: var WaveContext, path: string): bool =
  ## Adds the given file system path to the system include search paths. This
  ## function operates on the system include search path regardless of the mode
  ## of operation of the `add_include_path()`. Note though, that the given path
  ## is validated against the file system.
  ##
  ## If the given path string does not form a name of a valid file system
  ## directory item, the function returns false. If the given path was
  ## successfully added to the system include search paths, the function returns
  ## true.
  ctx.sysIncludes.add path
  return ctx.handle.addSysIncludePath(path.cstring)

proc setSysincludeDelimiter*(ctx: var WaveContext) =
  ## Switches the mode, how the `add_include_path()` function operates. By
  ## default the given file system path is added to the user include search
  ## paths. After calling this function a subsequent call to the
  ## `add_include_path()` adds the given file system path to the system
  ## include search paths. Additionally it inhibits the the use of the
  ## current reference directory as the first search directory for
  ## `#include "..."` directives.

  ctx.handle.setSysincludeDelimiter()

proc setCurrentFilename*(ctx: var WaveContext, name: string) =
  ctx.handle.setCurrentFilename(name.cstring)

proc findIncludeFile*(
    ctx: WaveContext,
    file: string,
    isSystem: bool = false
  ): AbsFile =
  var sRes: cstringArray = allocCStringArray([ file ])
  var dirRes: cstring
  # assertRef(ctx)
  assertRef(ctx.handle)
  /// "Getting current filename":
    let curr = ctx.handle.getCurrentFilename()

  /// "Calling C include file implementation":
    let res = ctx.handle.findIncludeFile(
      addr sRes[0],
      addr dirRes,
      isSystem,
      nil
    )

  if res:
    result = AbsFile($sRes[0])

  else:
    raise newEnvironmentAssertionError(
      "Could not find include file '" & file &
        "'. Current file is '" & $curr & "'" &
        tern(isSystem, " (is system file)", "") & ". " &
        "Include file configuration was\n" & formatIncludes(ctx))

proc setIncludePaths*(ctx: var WaveContext, user, sys: seq[string]) =
  ## Add system and include paths for wave context.
  # - NOTE :: It adds *both* system and include paths, and toggles system
  #   include state. Use this function only for one-time configuration that
  #   will not be altered, otherwise use [[code:addIncludePath]],
  #   [[code:addSysIncludePath]] with [[code:setSysIncludeDelimiter]] called
  #   where appropriate.
  for path in user:
    assert ctx.addIncludePath(path)

  # assert not ctx.wasSysInclude
  # ctx.setSysIncludeDelimiter()

  for path in sys:
    assert ctx.addSysIncludePath(path)

## #+end_group

proc newWaveContext*(
    str: string,
    file: string = "<unknown>",
    userIncludes: seq[string] = @[],
    sysIncludes: seq[string] = @[]
  ): WaveContext =
  ## Construct new wave context object.
  ## - NOTE if @arg{userIncludes} *or* @arg{sysIncludes} is a non-empty
  ##   sequence then [[code:setIncludePath]] is called for one-time configuration,
  ##   meaning subsequent configurations are not supported.
  # new(
  #   result,
  #   # proc(ctx: WaveContext) =
  #   #   echov "Destroying context"
  #     # destroyContext(ctx.handle)
  #     # deallocCStringArray(ctx.str)
  # )

  result.str = allocCStringArray([str])
  result.handle = newWaveContext(result.str[0], file)

  discard result.addIncludePath(".")
  if ?userIncludes or ?sysIncludes:
    result.setIncludePaths(userIncludes, sysIncludes)

proc setFoundWarningDirective*(
    ctx: var WaveContext,
    impl: FoundWarningDirectiveImplTypeNim
  ) =

  let env = rawEnv(impl)
  let impl = rawProc(impl)
  ctx.handle.setFoundWarningDirective(cast[FoundWarningDirectiveImplType](impl), env)




template onFoundWarningDirective*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setFoundWarningDirective(
    proc(
      ctx {.inject.}: ptr WaveContextImplHandle,
      message {.inject.}: ptr WaveTokenListHandle
    ): EntryHandling =

      body
  )

proc setEvaluatedConditionalExpression*(
    ctx: var WaveContext,
    impl: EvaluatedConditionalExpressionImplTypeNim
  ) =

  ##[

The callback is called, whenever the preprocessor has encountered a `#if`,
`#elif`, `#ifdef` or `#ifndef` directive. This hook gets passed the
non-expanded conditional expression (as it was given in the analysed source
file) and the result of the evaluation of this expression in the current
preprocessing context.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.
- The token parameter holds a reference to the evaluated directive token.
- The parameter expression holds the non-expanded token sequence comprising the
  evaluated expression.
- The parameter expression_value contains the result of the evaluation of the
  expression in the current preprocessing context.
- The return value defines, whether the given expression has to be evaluated
  again, allowing to decide which of the conditional branches should be
  expanded. You need to return 'true' from this hook function to force the
  expression to be re-evaluated. Note, this was changed from a 'void' for the
  Boost V1.35.0 release.

  ]##

  ctx.evaluatedConditional = impl

  let env = rawEnv(impl)
  let impl = rawProc(impl)
  ctx.handle.setEvaluatedConditionalExpression(
    cast[EvaluatedConditionalExpressionImplType](impl),
    env)

template onEvaluatedConditionalExpression*(
    inCtx: var WaveContext, body: untyped): untyped =

  inCtx.setEvaluatedConditionalExpression(
    proc (
      ctx {.inject.}: ptr WaveContextImplHandle;
      directive {.inject.}: ptr WaveTokenHandle;
      expression {.inject.}: ptr WaveTokenListHandle;
      expressionValue {.inject.}: bool): bool =

      body
  )

proc setExpandingFunctionLikeMacro*(
    ctx: var WaveContext,
    impl: ExpandingFunctionLikeMacroImplTypeNim
   ) =

  ##[

The function expanding_function_like_macro is called, whenever a
function-like macro is to be expanded, i.e. before the actual expansion
starts.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The macroname parameter marks the position where the macro to expand is
  defined. It contains the token which identifies the macro name used inside
  the corresponding macro definition.

- The formal_args parameter holds the formal arguments used during the
  definition of the macro.

- The definition parameter holds the macro definition for the macro to trace.
  This is a standard STL container which holds the token sequence identified
  during the macro definition as the macro replacement list.

- The macrocall parameter marks the position where this macro is invoked. It
  contains the token, which identifies the macro call inside the preprocessed
  input stream.

- The arguments parameter holds the macro arguments used during the
  invocation of the macro. This is a vector of standard STL containers which
  contain the token sequences identified at the position of the macro call as
  the arguments to be used during the macro expansion.

- The parameters seqstart and seqend point into the input token stream
  allowing to access the whole token sequence comprising the macro invocation
  (starting with the opening parenthesis and ending after the closing one).

- If the return value is true, the macro is not expanded, i.e. the overall
  macro invocation sequence, including the parameters are copied to the
  output without further processing . If the return value is false, the macro
  is expanded as expected.

]##

  ctx.handle.setExpandingFunctionLikeMacro(
    cast[ExpandingFunctionLikeMacroImplType](rawProc(impl)),
    rawEnv(impl))

proc setFoundIncludeDirective*(
    ctx: var WaveContext,
    impl: FoundIncludeDirectiveImplTypeNim
  ) =

  ##[

The function found_include_directive is called whenever whenever a
`#include` directive was located..

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The parameter filename contains the (expanded) file name found after the
  `#include` directive. This has the format `<file>`, `"file"` or `file`.
  The formats `<file>` or `"file"` are used for #include directives found
  in the preprocessed token stream, the format `file` is used for files
  specified through the `--force_include` command line argument.

- TODO document how specify `--force_include` arguments

- The parameter include_next is set to true if the found directive was a
  `#include_next` directive and the `BOOST_WAVE_SUPPORT_INCLUDE_NEXT`
  preprocessing constant was defined to something `!= 0`.

- If the return value is 'skip', the include directive is not executed,
  i.e. the file to include is not loaded nor processed. The overall
  directive is replaced by a single newline character. If the return value
  is 'process', the directive is executed in a normal manner.

  ]##

  ctx.handle.setFoundIncludeDirective(
    cast[FoundIncludeDirectiveImplType](rawProc(impl)),
    rawEnv(impl))


template onFoundIncludeDirective*(ctx: var WaveContext, body: untyped): untyped =
  ctx.setFoundIncludeDirective(
    proc(
      context {.inject.}: ptr WaveContextImplHandle;
      impl {.inject.}: cstring;
      includeNext {.inject.}: bool): EntryHandling =

      body
  )

proc setDefinedMacro*(
    ctx: var WaveContext,
    impl: DefinedMacroImplTypeNim
  ) =

  ##[

The function defined_macro is called whenever a macro was defined
successfully.

The ctx parameter provides a reference to the context_type used during
instantiation of the preprocessing iterators by the user. Note, this
parameter was added for the Boost V1.35.0 release.

- The parameter name is a reference to the token holding the macro name.

- The parameter is_functionlike is set to true whenever the newly defined
  macro is defined as a function like macro.

- The parameter parameters holds the parameter tokens for the macro
  definition. If the macro has no parameters or if it is a object like
  macro, then this container is empty.

- The parameter definition contains the token sequence given as the
  replacement sequence (definition part) of the newly defined macro.

- The parameter is_predefined is set to true for all macros predefined
  during the initialisation pahase of the library.

  ]##

  ctx.handle.setDefinedMacro(
    cast[DefinedMacroImplType](rawProc(impl)),
    rawEnv(impl))

template onDefinedMacro*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setDefinedMacro(
    proc (
      ctx {.inject.}: ptr WaveContextImplHandle;
      name {.inject.}: ptr WaveTokenHandle;
      isFunctionlike {.inject.}: bool;
      parameters {.inject.}: ptr WaveTokenVectorHandle;
      definition {.inject.}: ptr WaveTokenListHandle;
      isPredefined {.inject.}: bool): void =

      body

    )


proc setExpandingObjectLikeMacro*(
    ctx: var WaveContext,
    impl: ExpandingObjectLikeMacroImplTypeNim
  ) =

  ##[

The function expanding_object_like_macro is called, whenever a object-like
macro is to be expanded, i.e. before the actual expansion starts.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The argmacro parameter marks the position where the macro to expand is
  defined. It contains the token which identifies the macro name used
  inside the corresponding macro definition.

- The definition parameter holds the macro definition for the macro to
  trace. This is a standard STL container which holds the token sequence
  identified during the macro definition as the macro replacement list.

- The macrocall parameter marks the position where this macro is invoked.
  It contains the token which identifies the macro call inside the
  preprocessed input stream.

- If the return value is true, the macro is not expanded, i.e. the macro
  symbol is copied to the output without further processing. If the return
  value is false, the macro is expanded as expected.

  ]##

  ctx.handle.setExpandingObjectLikeMacro(
    cast[ExpandingObjectLikeMacroImplType](rawProc(impl)),
    rawEnv(impl))

template onExpandingObjectLikeMacro*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setExpandingObjectLikeMacro(
    proc(
      ctx {.inject.}: ptr WaveContextImplHandle;
      argmacro {.inject.}: ptr WaveTokenHandle;
      definition {.inject.}: ptr WaveTokenListHandle;
      macrocall {.inject.}: ptr WaveTokenHandle): EntryHandling =

      body

  )

template onExpandingFunctionLikeMacro*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setExpandingFunctionLikeMacro(
    proc(
      ctx {.inject.}: ptr WaveContextImplHandle;
      macrodef {.inject.}: ptr WaveTokenHandle;
      formalArgs {.inject.}: ptr WaveTokenVectorHandle;
      definition {.inject.}: ptr WaveTokenListHandle;
      macrocall {.inject.}: ptr WaveTokenHandle;
      arguments {.inject.}: ptr WaveTokenVectorHandle;
      seqstart {.inject.}: pointer;
      seqend {.inject.}: pointer): bool =

      body
  )

proc setExpandedMacro*(
    ctx: var WaveContext,
    impl: proc (
      ctx: ptr WaveContextImplHandle;
      result: ptr WaveTokenListHandle): void
  ) =

  ##[

The function expanded_macro is called whenever the expansion of a macro is
finished, the replacement list is completely scanned and the identified
macros herein are replaced by its corresponding expansion results, but
before the rescanning process starts.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The parameter result contains the the result of the macro expansion so
  far. This is a standard STL container containing the generated token
  sequence.

  ]##

  ctx.handle.setExpandedMacro(
    cast[ExpandedMacroImplType](rawProc(impl)),
    rawEnv(impl))

template onExpandedMacro*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setExpandedMacro(
    proc(
      ctx {.inject.}: ptr WaveContextImplHandle;
      result {.inject.}: ptr WaveTokenListHandle): void =

      body
  )

proc setRescannedMacro*(
    ctx: var WaveContext,
    impl: proc (
      ctx: ptr WaveContextImplHandle;
      result: ptr WaveTokenListHandle): void
  ) =

  ##[

The function rescanned_macro is called whenever the rescanning of a macro
is finished, i.e. the macro expansion is complete.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The parameter result contains the the result of the whole macro
  expansion. This is a standard STL container containing the generated
  token sequence.

  ]##

  ctx.handle.setRescannedMacro(
    cast[RescannedMacroImplType](rawProc(impl)),
    rawEnv(impl))


template onRescannedMacro*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setRescannedMacro(
    proc(
      ctx {.inject.}: ptr WaveContextImplHandle;
      result {.inject.}: ptr WaveTokenListHandle): void =

      body
  )

proc setEmitLineDirective*(
    ctx: var WaveContext,
    impl: proc (
      ctx: ptr WaveContextImplHandle;
      pending: ptr WaveTokenListHandle;
      act_token: ptr WaveTokenHandle): bool
) =
  ##[

The function emit_line_directive is called whenever a #line directive has
to be emitted into the generated output.

- The parameter ctx is a reference to the context object used for
  instantiating the preprocessing iterators by the user.

- The parameter pending may be used to push tokens back into the input
  stream, which are to be used instead of the default output generated for
  the #line directive.

- The parameter act_token contains the actual #pragma token, which may be
  used for error output. The line number stored in this token can be used
  as the line number emitted as part of the #line directive.

- If the return value is false, a default #line directive is emitted by the
  library. A return value of true will inhibit any further actions, the
  tokens contained in pending will be copied verbatim to the output.

  ]##

  ctx.handle.setEmitLineDirective(
    cast[EmitLineDirectiveImplType](rawProc(impl)),
    rawEnv(impl))

template onEmitLineDirective*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setEmitLineDirective(
    proc (
      ctx {.inject.}: ptr WaveContextImplHandle;
      pending {.inject.}: ptr WaveTokenListHandle;
      actToken {.inject.}: ptr WaveTokenHandle): bool =

      body
  )

proc setSkippedToken*(
    ctx: var WaveContext,
    impl: proc (
      context: ptr WaveContextImplHandle;
      token: ptr WaveTokenHandle): void
  ) =

  ##[

The function skipped_token is called, whenever a token is about to be
skipped due to a false preprocessor condition (code fragments to be skipped
inside the not evaluated conditional #if/#else/#endif branches).

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The parameter token refers to the token to be skipped.

  ]##

  ctx.skippedToken = impl
  ctx.handle.setSkippedToken(
    cast[SkippedTokenImplType](rawProc(impl)),
    rawEnv(impl))

template onSkippedToken*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setSkippedToken(
    proc(
      context {.inject.}: ptr WaveContextImplHandle;
      token {.inject.}: ptr WaveTokenHandle) =

      body
  )



proc setLocateIncludeFIle*(
    ctx: var WaveContext,
    impl: LocateIncludeFileImplTypeNim
  ) =

  ##[



The function locate_include_file is called, whenever a `#include` directive
was encountered. It is supposed to locate the given file and should return
the full file name of the located file. This file name is expected to
uniquely identify the referenced file.

- The parameter ctx is a reference to the context object used for
  instantiating the preprocessing iterators by the user.

- The parameter file_path contains the (expanded) file name found after the
  `#include` directive. This parameter holds the string as it is specified
  in the #include directive, i.e. `<file>` or `"file"` will result in a
  parameter value 'file'.

- The parameter is_system is set to true if this call happens as a result
  of a `#include <file>` directive, it is false otherwise, i.e. for
  `#include "file"` directives.

- The parameter current_name is only used if a #include_next directive was
  encountered (and `BOOST_WAVE_SUPPORT_INCLUDE_NEXT` was defined to be
  non-zero). In this case it points to unique full name of the current
  include file (if any). Otherwise this parameter is set to NULL.

- The parameter dir_path on return is expected to hold the directory part
  of the located file.

- The parameter native_name on return is expected to hold the unique full
  file name of the located file.

- The return value defines whether the file was located successfully.

  ]##

  ctx.handle.setLocateIncludeFile(
    cast[LocateIncludeFileImplType](rawProc(impl)),
    rawEnv(impl))

template onLocateIncludeFile*(inCtx: var WaveContext, body: untyped): untyped =
  inCtx.setLocateIncludeFile(
    proc (
      ctx {.inject.}: ptr WaveContextImplHandle;
      filePath {.inject}: cstring;
      isSystem {.inject.}: bool;
      currentName {.inject.}: cstring;
      dirPath {.inject.}: cstring;
      nativename {.inject.}: cstring): EntryHandling =

      body

  )

proc allTokens*(
    ctx: var WaveContext,
    onToken: proc(skipped: bool, tok: ptr WaveTokenHandle),
    ignoreHashLine: bool = true
  ) =
  ## Iterate over all tokens, including skipped ones.
  ## - WARNING :: Due to handing of the 'on skip' tokens, it is *highly
  ##   recommended* to perform all the analysis directly inside of callback -
  ##   when it's execution is finished passed `tok` pointer is not guaranteed
  ##   to exists anymore, especially if it was `skipped`.

  // "Begin all items implementation body"

  let oldConditional = ctx.evaluatedConditional

  ctx.onEvaluatedConditionalExpression():
    onToken(true, directive)
    for tok in expression:
      onToken(true, tok)

    if not isNil(oldConditional):
      return oldConditional(ctx, directive, expression, expressionValue)

  let oldToken = ctx.skippedToken
  ctx.onSkippedToken():
    onToken(true, token)

    if not isNil(oldToken):
      oldToken(context, token)

  var first: ptr WaveIteratorHandle = ctx.first()
  var inHashLine = false

  try:
    while first != ctx.last():
      let tok = first.getTok()
      if tok.kind == tokIdPpLine and ignoreHashLine:
        inHashLine = true

      if inHashLine:
        if tok.kind == tokIdNewline:
          inHashLine = false

      else:
        onToken(false, tok)
        # yield (false, tok)

      first.advance()

  finally:
    ctx.setEvaluatedConditionalExpression(oldConditional)
    ctx.setSkippedToken(oldToken)

  // "End all items implementation body"





proc addMacroDefinition*(
    ctx: var WaveContext,
    str: string,
    isPredefined: bool = false
  ): bool =

  ##[

Adds a new macro definition to the macro symbol table. The parameter
macrostring should contain the macro to define in the command line format,
i.e. something like `MACRO(x)=definition`. The following table describes
this format in more detail. The parameter is_predefined should be true
while defining predefined macros, i.e. macros, which are not undefinable
with an #undef directive from inside the preprocessed input stream. If this
parameter is not given, it defaults to false.

**Summary of possible formats for defining macros**

====================== ==================================
macro definition       description
====================== ==================================
`MACRO`                 define `MACRO` as `1`
`MACRO=`                define `MACRO` as nothing (empty)
`MACRO=definition`      define `MACRO` as definition
`MACRO(x)`              define `MACRO(x)` as `1`
`MACRO(x)=`             define `MACRO(x)` as nothing (empty)
`MACRO(x)=definition`   define `MACRO(x)` as `definition`
====================== ===================================

The function returns false, if the macro to define already was defined and
the new definition is equivalent to the existing one, it returns true, if
the new macro was successfully added to the macro symbol table.

If the given macro definition resembles a redefinition and the new macro is
not identical to the already defined macro (in the sense defined by the C++
Standard), the function throws a corresponding `preprocess_exception` using
`throw_exception` override for the active context.

For C wrappers exception is stored in the context and can be checked for
using `hasErrors`, and accessed using `popDiagnostics`

  ]##

  return ctx.handle.addMacroDefinition(str.cstring, isPredefined)

import std/options
export options

proc addMacroDefinition*(
    ctx: var WaveContext,
    name: string,
    args: seq[string],
    definition: Option[string] = none(string),
    isPredefined: bool = false
  ) =

  ## Convenience overload for adding macro definitions. By default (when
  ## empty argument list is supplied) behavior is identical to definig
  ## `MACRO` as `1`.
  ##
  ## - @arg{definition} :: Optional definition of the macro. Macro definition
  ##   has three modes - simply `#define macro` (no arguments) implicitly
  ##   defines it as `1`. That's what `none(string)` does. Other alternatives
  ##   pass definition using `=<definition>`. If you want to define macro as
  ##   nothing (explicitly empty string), use `some("")`

  var def = name
  if 0 < args.len:
    def.add "("
    for idx, arg in args:
      if 0 < idx:
        def.add ","
      def.add arg
    def.add ")"


  if definition.isSome():
    def.add "="
    def.add definition.get()

  if not ctx.addMacroDefinition(def, isPredefined):
    raiseErrors(ctx)

# proc addMacroDefinition*(
#     ctx: var WaveContext,
#     str: string,
#     isPredefined: bool = false) =
#   addMacroDefinition(ctx, str.cstring, isPredefined)

# proc getMacroDefinition*(
#     ctx: var WaveContext,
#     name: cstring,
#     isFunctionStyle: ptr bool,
#     isPredefined: ptr bool,
#     pos: ptr WavePosition,
#     parameters: ptr ptr WaveTokenVectorHandle,
#     definition: ptr ptr WaveTokenListHandle
#   ): bool {.apiProc, importc: "wave_getMacroDefinition".}

#   ##[

# Allows to retrieve all information known with regard to a macro definition.
# parameters

# - @arg{name} :: specifies the name of the macro the information should
#   be returned for.
# - @arg{isFunctionStyle}, @arg{isPredefined} :: whether the
#   macro has been defined as a function style macro or as a
#   predefined macro resp.
# - @arg{pos} :: will contain the position the
#   macro was defined at.
# - @arg{parameters} :: will contain the names of
#   the parameters the macro was defined with and the parameter definition will
#   contain the token sequence for the definition (macro body).

# The function returns true is the macro was defined and the requested
# information has been successfully retrieved, false otherwise.

#   ]##

type
  WaveMacroDefinition* = object
    isFunctionStyle: bool
    isPredefined: bool
    pos: WavePosition
    parameters: ptr WaveTokenVectorHandle
    definition: ptr WaveTokenListHandle

# proc `=destroy`(def: var WaveMacroDefinition) =


proc getMacroDefinition*(
    ctx: var WaveContext, name: string): WaveMacroDefinition =
  let ok = getMacroDefinition(
    ctx.handle,
    name.cstring,
    addr result.isFunctionStyle,
    addr result.isPredefined,
    addr result.pos,
    addr result.parameters,
    addr result.definition
  )


# type WaveProcessingHooksHandle {.apiPtr.} = object
