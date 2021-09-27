import ./boost_wave
import hmisc/core/all
import std/strformat
import hmisc/types/colorstring
import hmisc/algo/[clformat, hstring_algo]

echo cwaveDl

proc main() =
  var ctx = newWaveContext(allocCStringArray([
    lit3"""
      #warning "123"
      #if 1
      call1();
      #else
      call2();
      #endif
      #include <file>

    """])[0], "<Unknown>".cstring)

  ctx.setFoundWarningDirective(
    proc(
      ctx: ptr WaveContextImplHandle,
      message: ptr WaveTokenListHandle
    ): EntryHandling =
      echo "Found warning directive with ",
        len(message), " elements, value = ",
        $message

      return EntryHandlingSkip
  )

  var needComment = false
  ctx.setEvaluatedConditionalExpression(
    proc (
      ctx: ptr WaveContextImplHandle;
      directive: ptr WaveTokenHandle;
      expression: ptr WaveTokenListHandle;
      expression_value: bool): bool =

      echo "evaluated conditional expression ", $directive,
        $expression, " to ", expressionValue

      needComment = true
  )

  ctx.setFoundIncludeDirective(
    proc(
      context: ptr WaveContextImplHandle;
      impl: cstring;
      include_next: bool): EntryHandling =

      echo "found include directive for ", $impl
      return EntryHandlingSkip
  )

  ctx.setSkippedToken(
    proc(
      context: ptr WaveContextImplHandle;
      token: ptr WaveTokenHandle) =

      if needComment and token.kind != tokId_SPACE:
        stdout.write "// "
        needComment = false

      if token.kind in {tokIdNewline}:
        stdout.write wrap("\\n", CharBrace.doubleSquare), "\n"

      else:
        stdout.write wrap($token, CharBrace.doubleSquare)

      if token.kind in {tokIdNewline, tokIdCppComment}:
        needComment = true
  )

  var first: ptr WaveIteratorHandle = ctx.first()
  while first != ctx.last():
    let tok = first.getTok()
    echo hshow(tok.kind()) |<< 16, hshow(tok.getValue())
    first.advance()

when isMainModule:
  main()
