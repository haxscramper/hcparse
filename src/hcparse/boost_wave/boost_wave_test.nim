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
      echo toYellow("warning" |<< 16), wrap($message, CharBrace.doubleCurly)

      return EntryHandlingSkip
  )

  ctx.setEvaluatedConditionalExpression(
    proc (
      ctx: ptr WaveContextImplHandle;
      directive: ptr WaveTokenHandle;
      expression: ptr WaveTokenListHandle;
      expression_value: bool): bool =

      echo hshow(directive.kind) |<< 16, toCyan($directive & " " & $expression),
        " = ", expressionValue
  )

  ctx.setFoundIncludeDirective(
    proc(
      context: ptr WaveContextImplHandle;
      impl: cstring;
      include_next: bool): EntryHandling =

      echo toBlue("#include" |<< 16), $impl
      return EntryHandlingSkip
  )

  ctx.setSkippedToken(
    proc(
      context: ptr WaveContextImplHandle;
      token: ptr WaveTokenHandle) =

      stdout.write hshow(token.kind) |<< 16, " skip "
      if token.kind in {tokIdNewline}:
        echo wrap("\\n", CharBrace.doubleSquare)

      else:
        echo wrap($token, CharBrace.doubleSquare)

  )

  var first: ptr WaveIteratorHandle = ctx.first()
  while first != ctx.last():
    let tok = first.getTok()
    echo hshow(tok.kind()) |<< 16, " real ", hshow(tok.getValue())
    first.advance()

when isMainModule:
  main()
