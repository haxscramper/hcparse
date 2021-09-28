import ./boost_wave
import hmisc/core/all
import std/[strformat, sequtils, strutils]
import hmisc/types/colorstring
import hmisc/algo/[clformat, hstring_algo]

echo cwaveDl

proc main() =
  var ctx = newWaveContext(allocCStringArray([
    lit3"""
      #define concat(a, b) a ## b
      #warning "123"
      #if 1
      call1();
      #else
      call2();
      #endif
      #include <file>

      #define ABC (X+Y)
      #define X 100
      #define Y 50
      #define MUL(x,y) ((x)*(y))
      ABC
      MUL(MUL(1,2),3)

    """])[0], "<Unknown>".cstring)

  var indent = 16
  ctx.setFoundWarningDirective(
    proc(
      ctx: ptr WaveContextImplHandle,
      message: ptr WaveTokenListHandle
    ): EntryHandling =
      echo toYellow("warning" |<< indent), wrap($message, CharBrace.doubleCurly)

      return EntryHandlingSkip
  )

  ctx.setEvaluatedConditionalExpression(
    proc (
      ctx: ptr WaveContextImplHandle;
      directive: ptr WaveTokenHandle;
      expression: ptr WaveTokenListHandle;
      expression_value: bool): bool =

      echo hshow(directive.kind) |<< indent, toCyan($directive & " " & $expression),
        " = ", expressionValue
  )

  ctx.setFoundIncludeDirective(
    proc(
      context: ptr WaveContextImplHandle;
      impl: cstring;
      include_next: bool): EntryHandling =

      echo toBlue("#include" |<< indent), $impl
      return EntryHandlingSkip
  )

  ctx.setSkippedToken(
    proc(
      context: ptr WaveContextImplHandle;
      token: ptr WaveTokenHandle) =

      stdout.write hshow(token.kind) |<< indent, " skip "
      if token.kind in {tokIdNewline}:
        echo wrap("\\n", CharBrace.doubleSquare)

      else:
        echo wrap($token, CharBrace.doubleSquare)

  )

  ctx.setDefinedMacro(
    proc (
      ctx: ptr WaveContextImplHandle;
      name: ptr WaveTokenHandle;
      is_functionlike: bool;
      parameters: ptr WaveTokenVectorHandle;
      definition: ptr WaveTokenListHandle;
      is_predefined: bool): void =

      echo toGreen("#define" |<< indent), name
      for item in items(parameters):
        echo " " |<< indent, hshow(item.kind) |<< 16, wrap($item, CharBrace.doubleAngle)

      echo toGreen("as") |>> indent
      for item in items(definition):
        echo " " |<< indent, hshow(item.kind) |<< 16, wrap($item, CharBrace.doubleAngle)
  )

  ctx.setExpandingFunctionLikeMacro(
    proc(
      ctx: ptr WaveContextImplHandle;
      macrodef: ptr WaveTokenHandle;
      formal_args: ptr WaveTokenVectorHandle;
      definition: ptr WaveTokenListHandle;
      macrocall: ptr WaveTokenHandle;
      arguments: ptr WaveTokenVectorHandle;
      seqstart: pointer;
      seqend: pointer): bool =

      echo " " |<< indent, "expanding function-like macro", $macrodef
      inc indent, 2
  )

  ctx.setExpandingObjectLikeMacro(
    proc(
      ctx: ptr WaveContextImplHandle;
      argmacro: ptr WaveTokenHandle;
      definition: ptr WaveTokenListHandle;
      macrocall: ptr WaveTokenHandle): EntryHandling =

      echo " " |<< indent, "expanding object-like macro ", $argmacro
      inc indent, 2

  )

  ctx.setExpandedMacro(
    proc(
      ctx: ptr WaveContextImplHandle;
      result: ptr WaveTokenListHandle): void =

      echo " " |<< indent,
        "expanded macro to",
        clformat.joinc(mapIt(result, $it), " ")

      dec indent, 2
  )

  ctx.setRescannedMacro(
    proc(
      ctx: ptr WaveContextImplHandle;
      result: ptr WaveTokenListHandle): void =

      echo " " |<< indent,
        "rescanned macro to",
        clformat.joinc(mapIt(result, $it), " ")

      dec indent, 2
  )

  ctx.setEmitLineDirective(
    proc (
      ctx: ptr WaveContextImplHandle;
      pending: ptr WaveTokenListHandle;
      act_token: ptr WaveTokenHandle): bool =

      return true
  )

  var first: ptr WaveIteratorHandle = ctx.first()
  while first != ctx.last():
    let tok = first.getTok()
    echo hshow(tok.kind()) |<< indent, " real ", hshow(tok.getValue())
    first.advance()

when isMainModule:
  main()
