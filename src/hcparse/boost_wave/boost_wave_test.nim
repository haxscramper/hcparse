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
  ctx.onFoundWarningDirective():
    echo toYellow("warning" |<< indent), wrap($message, CharBrace.doubleCurly)
    return EntryHandlingSkip

  ctx.onEvaluatedConditionalExpression():
    echo hshow(directive.kind) |<< indent, toCyan($directive & " " & $expression),
      " = ", expressionValue

  ctx.onFoundIncludeDirective():
    echo toBlue("#include" |<< indent), $impl
    return EntryHandlingSkip

  ctx.onSkippedToken():
    stdout.write hshow(token.kind) |<< indent, " skip "
    if token.kind in {tokIdNewline}:
      echo wrap("\\n", CharBrace.doubleSquare)

    else:
      echo wrap($token, CharBrace.doubleSquare)

  ctx.onDefinedMacro():
    echo toGreen("#define" |<< indent), name
    for item in items(parameters):
      echo " " |<< indent, hshow(item.kind) |<< 16, wrap($item, CharBrace.doubleAngle)

    echo toGreen("as") |>> indent
    for item in items(definition):
      echo " " |<< indent, hshow(item.kind) |<< 16, wrap($item, CharBrace.doubleAngle)

  ctx.onExpandingFunctionLikeMacro():
    echo " " |<< indent, "expanding function-like macro", $macrodef
    inc indent, 2

  ctx.onExpandingObjectLikeMacro():
    echo " " |<< indent, "expanding object-like macro ", $argmacro
    inc indent, 2

  ctx.onExpandedMacro():
    echo " " |<< indent,
      "expanded macro to",
      clformat.joinc(mapIt(result, $it), " ")

    dec indent, 2

  ctx.onRescannedMacro():
    echo " " |<< indent,
      "rescanned macro to",
      clformat.joinc(mapIt(result, $it), " ")

    dec indent, 2

  ctx.onEmitLineDirective():
    return true

  var first: ptr WaveIteratorHandle = ctx.first()
  while first != ctx.last():
    let tok = first.getTok()
    echo hshow(tok.kind()) |<< indent, " real ", hshow(tok.getValue())
    first.advance()

when isMainModule:
  main()
