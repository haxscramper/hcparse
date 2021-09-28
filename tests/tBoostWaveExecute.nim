import hcparse/boost_wave/boost_wave
import hmisc/core/all
import std/[strformat, sequtils, strutils]
import hmisc/types/colorstring
import hmisc/algo/[clformat, hstring_algo]
import hmisc/preludes/unittest

suite "Test expansion hooks":
  test "Allhooks":
    var ctx = newWaveContext(
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

      """)

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

    for tok in items(ctx, false):
      echo hshow(tok.kind()) |<< indent, " real ", hshow(tok.getValue())

suite "Iterate over tokens":
  var ctx = newWaveContext("""
#if 1
test1
#else
test2
#endif
""")

  var toks: seq[(WaveTokId, string)]
  ctx.allTokens() do (skipped: bool, tok: ptr WaveTokenHandle):
    toks.add((tok.kind, $tok))

  pprint toks

suite "Get expanded text":
  test "Simple token":
    var ctx = newWaveContext("test()")
    check ctx.getExpanded() == "test()"

  test "Get expanded with macros":
    var ctx = newWaveContext("""
#if defined(_MSC_VER)
# define GIT_CALLBACK(name) (__cdecl *name)
#else
# define GIT_CALLBACK(name) (*name)
#endif
void GIT_CALLBACK(free)(git_writestream *stream);
""")

    check ctx.getExpanded().strip() == "void (*free)(git_writestream *stream);"

  test "Get expanded with explicitly defined macros":
    var ctx = newWaveContext("GIT_CALLBACK(free)(git_writestream *stream);")
    ctx.addMacroDefinition("GIT_CALLBACK(name)=(*name)")
    check ctx.getExpanded() == "(*free)(git_writestream *stream);"

  test "Get expanded with explicitly defined macros via overload":
    var ctx = newWaveContext("GIT_CALLBACK(free)(git_writestream *stream);")
    ctx.addMacroDefinition("GIT_CALLBACK", @["name"], some "(*name)")
    check ctx.getExpanded() == "(*free)(git_writestream *stream);"
