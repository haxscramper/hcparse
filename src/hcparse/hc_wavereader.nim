import ./boost_wave/boost_wave
import hmisc/other/oswrap
import hmisc/core/all

type
  WaveReader* = object
    ctx*: WaveContext

proc newWaveReader*(file: AbsFile, withHook: bool = false): WaveReader =
  var resCtx: WaveContext = newWaveContext(readFile(file), file.string)
  resCtx.onFoundIncludeDirective():
    let file = resCtx.findIncludeFile(unescapeInclude(impl)).get()

    var subcontext = newWaveContext(readFile($file), $file)
    subcontext.skipAll()

    for def in macroNames(subcontext):
      let mdef = subcontext.getMacroDefinition($def)
      if not mdef.isPredefined:
        echo "def> [", def, "] = <", mdef.definition, ">"

    return EntryHandlingSkip

  result.ctx = resCtx


proc getExpanded*(reader: var WaveReader): string =
  for tok in items(reader.ctx):
    result.add tok.strVal()
