import ./boost_wave/boost_wave
import hmisc/other/oswrap
import hmisc/core/all
import std/[strutils, options, tables]

type
  WaveReader* = object
    ctx*: WaveContext

  WaveCache* = ref object
    defines*: Table[AbsFile, seq[
      tuple[name: string, args, body: seq[string]]]]

proc newWaveReader*(
    file: AbsFile, 
    cache: WaveCache,
    userIncludes: seq[string] = @[],
    sysIncludes: seq[string] = @[]
  ): WaveReader =
  var resCtx: WaveContext = newWaveContext(
    readFile(file), file.string, userIncludes, sysIncludes)

  resCtx.onFoundIncludeDirective():
    let file = resCtx.findIncludeFile(unescapeInclude(impl))

    if file notin cache.defines:
      var subcontext = newWaveContext(
        readFile($file), $file, userIncludes, sysIncludes)
      subcontext.skipAll()

      for def in macroNames(subcontext):
        let mdef = subcontext.getMacroDefinition($def)
        if not mdef.isPredefined:
          echo "def> [", def, "] = <", mdef.definition, ">"

          var args, body: seq[string]

          for arg in mdef.parameters: args.add $arg
          for arg in mdef.definition: body.add $arg

          cache.defines.mgetOrPut(file, @[]).add(($def, args, body))

    for (name, args, body) in cache.defines[file]:
      resCtx.addMacroDefinition(name, args, some body.join(""))

    return EntryHandlingSkip

  result.ctx = resCtx

proc newWaveCache*(): WaveCache = new(result)

proc getExpanded*(reader: var WaveReader): string =
  for tok in items(reader.ctx):
    result.add tok.strVal()
