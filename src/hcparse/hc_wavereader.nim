import ./boost_wave/boost_wave
import hmisc/other/oswrap
import hmisc/core/[all, code_errors]
import std/[strutils, options, tables]

export boost_wave

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
    sysIncludes: seq[string] = @[],
    subTargets: seq[string] = @[]
  ): WaveReader =
  var resCtx: WaveContext = newWaveContext(
    readFile(file), file.string, userIncludes, sysIncludes)

  resCtx.onFoundIncludeDirective():
    try:
      let inclf = unescapeInclude(impl)
      if inclf in subTargets or subTargets.len == 0:
        let file = resCtx.findIncludeFile(inclf)
        if file notin cache.defines:
          cache.defines[file] = @[]
          var subcontext = newWaveContext(
            readFile($file), $file, userIncludes, sysIncludes)
          var first: ptr WaveIteratorHandle = subcontext.first()
          var last: ptr WaveIteratorHandle = subcontext.last()
          subcontext.skipAll()

          for def in macroNames(subcontext):
            let mdef = subcontext.getMacroDefinition($def)
            if not mdef.isPredefined:
              var args, body: seq[string]

              for arg in mdef.parameters: args.add $arg
              for arg in mdef.definition: body.add $arg

              cache.defines[file].add(($def, args, body))

        for (name, args, body) in cache.defines[file]:
          if isNil(resCtx):
            echov "FUCKING PIECE OF SHIT"
            quit(1)

          assertRef resCtx
          resCtx.addMacroDefinition(name, args, some body.join(""))

    except:
      echov "Died here"
      echov getCurrentExceptionMsg()
      raise

    return EntryHandlingSkip

  result.ctx = resCtx

proc newWaveCache*(): WaveCache = new(result)

proc getExpanded*(reader: var WaveReader): string =
  for tok in items(reader.ctx):
    result.add tok.strVal()
