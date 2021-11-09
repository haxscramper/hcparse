import ./boost_wave
import hmisc/other/oswrap
import hmisc/core/[all]
import std/[strutils, options, tables]

import ../read_libclang/hc_types

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
    conf: ParseConf,
  ): WaveReader =
  var resCtx: WaveContext = newWaveContext(
    readFile(file), file.string, conf.userIncludes, conf.sysIncludes)

  for (name, args, impl) in conf.macroDefs:
    resCtx.addMacroDefinition(name, args, impl)

  resCtx.onFoundIncludeDirective():
    try:
      let inclf = unescapeInclude(impl)
      let file = resCtx.findIncludeFile(inclf)
      if file notin cache.defines:
        cache.defines[file] = @[]
        var subcontext = newWaveContext(
          readFile($file), $file, conf.userIncludes, conf.sysIncludes)

        for (name, args, impl) in conf.macroDefs:
          subcontext.addMacroDefinition(name, args, impl)

        var
          first: ptr WaveIteratorHandle = subcontext.first()
          last: ptr WaveIteratorHandle = subcontext.last()

        subcontext.skipAll()

        for def in macroNames(subcontext):
          let mdef = subcontext.getMacroDefinition($def)
          if not mdef.isPredefined:
            var args, body: seq[string]

            for arg in mdef.parameters: args.add $arg
            for arg in mdef.definition: body.add $arg

            cache.defines[file].add(($def, args, body))

      for (name, args, body) in cache.defines[file]:
        # FIXME keep track of all defined macros to avoid illegal macro
        # redefinition excepsions that might potentially be reaised
        # here.?
        # if "HOSTED" in name:
        #   echov name, $args, $body

        resCtx.addMacroDefinition(name, args, some body.join(""))


    except:
      raise

    return EntryHandlingSkip

  result.ctx = resCtx

proc newWaveCache*(): WaveCache = new(result)

proc getExpanded*(reader: var WaveReader): string =
  for tok in items(reader.ctx):
    result.add tok.strVal()
