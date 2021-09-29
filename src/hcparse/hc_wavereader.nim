import ./boost_wave/boost_wave
import hmisc/other/oswrap
import hmisc/core/all

type
  WaveReader* = object
    ctx*: WaveContext

proc newWaveReader*(file: AbsFile): WaveReader =
  var resCtx: WaveContext = newWaveContext(readFile(file), file.string)

  resCtx.onLocateIncludeFile():
    echov "Dropping to subcontext"
    let file = resCtx.findIncludeFIle($filePath)

    var subcontext = newWaveContext(readFile($filePath), $filePath)


    for tok in items(subcontext):
      discard

    echo "asdf"
    for def in macroNames(subcontext):
      echo def

    return EntryHandlingSkip

  result.ctx = resCtx


proc getExpanded*(reader: var WaveReader): string =
  for tok in items(reader.ctx):
    result.add tok.strVal()
