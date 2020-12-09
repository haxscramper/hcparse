import hmisc/other/[hcligen, oswrap, colorlogger]
import hmisc/types/colortext
import hnimast
import libclang_utils


proc wrapCpp*(
  file, outfile: FsFile,
  includePaths: seq[FsDir] = @[],
  errorReparseVerbose: bool = false) =
  startColorLogger()
  var
    wrapConf = baseWrapConfig
    parseConf = baseCppParseConfig
    cache: WrapCache
    index: FileIndex

  wrapConf.isInternal = proc(
      dep: AbsFile,
      conf: WrapConfig,
      index: FileIndex
    ): bool {.closure} =
      true

  for path in includePaths:
    parseConf.globalFlags.add("-I" & path.getStr().strip())

  let parsed = parseFile(
    file.toAbsFile(), parseConf, wrapConf,
    reparseOnNil = errorReparseVerbose
  )

  let wrapped = parsed.wrapFile(wrapConf, cache, index)
  outFile.writeFile($wrapped)

  debug colorizeToStr($wrapped, "cpp")

when isMainModule:
  dispatchMulti(
    [wrapCpp]
  )
