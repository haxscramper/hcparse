import hmisc/other/[hcligen, oswrap, colorlogger]
import hmisc/algo/halgorithm
import hmisc/types/colortext
import hmisc/hdebug_misc
import hnimast, hnimast/pprint
import std/[strformat, sets, tables]
import libclang


proc wrapCpp*(
    file, outfile: FsFile,
    codegens: Option[FsDir] = none(FsDir),
    includePaths: seq[FsDir] = @[],
    errorReparseVerbose: bool = false,
    isImportcpp: bool = true,
    globalFlags: seq[string] = @[],
    compile: seq[FsFile] = @[]
  ) =

  let pconf = baseCppParseConfig.withIt do:
    if not isImportcpp:
      it.globalFlags = @[]

    for fl in includePaths:
      it.globalFlags.add "-I" & fl.getStr()

    for fl in globalFlags:
      it.globalFlags.add fl


  let wconf = baseWrapConf.withIt do:
    discard

  writeWrapped(
    wrapSingleFile(
      file,
      errorReparseVerbose,
      wrapConf = wconf,
      parseConf = pconf
    ),
    outFile,
    codegens,
    compile,
    wconf
  )


when isMainModule:
  dispatchMulti(
    [wrapCpp]
  )
