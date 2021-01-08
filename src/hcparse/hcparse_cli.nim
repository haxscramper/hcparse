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


  let wconf = baseWrapConfig.withIt do:
    discard

  let (wrapped, codegen) = wrapSingleFile(
    file,
    errorReparseVerbose,
    wrapConf = wconf,
    parseConf = pconf
  )

  var filenames: HashSet[string]
  withStreamFile(outFile):
    for gen in codegen:
      if gen.filename.hasExt("cpp") and $gen.filename notin filenames:
        let res = gen.filename.withBasePrefix("gen_")
        file.writeLine(&"{{.compile: \"{res}\".}}")
        filenames.incl $gen.filename

    for gen in compile:
      file.writeLine(&"{{.compile: \"{gen}\".}}")


    for entry in wrapped:
      # stdout.write(entry)
      file.write(entry)

  var resFiles: Table[string, File]

  if codegens.isSome():
    for gen in codegen:
      let target = codegens.get() / gen.filename
      # info "Writing generated code into", target
      # debug gen.code
      if $target notin resFiles:
        let res = target.withBasePrefix("gen_")
        resFiles[$target] = open($res, fmWrite)
        resFiles[$target].write(gen.header)

      resFiles[$target].write(gen.code)

  for _, file in pairs(resFiles):
    file.close()
      # writeFile(target, gen.code)

when isMainModule:
  dispatchMulti(
    [wrapCpp]
  )
