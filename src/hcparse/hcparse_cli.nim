import hmisc/other/[hcligen, oswrap, colorlogger]
import hmisc/types/colortext
import hnimast
import libclang_utils


proc wrapCpp*(
  file, outfile: FsFile,
  includePaths: seq[FsDir] = @[],
  errorReparseVerbose: bool = false) =
  startColorLogger()
  let wrapped = wrapSingleFile(file, includePaths, errorReparseVerbose)

  outFile.writeFile($wrapped)
  # debug colorizeToStr($wrapped, "cpp")

when isMainModule:
  dispatchMulti(
    [wrapCpp]
  )
