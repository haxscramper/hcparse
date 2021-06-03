import
  std/[sugar, strutils, sequtils, strformat, options],
  hmisc/other/[oswrap, hshell, colorlogger],
  hmisc/types/[colortext],
  hmisc/helpers,
  hcparse,
  htsparse/cpp/cpp

startColorLogger()
startHax()

let file = "/tmp/a.c"
let str = """

class DeclareClass {
  public:
    int publicField;
    DeclareClass* publicMethod();
};

"""

file.writeFile(str)

let wrapConf = baseCppWrapConf.withDeepIt do:
  it.baseDir = AbsDir("/tmp")
  it.showParsed = true

let resFile = "/tmp/res.nim"
wrapWithConf(AbsFile file, AbsFile resFile, wrapConf, baseCppParseConf)

execShell shellCmd(nim, cpp, $resFile)
