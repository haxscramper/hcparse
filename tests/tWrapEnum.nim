import
  std/[sugar, strutils, sequtils, strformat, options],
  hmisc/other/[oswrap, hshell, colorlogger],
  hmisc/types/[colortext],
  hmisc/helpers,
  hcparse,
  hcparse/[dox_xml, hc_docwrap]

startColorLogger(showFile = true)
startHax()

let
  dir = getNewTempDir("tWrapEnum")
  doxDir = dir / "dox"

let file = dir /. "a.c"
file.writeFile """

#define CL class

class
   DeclareClass {
  public:
    int publicField; float otherField;
    DeclareClass* publicMethod();
    int secondPublic() { return 12; }
};

"""

doxygenXmlForDir(dir, doxDir)
let wrapConf = baseCppWrapConf.withDeepIt do:
  it.baseDir = dir
  it.showParsed = true
  it.refidMap = getRefidLocations(doxDir)

let resFile = dir /. "res.nim"
wrapWithConf(file, resFile, wrapConf, baseCppParseConf)

resFile.appendFile """
{.experimental: "implicitDeref".}

var decl = newDeclareClass()
echo decl.publicField
echo decl.secondPublic()
"""

execShell shellCmd(nim, cpp, -r, $resFile)
