import
  std/[sugar, strutils, sequtils, strformat, options],
  hmisc/other/[oswrap, hshell, hlogger],
  hmisc/types/[colortext],
  hmisc/core/all,
  hcparse,
  hcparse/[dox_xml]

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
    int withArgs(int arg1, int arg2) { return arg1 * arg2 * 3; }

    const char* getNames() { return "test"; }
    char* const getNames1();
};

"""

doxygenXmlForDir(dir, doxDir)
let wrapConf = baseCppWrapConf.withDeepIt do:
  it.baseDir = dir
  it.logger = newTermLogger()
  it.showParsed = true
  # it.refidMap = getRefidLocations(doxDir)
  it.codegenDir = some dir

let resFile = dir /. "res.nim"
wrapWithConf(file, resFile, wrapConf, baseCppParseConf)

resFile.appendFile """
{.experimental: "implicitDeref".}

var decl = newDeclareClass()
echo decl.publicField
echo decl.secondPublic() * decl.withArgs(1, 2)
"""

execShell shellCmd(nim, cpp, -r, $resFile)
