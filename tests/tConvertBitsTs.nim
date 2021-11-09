import
  hmisc/preludes/unittest,
  hcparse/[hc_parsefront, hc_codegen],
  hcparse/interop_ir/wrap_store,
  compiler/[ast, renderer],
  hnimast/[nim_decl, object_decl, obj_field_macros, hast_common, proc_decl]

import std/[strutils]

import pkg/[jsony, frosty], pkg/frosty/streams

func getFields[N](o: ObjectDecl[N]): seq[ObjectField[N]] = getFlatFields(o)

var fixConf = baseFixConf.withIt do:
  it.onFixName():
    result = cache.fixContextedName(name)

  it.onGetBind():
    return cxxHeader("?")

  it.typeStore = newTypeStore()

proc lib(path: varargs[string]): CxxLibImport =
  cxxLibImport("test", @path)


proc convStr(str: string, conf: CodegenConf = cxxCodegenConf): string =
  let lib = lib(@["z"])
  wrapViaTs(str, fixConf, lib).
    postFixEntries(fixConf, lib).
    toString(conf)

proc convEntries(str: string, conf: CodegenConf = cxxCodegenConf): seq[CxxEntry] =
  let lib = lib(@["z"])
  wrapViaTs(str, fixConf, lib).postFixEntries(fixConf, lib)

proc convDecls(
    str: string, conf: CodegenConf = cxxCodegenConf): seq[NimDecl[PNode]] =
  let lib = lib(@["z"])
  hc_codegen.toNNode[PNode](
    wrapViaTs(str, fixConf, lib).
    postFixEntries(fixConf, lib), conf)

proc convPPrint(str: string) =
  let lib = lib(@["z"])
  pprint wrapViaTs(str, fixConf, lib).
    postFixEntries(fixConf, lib)

suite "Convert type declarations":
  test "Regular struct":
    check convDecls("struct S {};")[0].getObject().getName() == "S"

  test "Struct with fields":
    let
      f1 = convDecls(
        "struct WithFields { int field; };")[0].getObject().getFields()[0]
      f2 = convDecls(
        "struct WithFields { int __field; };")[0].getObject().getFields()[0]

    check:
      not f1.hasPragma("importcpp")
      f1.name == "field"

      f2.hasPragma("importcpp")
      f2.name == "field"
      f2.getPragmaArgs("importcpp")[0].getStrVal() == "__field"

  test "Class with methods":
    let
      decls = convDecls("struct A { void get(); void set(int val); };")
      declA = decls[0].getObject()
      declGet = decls[1].getProc()
      declSet = decls[2].getProc()

    check:
      declA.getName() == "A"
      declGet.getName() == "get"
      declGet.argumentNames() == @["this"]
      declSet.argumentNames() == @["this", "val"]


  test "Class with documentation":
    let
      decls = convDecls("struct A { int field; /* doc comment */ };")
      declA = decls[0].getObject()

    check:
      declA.getName() == "A"
      declA.getFields()[0].getName() == "field"
      "doc comment" in declA.getFields()[0].docComment

suite "Repeated names":
  test "Multiple structs":
    let decls = convDecls("struct _S {}; struct S{};")[0].getTypes()
    check:
      decls[0].getObject().getName() == "S"
      decls[1].getObject().getName() == "S1"

suite "Procedures":
  test "Variadic proc":
    let pr = convDecls("void git_libgit2_opts(int option, ...);").
      getFirst(nekProcDecl).getProc()

    check:
      pr.hasPragma("varargs")

# suite "Enum":
#   test "enum":
#     echov convStr("""
# enum fullpow { q = 0, a = 1 << 0, b = 1 << 1 };
# """)

suite "Qt elements":
  test "":
    echo convStr("""

class QObject {
    public:
        static QMetaObject::Connection connect(
            const QMetaMethod &method,
            Qt::ConnectionType type = Qt::AutoConnection);
};

class QPaintDevice {
    public: int width() const { return metric(PdmWidth); }
};

class QWidget : public QObject, public QPaintDevice {
    public: QRect frameGeometry() const;
};

class QAbstractButton : public QWidget {
    public: explicit QAbstractButton(QWidget *parent = nullptr);
};""")


suite "Serialization":
  test "Functions":
    let entries = convEntries("int getIdx();")
    let test = freeze entries
    let unpacked = thaw[seq[CxxEntry]](test)

    echo unpacked.toString(cxxCodegenConf)
