import
  hnimast/interop/wrap_macros,
  hmisc/core/all,
  hmisc/other/oswrap

static:
  startHaxComp()

{.passc: "-fPIC".}
{.passc: "-I/usr/include/qt".}
{.passc: "-I/usr/include/qt/QtGui".}
{.passc: "-I/usr/include/qt/QtWidgets".}

{.passl: "-lQt5Gui".}
{.passl: "-lQt5Core".}
{.passl: "-lQt5Widgets".}

wrapheader "<QWidget>":
  class QWidget

wrapheader "<QTextEdit>":
  class QTextEdit of QWidget:
    proc newQTextEdit(parent: ptr QWidget): ptr QTextEdit {.constructor.}

wrapheader "<QMainWindow>":
  class QMainWindow of QWidget:
    proc newQMainWindow(): ptr QMainWindow {.constructor.}
    proc show()
    proc setFixedSize(w: int, h: int)
    proc setCentralWidget(widget: ptr QWidget)

wrapheader "<QApplication>":
  class QApplication:
    proc newQApplication(argc: cint, argv: cstringArray):
      ptr QApplication {.constructor.}

    proc exec()

static: startHaxComp()

cgen "${cacheDir}/${file}":
  include "<QTextEdit>"

  class DerivedEditor of QTextEdit:
    proc textChangedSlot() {.slot.} =
      echo "Signal derived changed"

    proc new(parent: ptr QWidget): ptr DerivedEditor
      {.constructor(QTextEdit(parent)).}

template connect*[A, B](
    a: ptr A, signal: untyped{nkIdent},
    b: ptr B, slot: untyped{nkIdent}
  ): untyped =

  const
    signalPtr = "&" & $typeof(a[]) & "::" & astToStr(signal)
    slotPtr = "&" & $typeof(b[]) & "::" & astToStr(slot)

  block:
    let
      inA {.inject.} = a
      inB {.inject.} = b

    // "Emit QObject connect start"
    {.emit: "QObject::connect(`inA`, " & signalPtr & ", `inB`, " & slotPtr & ");".}
    // "End connect"



import hmisc/shellexec/xephyr

proc main() =
  withXephyr():
    var
      argc: cint = 0
      argv: cstringarray

    // "QApplication"
    var app: ptr QApplication = newQApplication(argc, argv)

    // "QMainWindow"
    var window: ptr QMainWindow = newQMainWindow()

    // "Derived editor"
    var edit: ptr DerivedEditor = newDerivedEditor(nil)

    window[].setFixedSize(320, 320)
    window[].show()
    window[].setCentralWidget(edit)

    connect(edit, textChanged, edit, textChangedSlot)
    app[].exec()

if not defined(haxTestall):
  main()
