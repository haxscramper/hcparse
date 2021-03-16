import std/[unittest, decls, options]
import hmisc/other/[oswrap]
import hmisc/helpers
import hcparse



suite "Show function body":
  test "Simple main":

    let outfile = AbsFile("/tmp/example.cpp")
    outfile.writeFile """
    typedef enum E {a} E1, *EPtr;
    typedef enum {b} E2;
    enum E3 {c};
    // struct E2 {};
    // typedef struct S {enum {q} field;} SName, *SPtr;
    """

    let
      unit = parseFile(outfile, baseCParseConfig, opts = {})
      topCursor = unit.getTranslationUnitCursor()

    echo unit.isNil
    echo topCursor.cxKind

    var functionNames: seq[string]
    topCursor.visitMainFile do: # Visit all ast elements from main translation unit
      makeVisitor [unit, functionNames]: # Create visitor callback. All
        # captured variables have to be explicitly declared in capture
        # list. Two variables are implicitly injected into callback scope
        # - `cursor` (current AST element being visited) and `parent` -
        # parent node for cursor.
        echo cursor.treeRepr(unit)
        if cursor.cxKind == ckFunctionDecl:
          functionNames.add $cursor

        return cvrContinue

    echo "found functions: ", functionNames

suite "Get all tokens":
  test "Main":
    let str = """
#ifndef __HEADER_FOO
#define __HEADER_FOO

//+reflect
class Foo
{
    Foo *parent;  /* Parent AST node. */
    Foo *child;   /* First child AST node. */
    Foo *last;    /* Last child AST node. */
    Foo *next;    /* Sibling AST node. */
    Foo *prev;    /* Prior sibling AST node. */

    private:
        int m_int; //+reflect
        int fld1; //! qt convention
        int fld2; /// doxygen regular
        int fld3; ///< doxygen arrow
        int fld4; /*! hello */
        int fld5; /** hello */

      /**
       *  @brief Copy constructor with allocator argument.
       * @param  __uset  Input %unordered_set to copy.
       * @param  __a  An allocator object.
       */
      Foo() { }

      /// Move constructor.
      Foo(Foo&&) = default;

};

#endif
"""


    let outfile = getTempFile(AbsDir("/tmp"), "XXXXXXX.cpp")
    outfile.writeFile str

    let
      unit = parseFile(outfile, opts = {})
      topCursor = unit.getTranslationUnitCursor()

    echo topCursor.treeRepr(unit)

    # for tok in topCursor.tokenStrings(unit):
    #   echo $tok

    echo fromTokens(topCursor.tokens(unit), unit)

    topCursor.visitMainFile do:
      makeVisitor []:
        echo "(", cursor, "): ", cursor.rawComment()
        return cvrRecurse
