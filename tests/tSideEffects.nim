import std/[unittest, decls]
import hmisc/other/[oswrap]
import hcparse

suite "Show function body":
  test "Simple main":

    let outfile = AbsFile("/tmp/example.cpp")
    outfile.writeFile """
    #include <iostream>

    /// Documentation comments for class
    /// Multiline comment
    class MyClass
    {
    public:
      int field; ///< Field documentation
      virtual void method() const = 0;
      static const int static_field;
      static int static_method();
    };

    /** This is a main function documenation comment */
    int main() {
      1 + 2;
      // std::cout << "Hello world\n";
    }
    """


    let
      trIndex = createIndex()
      unit = parseTranslationUnit(trIndex, outfile)
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
          echo cursor.rawComment()
          functionNames.add $cursor

        return cvrContinue

    echo "found functions: ", functionNames
