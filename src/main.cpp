#include <llvm/Support/CommandLine.h>

#include <clang/Frontend/FrontendActions.h>

#include <clang/Tooling/JSONCompilationDatabase.h>
#include <clang/Tooling/Tooling.h>

#include <clang/Lex/PPCallbacks.h>
#include <clang/Lex/MacroInfo.h>

#include <clang/AST/ASTContext.h>
#include <clang/AST/RecursiveASTVisitor.h>

#include <clang/Frontend/CompilerInstance.h>
#include <llvm/Support/Path.h>
#include <llvm/ADT/StringSwitch.h>

#include <iostream>
#include <fstream>
#include <limits>
#include <stdexcept>
#include <ctime>

#include "embedded_includes.h"
#include "stringbuilder.hpp"
#include "projectmanager.hpp"
// #include "compat.hpp"


namespace cl = llvm::cl;

cl::opt<std::string> BuildPath(
    "b",
    cl::value_desc("compile_commands.json"),
    cl::desc("Path to the compilation database (compile_commands.json) If "
             "this argument is not passed, the compilation arguments can "
             "be passed on the command line after '--'"),
    cl::Optional);

cl::list<std::string> SourcePaths(
    cl::Positional,
    cl::desc("<sources>* [-- <compile command>]"),
    cl::ZeroOrMore);

cl::opt<std::string> OutputPath(
    "o",
    cl::value_desc("output path"),
    cl::desc("Output directory where the generated files will be put"),
    cl::Required);

cl::list<std::string> ProjectPaths(
    "p",
    cl::value_desc("<project>:<path>[:<revision>]"),
    cl::desc(
        "Project specification: The name of the project, the absolute "
        "path of the source code, and the revision separated by colons. "
        "Example: -p projectname:/path/to/source/code:0.3beta"),
    cl::ZeroOrMore);


cl::list<std::string> ExternalProjectPaths(
    "e",
    cl::value_desc("<project>:<path>:<url>"),
    cl::desc("Reference to an external project. Example: -e "
             "clang/include/clang:/opt/llvm/include/clang/:https://"
             "code.woboq.org/llvm"),
    cl::ZeroOrMore);

cl::opt<bool> ProcessAllSources(
    "a",
    cl::desc("Process all files from the compile_commands.json. If this "
             "argument is passed, the list of sources does not need to be "
             "passed"));

cl::extrahelp extra(

    R"(

EXAMPLES:

Simple generation without compile command or project (compile command specified inline)
  hcparse -o db.sqlite $PWD -- -std=c++14 -I/opt/llvm/include

With a project
  hcparse -o db.sqlite -b $PWD/compile_commands.js -a -p hcparse:$PWD
)");

std::string locationToString(
    clang::SourceLocation loc,
    clang::SourceManager& sm) {
    clang::PresumedLoc fixed = sm.getPresumedLoc(loc);
    if (!fixed.isValid()) return "???";
    return (llvm::Twine(fixed.getFilename()) + ":" +
            llvm::Twine(fixed.getLine()))
        .str();
}

enum class DatabaseType {
    InDatabase,
    NotInDatabase,
    ProcessFullDirectory
};

struct Annotator {
    clang::SourceManager* sm;
    ProjectManager*       pm;

    clang::SourceManager& getSourceManager() { return *sm; }
    void setSourceManager(clang::SourceManager& in) { sm = &in; }
    bool shouldProcess(clang::FileID FID) {
        // TODO cache known declarations
        return true;
    }
};

struct DiagnosticClient : clang::DiagnosticConsumer {
    Annotator& annotator;
    DiagnosticClient(Annotator& fm) : annotator(fm) {}

    virtual void HandleDiagnostic(
        clang::DiagnosticsEngine::Level DiagLevel,
        const clang::Diagnostic&        Info) override {
        std::string             clas;
        llvm::SmallString<1000> diag;
        Info.FormatDiagnostic(diag);

        switch (DiagLevel) {
            case clang::DiagnosticsEngine::Fatal: {
                std::cerr << "FATAL ";
                [[fallthrough]];
            }
            case clang::DiagnosticsEngine::Error: {
                std::cerr << "Error: "
                          << locationToString(
                                 Info.getLocation(),
                                 annotator.getSourceManager())
                          << ": " << diag.c_str() << std::endl;
                clas = "error";
                break;
            }
            case clang::DiagnosticsEngine::Warning: {
                clas = "warning";
                break;
            }
            default: return;
        }
    }
};

struct ASTVisitor : clang::RecursiveASTVisitor<ASTVisitor> {};


class PreprocessorCallback : public clang::PPCallbacks {
    Annotator&           annotator;
    clang::Preprocessor& PP;
    bool                 disabled = false; // To prevent recurstion
    bool seenPragma = false; // To detect _Pragma in expansion
    bool recoverIncludePath; // If we should try to find the include paths
                             // harder

  public:
    PreprocessorCallback(
        Annotator&           fm,
        clang::Preprocessor& PP,
        bool                 recoverIncludePath)
        : annotator(fm), PP(PP), recoverIncludePath(recoverIncludePath) {}
};


class ASTConsumer : public clang::ASTConsumer {
    clang::CompilerInstance& ci;
    Annotator                annotator;
    DatabaseType             WasInDatabase;

  public:
    ASTConsumer(
        clang::CompilerInstance& ci,
        ProjectManager&          projectManager,
        DatabaseType             WasInDatabase)
        : clang::ASTConsumer(), ci(ci), WasInDatabase(WasInDatabase) {
        // ci.getLangOpts().DelayedTemplateParsing = (true);
        ci.getPreprocessor().enableIncrementalProcessing();
    }

    virtual ~ASTConsumer() {
        ci.getDiagnostics().setClient(
            new clang::IgnoringDiagConsumer, true);
    }

    virtual void Initialize(clang::ASTContext& Ctx) override {
        annotator.setSourceManager(Ctx.getSourceManager());
        ci.getPreprocessor().addPPCallbacks(
            std::unique_ptr<clang::PPCallbacks>(new PreprocessorCallback(
                annotator,
                ci.getPreprocessor(),
                WasInDatabase == DatabaseType::ProcessFullDirectory)));
        ci.getDiagnostics().setClient(
            new DiagnosticClient(annotator), true);
        ci.getDiagnostics().setErrorLimit(0);
    }

    virtual bool HandleTopLevelDecl(clang::DeclGroupRef D) override {
        if (ci.getDiagnostics().hasFatalErrorOccurred()) {
            // Reset errors: (Hack to ignore the fatal errors.)
            ci.getDiagnostics().Reset();
            // When there was fatal error, processing the warnings may
            // cause crashes
            ci.getDiagnostics().setIgnoreAllWarnings(true);
        }
        return true;
    }

    virtual void HandleTranslationUnit(clang::ASTContext& Ctx) override {
        ci.getPreprocessor().getDiagnostics().getClient();

        ASTVisitor v{};
        v.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

    virtual bool shouldSkipFunctionBody(clang::Decl* D) override {
        return !annotator.shouldProcess(
            clang::FullSourceLoc(
                D->getLocation(), annotator.getSourceManager())
                .getExpansionLoc()
                .getFileID());
    }
};

class Action : public clang::ASTFrontendAction {
    static std::set<std::string> processed;
    DatabaseType                 WasInDatabase;

  protected:
    virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
        clang::CompilerInstance& CI,
        llvm::StringRef          InFile) override {
        if (processed.count(InFile.str())) {
            std::cerr << "Skipping already processed " << InFile.str()
                      << std::endl;
            return nullptr;
        }

        processed.insert(InFile.str());
        CI.getFrontendOpts().SkipFunctionBodies = true;
        return std::unique_ptr<clang::ASTConsumer>(
            new ASTConsumer(CI, *projectManager, WasInDatabase));
    }

  public:
    Action(DatabaseType WasInDatabase = DatabaseType::InDatabase)
        : WasInDatabase(WasInDatabase) {}
    virtual bool hasCodeCompletionSupport() const override { return true; }
    static ProjectManager* projectManager;
};


std::set<std::string> Action::processed;
ProjectManager*       Action::projectManager = nullptr;

static bool proceedCommand(
    std::vector<std::string> command,
    llvm::StringRef          Directory,
    llvm::StringRef          file,
    clang::FileManager*      FM,
    DatabaseType             WasInDatabase) {
    // This code change all the paths to be absolute paths
    //  FIXME:  it is a bit fragile.
    bool previousIsDashI    = false;
    bool previousNeedsMacro = false;
    bool hasNoStdInc        = false;
    for (std::string& A : command) {
        if (previousIsDashI && !A.empty() && A[0] != '/') {
            A               = Directory % "/" % A;
            previousIsDashI = false;
            continue;
        } else if (A == "-I") {
            previousIsDashI = true;
            continue;
        } else if (A == "-nostdinc" || A == "-nostdinc++") {
            hasNoStdInc = true;
            continue;
        } else if (A == "-U" || A == "-D") {
            previousNeedsMacro = true;
            continue;
        }
        if (previousNeedsMacro) {
            previousNeedsMacro = false;
            continue;
        }
        previousIsDashI = false;
        if (A.empty()) { continue; }
        if (llvm::StringRef(A).startswith("-I") && A[2] != '/') {
            A = "-I" % Directory % "/" % llvm::StringRef(A).substr(2);
            continue;
        }
        if (A[0] == '-' || A[0] == '/') { continue; }
        std::string PossiblePath = Directory % "/" % A;
        if (llvm::sys::fs::exists(PossiblePath)) { A = PossiblePath; }
    }

    command = clang::tooling::getClangSyntaxOnlyAdjuster()(command, file);
    command = clang::tooling::getClangStripOutputAdjuster()(command, file);

    if (!hasNoStdInc) {
        command.push_back("-isystem");
        command.push_back("/builtins");
    }

    command.push_back("-Qunused-arguments");
    command.push_back("-Wno-unknown-warning-option");
    clang::tooling::ToolInvocation Inv(
        command, std::unique_ptr<Action>(new Action(WasInDatabase)), FM);

    bool result = Inv.run();
    if (!result) {
        std::cerr << "Error: The file was not recognized as source code: "
                  << file.str() << std::endl;
    }
    return result;
}

int main(int argc, const char** argv) {
    std::string                                          ErrorMessage;
    std::unique_ptr<clang::tooling::CompilationDatabase> Compilations(
        clang::tooling::FixedCompilationDatabase::loadFromCommandLine(
            argc, argv, ErrorMessage));
    if (!ErrorMessage.empty()) {
        std::cerr << ErrorMessage << std::endl;
        ErrorMessage = {};
    }

    llvm::cl::ParseCommandLineOptions(argc, argv);

    ProjectManager projectManager{};
    for (std::string& s : ProjectPaths) {
        auto colonPos = s.find(':');
        if (colonPos >= s.size()) {
            std::cerr << "fail to parse project option : " << s
                      << std::endl;
            continue;
        }
        auto        secondColonPos = s.find(':', colonPos + 1);
        ProjectInfo info{
            s.substr(0, colonPos),
            s.substr(colonPos + 1, secondColonPos - colonPos - 1),
            secondColonPos < s.size() ? s.substr(secondColonPos + 1)
                                      : std::string()};
        if (!projectManager.addProject(std::move(info))) {
            std::cerr << "invalid project directory for : " << s
                      << std::endl;
        }
    }
    for (std::string& s : ExternalProjectPaths) {
        auto colonPos = s.find(':');
        if (colonPos >= s.size()) {
            std::cerr << "fail to parse project option : " << s
                      << std::endl;
            continue;
        }
        auto secondColonPos = s.find(':', colonPos + 1);
        if (secondColonPos >= s.size()) {
            std::cerr << "fail to parse project option : " << s
                      << std::endl;
            continue;
        }
        ProjectInfo info{
            s.substr(0, colonPos),
            s.substr(colonPos + 1, secondColonPos - colonPos - 1),
            ProjectInfo::External};
        info.external_root_url = s.substr(secondColonPos + 1);
        if (!projectManager.addProject(std::move(info))) {
            std::cerr << "invalid project directory for : " << s
                      << std::endl;
        }
    }

    Action::projectManager = &projectManager;


    if (!Compilations && llvm::sys::fs::exists(BuildPath)) {
        if (llvm::sys::fs::is_directory(BuildPath)) {
            Compilations = std::unique_ptr<
                clang::tooling::CompilationDatabase>(
                clang::tooling::CompilationDatabase::loadFromDirectory(
                    BuildPath, ErrorMessage));
        } else {
            Compilations = std::unique_ptr<
                clang::tooling::CompilationDatabase>(
                clang::tooling::JSONCompilationDatabase::loadFromFile(
                    BuildPath,
                    ErrorMessage,
                    clang::tooling::JSONCommandLineSyntax::AutoDetect));
        }
        if (!Compilations && !ErrorMessage.empty()) {
            std::cerr << ErrorMessage << std::endl;
        }
    }

    if (!Compilations) {
        std::cerr << "Could not load compilationdatabase. "
                     "Please use the -b option to a path containing a "
                     "compile_commands.json, or use "
                     "'--' followed by the compilation commands."
                  << std::endl;
        return EXIT_FAILURE;
    }

    bool                     IsProcessingAllDirectory = false;
    std::vector<std::string> DirContents;
    std::vector<std::string> AllFiles = Compilations->getAllFiles();
    std::sort(AllFiles.begin(), AllFiles.end());
    llvm::ArrayRef<std::string> Sources = SourcePaths;
    if (Sources.empty() && ProcessAllSources) {
        // Because else the order is too random
        Sources = AllFiles;
    } else if (ProcessAllSources) {
        std::cerr << "Cannot use both sources and  '-a'" << std::endl;
        return EXIT_FAILURE;
    } else if (
        Sources.size() == 1 &&
        llvm::sys::fs::is_directory(Sources.front())) {
        // A directory was passed, process all the files in that directory
        llvm::SmallString<128> DirName;
        llvm::sys::path::native(Sources.front(), DirName);
        while (DirName.endswith("/"))
            DirName.pop_back();
        std::error_code EC;
        for (llvm::sys::fs::recursive_directory_iterator
                 it(DirName.str(), EC),
             DirEnd;
             it != DirEnd && !EC;
             it.increment(EC)) {
            if (llvm::sys::path::filename(it->path()).startswith(".")) {
                it.no_push();
                continue;
            }
            DirContents.push_back(it->path());
        }
        Sources                  = DirContents;
        IsProcessingAllDirectory = true;
        if (EC) {
            std::cerr << "Error reading the directory: " << EC.message()
                      << std::endl;
            return EXIT_FAILURE;
        }

        if (ProjectPaths.empty()) {
            ProjectInfo info{
                std::string(llvm::sys::path::filename(DirName)),
                std::string(DirName.str())};
            projectManager.addProject(std::move(info));
        }
    }

    if (Sources.empty()) {
        std::cerr << "No source files.  Please pass source files as "
                     "argument, or use '-a'"
                  << std::endl;
        return EXIT_FAILURE;
    }
    if (ProjectPaths.empty() && !IsProcessingAllDirectory) {
        std::cerr << "You must specify a project name and directory with "
                     "'-p name:directory'"
                  << std::endl;
        return EXIT_FAILURE;
    }

    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> VFS(
        new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem()));
    clang::FileManager FM({"."}, VFS);
    FM.Retain();

    // Map virtual files
    {
        auto InMemoryFS = llvm::IntrusiveRefCntPtr<
            llvm::vfs::InMemoryFileSystem>(
            new llvm::vfs::InMemoryFileSystem);
        VFS->pushOverlay(InMemoryFS);
        // Map the builtins includes
        const EmbeddedFile* f = EmbeddedFiles;
        while (f->filename) {
            InMemoryFS->addFile(
                f->filename,
                0,
                llvm::MemoryBuffer::getMemBufferCopy(f->content));
            f++;
        }
    }

    int Progress = 0;

    std::vector<std::string> NotInDB;

    for (const auto& it : Sources) {
        std::string file = clang::tooling::getAbsolutePath(it);
        Progress++;

        if (it.empty() || it == "-") { continue; }

        llvm::SmallString<256> filename;
        llvm::sys::fs::real_path(file, filename);
        auto project = projectManager.projectForFile(filename);
        if (!project) {
            std::cerr
                << "Sources: Skipping file not included by any project "
                << filename.c_str() << std::endl;
            continue;
        }

        bool isHeader = llvm::StringSwitch<bool>(
                            llvm::sys::path::extension(filename))
                            .Cases(".h", ".H", ".hh", ".hpp", ".hxx", true)
                            .Default(false);

        auto compileCommandsForFile = Compilations->getCompileCommands(
            file);
        if (!compileCommandsForFile.empty() && !isHeader) {
            std::cerr << '[' << (100 * Progress / Sources.size())
                      << "%] Processing " << file << "\n";
            proceedCommand(
                compileCommandsForFile.front().CommandLine,
                compileCommandsForFile.front().Directory,
                file,
                &FM,
                IsProcessingAllDirectory
                    ? DatabaseType::ProcessFullDirectory
                    : DatabaseType::InDatabase);
        } else {
            // TODO: Try to find a command line for a file in the same path
            std::cerr << "Delayed " << file << "\n";
            Progress--;
            NotInDB.push_back(std::string(filename.str()));
            continue;
        }
    }

    for (const auto& it : NotInDB) {
        std::string file = clang::tooling::getAbsolutePath(it);
        Progress++;
        auto project = projectManager.projectForFile(file);
        if (!project) {
            std::cerr
                << "NotInDB: Skipping file not included by any project "
                << file.c_str() << std::endl;
            continue;
        }

        llvm::StringRef similar;

        auto compileCommandsForFile = Compilations->getCompileCommands(
            file);
        std::string fileForCommands = file;
        if (compileCommandsForFile.empty()) {
            // Find the element with the bigger prefix
            auto lower = std::lower_bound(
                AllFiles.cbegin(), AllFiles.cend(), file);
            if (lower == AllFiles.cend()) { lower = AllFiles.cbegin(); }
            compileCommandsForFile = Compilations->getCompileCommands(
                *lower);
            fileForCommands = *lower;
        }

        bool success = false;
        if (!compileCommandsForFile.empty()) {
            std::cerr << '[' << (100 * Progress / Sources.size())
                      << "%] Processing " << file << "\n";
            auto command = compileCommandsForFile.front().CommandLine;
            std::replace(
                command.begin(), command.end(), fileForCommands, it);
            if (llvm::StringRef(file).endswith(".qdoc")) {
                command.insert(command.begin() + 1, "-xc++");
                // include the header for this .qdoc file
                command.push_back("-include");
                command.push_back(
                    llvm::StringRef(file).substr(0, file.size() - 5) %
                    ".h");
            }
            success = proceedCommand(
                std::move(command),
                compileCommandsForFile.front().Directory,
                file,
                &FM,
                IsProcessingAllDirectory
                    ? DatabaseType::ProcessFullDirectory
                    : DatabaseType::NotInDatabase);
        } else {
            std::cerr << "Could not find commands for " << file << "\n";
        }
    }
}
