#define STRINGIFY(x) #x
#define XSTRINGIFY(x) STRINGIFY(x)

// adapted from https://github.com/katahiromz/BoostWaveExample
// to build run `clang++ -lboost_system -lboost_filesystem -lboost_thread
// -lboost_wave -o../bin/deps deps.cpp`

#include <boost/property_tree/info_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/wave.hpp>
#include <boost/wave/cpplexer/cpp_lex_iterator.hpp>
#include <boost/wave/cpplexer/cpp_lex_token.hpp>
#include <boost/wave/preprocessing_hooks.hpp>

#include <boost/property_tree/ptree.hpp>
#include <boost/ref.hpp>
#include <stack>

#include <fstream>
#include <iostream>
#include <string>
// #include <vector>

#include "predefined.hpp"

class BasicInputPolicy
{
  public:
    template <typename IterContextT>
    class inner
    {
      public:
        static std::string readFile(const char* filePath) {
            // Open file
            std::ifstream fs(filePath);
            if (!fs) {
                std::string msg = "Cannot open file '";
                msg += (filePath == nullptr) ? "(nullptr)" : filePath;
                msg += "'.";
                throw std::runtime_error(msg.c_str());
            }

            // Read
            fs.unsetf(std::ios::skipws);
            std::string text(
                std::istreambuf_iterator<char>(fs.rdbuf()),
                std::istreambuf_iterator<char>());

            return text;
        }

        template <typename PositionT>
        static void init_iterators(
            IterContextT&                 iterContext,
            const PositionT&              pos,
            boost::wave::language_support language) {
            try {
                iterContext.code = readFile(iterContext.filename.c_str());
                iterContext.code += "\n";
            } catch (const std::exception&) {
                BOOST_WAVE_THROW_CTX(
                    iterContext.ctx,
                    boost::wave::preprocess_exception,
                    bad_include_file,
                    iterContext.filename.c_str(),
                    pos);
                return;
            }

            typedef typename IterContextT::iterator_type iterator_type;
            iterContext.first = iterator_type(
                iterContext.code.begin(),
                iterContext.code.end(),
                PositionT(iterContext.filename),
                language);
            iterContext.last = iterator_type();
        }

      protected:
        std::string code;
    };
};

inline void show_help(void) {
    std::cout
        << "Usage: cpp [options] input-file\n"
           "Generate tree of dependencies for file <input-file>\n\n"
           "WARNING: no assumptions about include paths is made - "
           "you need to manually include locations for the system include "
           "headers and your project's include paths. "
           "\n\n"
           "** System variable & default paths\n\n"
           "`$CPATH` is added to sytem paths and either of "
           "`$C_INCLUDE_PATH` or `$CPLUS_INCLUDE_PATH` "
           "(mutally exclusive!) variables. `/usr/include` is "
           "added to system include path if none of the above variables "
           "are defined\n"
           "Basically you have two ways for using this - either set none "
           "of the variables and supply all options through the "
           "command-line flags (+ /usr/include added by default), or set "
           "`$CPATH` for common include directories (both C++ and C) and "
           "one of the `C/CPLUS` variables. C one is queried first"
           "\n\n"
           "** Options:\n\n"
           "  -Dmacro        Defines a macro\n"
           "  -Dmacro=def    Defines a macro\n"
           "  -Umacro        Undefines a macro\n"
           "  -Ipath         Adds include path\n"
           "  -Spath         Adds system include path\n\n"
           "** Output format\n\n"
           "Output consists of nested opening/closing parenthesis {} with "
           "list of file. Each opening paren corresponds to opening "
           "#include directive, with all elements in the next layer being "
           "retrived from the #include'd file. Output example:\n\n"
           // clang-format off
"  <iostream> /usr/include/c++/10.2.0/iostream\n"
"  {\n"
"    <bits/c++config.h> /usr/include/c++/10.2.0/x86_64-pc-linux-gnu/bits/c++config.h\n"
"      {\n"
"        <bits/os_defines.h> /usr/include/c++/10.2.0/x86_64-pc-linux-gnu/bits/os_defines.h\n"
"          {\n"
        // clang-format on
        << std::endl;
}
std::vector<std::string> split(const std::string& s, char delim) {
    std::vector<std::string> elems;
    std::stringstream        ss(s);
    std::string              item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }

    return elems;
}


template <typename T_CONTEXT>
inline bool setup_context(T_CONTEXT& ctx, int argc, char** argv) {
    using namespace boost;

    // Language options
    ctx.set_language(wave::language_support(
        wave::support_cpp | wave::support_option_long_long
        | wave::support_option_variadics));

    add_predefined_macros(ctx);

    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-' && argv[i][1]) {
            std::string str = &(argv[i][2]);
            switch (argv[i][1]) {
                case 'D': {
                    ctx.add_macro_definition(str);
                    break;
                }
                case 'U': {
                    ctx.remove_macro_definition(str);
                    break;
                }
                case 'I': {
                    std::cerr << "-I [ " << str << " ]\n";
                    ctx.add_include_path(str.c_str());
                    break;
                }
                case 'S': {
                    std::cerr << "-S [ " << str << " ]\n";
                    ctx.add_sysinclude_path(str.c_str());
                    break;
                }
                default:
                    fprintf(
                        stderr, "ERROR: invalid argument '%s'\n", argv[i]);
                    return false;
            }
        }
    }

    const char* path1 = getenv("CPATH");
    const char* path2 = getenv("C_INCLUDE_PATH");
    const char* path3 = getenv("CPLUS_INCLUDE_PATH");
    if (path1) {
        for (auto& path : split(std::string(path1), ':')) {
            std::cerr << "-S [ " << path << " ]\n";
            ctx.add_sysinclude_path(path.c_str());
        }
    }
    if (path2) {
        for (auto& path : split(std::string(path2), ':')) {

            std::cerr << "-S [ " << path << " ]\n";
            ctx.add_sysinclude_path(path.c_str());
        }
    } else if (path3) {
        for (auto& path : split(std::string(path3), ':')) {

            std::cerr << "-S [ " << path << " ]\n";
            ctx.add_sysinclude_path(path.c_str());
        }
    }

    if (!path1 && !path2 && !path3) {

        std::cerr << "-S [ /usr/include ]\n";
        ctx.add_sysinclude_path("/usr/include");
    }

    return true;
}


// A hook class to output the tree
class MakeIncludeTreeHook
    : public boost::wave::context_policies::default_preprocessing_hooks
{
  private:
    typedef boost::reference_wrapper<boost::property_tree::ptree>
        ref_ptree;

  public:
    MakeIncludeTreeHook(boost::property_tree::ptree& target)
        : _target(target)
        , _current(target)
        , _parentStack()
        , _lastIncFile() {
    }

  public:
    const boost::property_tree::ptree& getTarget() const {
        return _target;
    }

  public:
    template <typename ContextT>
    bool found_include_directive(
        ContextT const&    ctx,
        std::string const& filename,
        bool               include_next) {
        _lastIncFile = filename;

        return false;
    }

    template <typename ContextT>
    void opened_include_file(
        ContextT const&,
        std::string const&,
        std::string const& absname,
        bool) {
        // std::cout << "open " << absname << "\n";
        using namespace boost::property_tree;

        _parentStack.push(_current);

        ptree::iterator itr = _current.get().push_back(ptree::value_type(
            _lastIncFile, boost::property_tree::ptree(absname)));
        _current            = boost::ref((*itr).second);
    }

    template <typename ContextT>
    void returning_from_include_file(ContextT const&) {
        // std::cout << "return\n";
        _current = _parentStack.top();

        _parentStack.pop();
    }

  private:
    ref_ptree             _target;
    ref_ptree             _current;
    std::stack<ref_ptree> _parentStack;
    std::string           _lastIncFile;
};

int main(int argc, char** argv) {
    using namespace std;
    namespace wave = boost::wave;
    using namespace boost::property_tree;

    if (argc < 2 || strcmp(argv[1], "--help") == 0) {
        show_help();
        return 1;
    }

    // Load source
    std::string code;
    {
        std::ifstream fs(argv[argc - 1]);
        fs.unsetf(std::ios::skipws);
        code.assign(
            std::istreambuf_iterator<char>(fs.rdbuf()),
            std::istreambuf_iterator<char>());
    }

    // Tree
    ptree incTree;

    // Prepare context
    typedef wave::context<
        std::string::const_iterator,
        wave::cpplexer::lex_iterator<wave::cpplexer::lex_token<>>,
        BasicInputPolicy,
        MakeIncludeTreeHook> // Original hook
            Context;
    Context ctx(
        code.begin(),
        code.end(),
        argv[argc - 1],
        MakeIncludeTreeHook(incTree)); // Pass the tree

    if (!setup_context(ctx, argc, argv)) {
        cerr << "Context setup failed";
        return 2;
    }

    Context::iterator_type itrEnd = ctx.end();
    Context::iterator_type itr    = ctx.begin();

    cerr << "Starting preprocessor\n";
    try {
        auto tmp = (itr != itrEnd);
    } catch (const boost::wave::preprocess_exception& ex) {
        cerr << "ERROR in " << ex.file_name() << " : " << ex.line_no()
             << endl;
        cerr << ex.description() << endl;
        return 1;
    }


    while (itr != itrEnd) {
        try {
            ++itr;
        } catch (const wave::cpp_exception& ex) {
            if (!ex.is_recoverable()) {
                cerr << "ERROR in " << ex.file_name() << " : "
                     << ex.line_no() << endl;
                cerr << "  -> " << ex.description() << endl;

                cerr << "Severe error; stopping processing\n";
                return 1;
            } else {
                // fucking "mIsSiNg iNcLuDe fIlE iS rEcOvErAbLe ErRoR"
                if (ex.get_errorcode()
                    == boost::wave::macro_handling_exception::
                        bad_include_file) {
                    cerr << ex.description() << endl;
                    return 1;
                }
            }
        } catch (const wave::preprocess_exception& ex) {
            cerr << "Preprocessor exception";
            cerr << "ERROR in " << ex.file_name() << " : " << ex.line_no()
                 << endl;
            cerr << ex.description() << "\n";
            // cerr << ex.what() << "\n";
            return 1;
        } catch (...) {
            cerr << "Exception ocurred during preprocessing";
            return 1;
        }
    }

    info_parser::write_info(cout, incTree);
    return 0;
}
