import hmisc/preludes/unittest
import
  hmisc/other/[
    hpprint,
    hlogger
  ],

  hcparse/[
    hc_parsefront,
    hc_impls
  ],

  hcparse/read_libclang/[
      hc_types,
      hc_visitors,
      hc_clangreader,
  ],

  hcparse/processor/[hc_postprocess]

proc convStr(file: AbsFile, s: string, print: bool = false): string =
  file.writeFile(s)
  var cache = newWrapCache()
  if print:
    let unit = parseFileViaClang(file)
    echo unit.getTranslationUnitCursor().treeRepr()

  var wrap = baseCppWrapConf.withDeepIt do:
    it.logger = newTermLogger()
    it.onIgnoreCursor():
      return false


  let fix = baseFixConf.withIt do:
    it.typeStore = newTypeStore()
    it.onGetBind():
      return cxxHeader("?")


  let api = parseFileViaClang(file).
    splitDeclarations(wrap, cache)

  let wrapped = api.wrapApiUnit(wrap, cache).
    postFixEntries(fix, cxxLibImport("h", @["h"]))

  return wrapped.toString(cxxCodegenConf)


suite "Parse basic file":
  let dir = getTestTempDir()
  mkDir dir
  let file = dir /. "file.hpp"
  test "Dump tree repr":
    writeFile(file, """
struct Struct {
  int field;

  int get() const;

  public __attribute__((annotate("qt_signal"))):

    void set1(int arg);
    void set2(int arg);

};

template <typename T, class Z = int>
class Templated {
  public:
    T get() const noexcept;
    void set(Z value);
    static void getQ();
};

// enum class Enum {en1, en2};
// Enum getEnum(Enum in);

Templated<int> operator%(int arg1, Struct arg2);

int main() {}

typedef struct LIBSSH2_USERAUTH_KBDINT_PROMPT
{
    char *text;
    unsigned int length;
    unsigned char echo;
} LIBSSH2_USERAUTH_KBDINT_PROMPT;

typedef struct Q_LIBSSH2_POLLFD {
    unsigned char type;
    union {
        int socket_1;
        int socket_2;
    } fd;
    unsigned long events;
} LIBSSH2_POLLFD;

typedef unsigned long long libssh2_uint64_t;
typedef long long libssh2_int64_t;
typedef struct stat libssh2_struct_stat;
typedef int libssh2_struct_stat_size;

""")

    let unit = parseFileViaClang(file)
    echo unit.getTranslationUnitCursor().treeRepr()

  test "Visitors":
    var cache = newWrapCache()
    let api = parseFileViaClang(file).
      splitDeclarations(baseCppWrapConf, cache)

    var fix = baseFixConf.withIt do:
      it.typeStore = newTypeStore()

    fix.onGetBind():
      return cxxHeader("?")

    var wrap = baseCppWrapConf.withDeepIt do:
      it.logger = newTermLogger()

    let wrapped = api.wrapApiUnit(wrap, cache).
      postFixEntries(fix, cxxLibImport("h", @["h"]))

    echo "Generated wrapped"
    echo wrapped.toString(cxxCodegenConf)

suite "Convert edge cases":
  test "Rename mapped to the same nim ident":
    let file = getTestTempFile("c")
    let s = file.convStr("""
typedef struct _LIBSSH2_USERAUTH_KBDINT_PROMPT
{
    char *text;
    unsigned int length;
    unsigned char echo;
} LIBSSH2_USERAUTH_KBDINT_PROMPT;

typedef struct _LIBSSH2_USERAUTH_KBDINT_RESPONSE
{
    char *text;
    unsigned int length;
} LIBSSH2_USERAUTH_KBDINT_RESPONSE;

/* 'keyboard-interactive' authentication callback */
#define LIBSSH2_USERAUTH_KBDINT_RESPONSE_FUNC(name_) \
 void name_(const char *name, int name_len, const char *instruction, \
            int instruction_len, int num_prompts, \
            const LIBSSH2_USERAUTH_KBDINT_PROMPT *prompts,              \
            LIBSSH2_USERAUTH_KBDINT_RESPONSE *responses, void **abstract)

int libssh2_userauth_keyboard_interactive_ex(
  const char *username,
  unsigned int username_len,
  LIBSSH2_USERAUTH_KBDINT_RESPONSE_FUNC((*response_callback)));

""", true)

    echo s
