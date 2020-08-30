template <typename Context>
inline void add_predefined_macros(Context& ctx) {
#ifdef DOS
    ctx.add_macro_definition("DOS=" XSTRINGIFY(DOS), true);
#endif
#ifdef DOS16
    ctx.add_macro_definition("DOS16=" XSTRINGIFY(DOS16), true);
#endif
#ifdef DOSWIN32
    ctx.add_macro_definition("DOSWIN32=" XSTRINGIFY(DOSWIN32), true);
#endif
#ifdef IA64
    ctx.add_macro_definition("IA64=" XSTRINGIFY(IA64), true);
#endif
#ifdef MAC
    ctx.add_macro_definition("MAC=" XSTRINGIFY(MAC), true);
#endif
#ifdef MIDL_PASS
    ctx.add_macro_definition("MIDL_PASS=" XSTRINGIFY(MIDL_PASS), true);
#endif
#ifdef NONAMELESSUNION
    ctx.add_macro_definition(
        "NONAMELESSUNION=" XSTRINGIFY(NONAMELESSUNION), true);
#endif
#ifdef OLE2ANSI
    ctx.add_macro_definition("OLE2ANSI=" XSTRINGIFY(OLE2ANSI), true);
#endif
#ifdef STDC
    ctx.add_macro_definition("STDC=" XSTRINGIFY(STDC), true);
#endif
#ifdef UNIX
    ctx.add_macro_definition("UNIX=" XSTRINGIFY(UNIX), true);
#endif
#ifdef WIN16
    ctx.add_macro_definition("WIN16=" XSTRINGIFY(WIN16), true);
#endif
#ifdef WIN32
    ctx.add_macro_definition("WIN32=" XSTRINGIFY(WIN32), true);
#endif
#ifdef WIN64
    ctx.add_macro_definition("WIN64=" XSTRINGIFY(WIN64), true);
#endif
#ifdef WINNT
    ctx.add_macro_definition("WINNT=" XSTRINGIFY(WINNT), true);
#endif
#ifdef WINVER
    ctx.add_macro_definition("WINVER=" XSTRINGIFY(WINVER), true);
#endif
#ifdef _68K_
    ctx.add_macro_definition("_68K_=" XSTRINGIFY(_68K_), true);
#endif
#ifdef _AIX
    ctx.add_macro_definition("_AIX=" XSTRINGIFY(_AIX), true);
#endif
#ifdef _ALPHA_
    ctx.add_macro_definition("_ALPHA_=" XSTRINGIFY(_ALPHA_), true);
#endif
#ifdef _AMD64_
    ctx.add_macro_definition("_AMD64_=" XSTRINGIFY(_AMD64_), true);
#endif
#ifdef _ARM64_
    ctx.add_macro_definition("_ARM64_=" XSTRINGIFY(_ARM64_), true);
#endif
#ifdef _ARM_
    ctx.add_macro_definition("_ARM_=" XSTRINGIFY(_ARM_), true);
#endif
#ifdef _AXP64_
    ctx.add_macro_definition("_AXP64_=" XSTRINGIFY(_AXP64_), true);
#endif
#ifdef _CHAR_UNSIGNED
    ctx.add_macro_definition(
        "_CHAR_UNSIGNED=" XSTRINGIFY(_CHAR_UNSIGNED), true);
#endif
#ifdef _FORTIFY_SOURCE
    ctx.add_macro_definition(
        "_FORTIFY_SOURCE=" XSTRINGIFY(_FORTIFY_SOURCE), true);
#endif
#ifdef _IA64_
    ctx.add_macro_definition("_IA64_=" XSTRINGIFY(_IA64_), true);
#endif
#ifdef _LONGLONG
    ctx.add_macro_definition("_LONGLONG=" XSTRINGIFY(_LONGLONG), true);
#endif
#ifdef _LP64
    ctx.add_macro_definition("_LP64=" XSTRINGIFY(_LP64), true);
#endif
#ifdef _MAC
    ctx.add_macro_definition("_MAC=" XSTRINGIFY(_MAC), true);
#endif
#ifdef _MAC_INT_64
    ctx.add_macro_definition("_MAC_INT_64=" XSTRINGIFY(_MAC_INT_64), true);
#endif
#ifdef _MIPS_
    ctx.add_macro_definition("_MIPS_=" XSTRINGIFY(_MIPS_), true);
#endif
#ifdef _MPPC_
    ctx.add_macro_definition("_MPPC_=" XSTRINGIFY(_MPPC_), true);
#endif
#ifdef _MSC_VER
    ctx.add_macro_definition("_MSC_VER=" XSTRINGIFY(_MSC_VER), true);
#endif
#ifdef _MT
    ctx.add_macro_definition("_MT=" XSTRINGIFY(_MT), true);
#endif
#ifdef _M_ALPHA
    ctx.add_macro_definition("_M_ALPHA=" XSTRINGIFY(_M_ALPHA), true);
#endif
#ifdef _M_AMD64
    ctx.add_macro_definition("_M_AMD64=" XSTRINGIFY(_M_AMD64), true);
#endif
#ifdef _M_AXP64
    ctx.add_macro_definition("_M_AXP64=" XSTRINGIFY(_M_AXP64), true);
#endif
#ifdef _M_CEE
    ctx.add_macro_definition("_M_CEE=" XSTRINGIFY(_M_CEE), true);
#endif
#ifdef _M_CEE_PURE
    ctx.add_macro_definition("_M_CEE_PURE=" XSTRINGIFY(_M_CEE_PURE), true);
#endif
#ifdef _M_I86MM
    ctx.add_macro_definition("_M_I86MM=" XSTRINGIFY(_M_I86MM), true);
#endif
#ifdef _M_IA64
    ctx.add_macro_definition("_M_IA64=" XSTRINGIFY(_M_IA64), true);
#endif
#ifdef _M_IX86
    ctx.add_macro_definition("_M_IX86=" XSTRINGIFY(_M_IX86), true);
#endif
#ifdef _M_MRX000
    ctx.add_macro_definition("_M_MRX000=" XSTRINGIFY(_M_MRX000), true);
#endif
#ifdef _M_PPC
    ctx.add_macro_definition("_M_PPC=" XSTRINGIFY(_M_PPC), true);
#endif
#ifdef _NO_COM
    ctx.add_macro_definition("_NO_COM=" XSTRINGIFY(_NO_COM), true);
#endif
#ifdef _POSIX_
    ctx.add_macro_definition("_POSIX_=" XSTRINGIFY(_POSIX_), true);
#endif
#ifdef _PPC_
    ctx.add_macro_definition("_PPC_=" XSTRINGIFY(_PPC_), true);
#endif
#ifdef _WIN32
    ctx.add_macro_definition("_WIN32=" XSTRINGIFY(_WIN32), true);
#endif
#ifdef _WIN32_FUSION
    ctx.add_macro_definition(
        "_WIN32_FUSION=" XSTRINGIFY(_WIN32_FUSION), true);
#endif
#ifdef _WIN32_IE
    ctx.add_macro_definition("_WIN32_IE=" XSTRINGIFY(_WIN32_IE), true);
#endif
#ifdef _WIN32_WCE
    ctx.add_macro_definition("_WIN32_WCE=" XSTRINGIFY(_WIN32_WCE), true);
#endif
#ifdef _WIN32_WINDOWS
    ctx.add_macro_definition(
        "_WIN32_WINDOWS=" XSTRINGIFY(_WIN32_WINDOWS), true);
#endif
#ifdef _WIN32_WINNT
    ctx.add_macro_definition(
        "_WIN32_WINNT=" XSTRINGIFY(_WIN32_WINNT), true);
#endif
#ifdef _WIN64
    ctx.add_macro_definition("_WIN64=" XSTRINGIFY(_WIN64), true);
#endif
#ifdef _WIN95
    ctx.add_macro_definition("_WIN95=" XSTRINGIFY(_WIN95), true);
#endif
#ifdef _WINDOWS
    ctx.add_macro_definition("_WINDOWS=" XSTRINGIFY(_WINDOWS), true);
#endif
#ifdef _WINNT
    ctx.add_macro_definition("_WINNT=" XSTRINGIFY(_WINNT), true);
#endif
#ifdef _Windows
    ctx.add_macro_definition("_Windows=" XSTRINGIFY(_Windows), true);
#endif
#ifdef _X86_
    ctx.add_macro_definition("_X86_=" XSTRINGIFY(_X86_), true);
#endif
#ifdef __APPLE__
    ctx.add_macro_definition("__APPLE__=" XSTRINGIFY(__APPLE__), true);
#endif
#ifdef __ATOMIC_ACQUIRE
    ctx.add_macro_definition(
        "__ATOMIC_ACQUIRE=" XSTRINGIFY(__ATOMIC_ACQUIRE), true);
#endif
#ifdef __ATOMIC_ACQ_REL
    ctx.add_macro_definition(
        "__ATOMIC_ACQ_REL=" XSTRINGIFY(__ATOMIC_ACQ_REL), true);
#endif
#ifdef __ATOMIC_CONSUME
    ctx.add_macro_definition(
        "__ATOMIC_CONSUME=" XSTRINGIFY(__ATOMIC_CONSUME), true);
#endif
#ifdef __ATOMIC_RELAXED
    ctx.add_macro_definition(
        "__ATOMIC_RELAXED=" XSTRINGIFY(__ATOMIC_RELAXED), true);
#endif
#ifdef __ATOMIC_RELEASE
    ctx.add_macro_definition(
        "__ATOMIC_RELEASE=" XSTRINGIFY(__ATOMIC_RELEASE), true);
#endif
#ifdef __ATOMIC_SEQ_CST
    ctx.add_macro_definition(
        "__ATOMIC_SEQ_CST=" XSTRINGIFY(__ATOMIC_SEQ_CST), true);
#endif
#ifdef __BIGGEST_ALIGNMENT__
    ctx.add_macro_definition(
        "__BIGGEST_ALIGNMENT__=" XSTRINGIFY(__BIGGEST_ALIGNMENT__), true);
#endif
#ifdef __BIG_ENDIAN__
    ctx.add_macro_definition(
        "__BIG_ENDIAN__=" XSTRINGIFY(__BIG_ENDIAN__), true);
#endif
#ifdef __BORLANDC__
    ctx.add_macro_definition(
        "__BORLANDC__=" XSTRINGIFY(__BORLANDC__), true);
#endif
#ifdef __BYTE_ORDER__
    ctx.add_macro_definition(
        "__BYTE_ORDER__=" XSTRINGIFY(__BYTE_ORDER__), true);
#endif
#ifdef __CHAR16_TYPE__
    ctx.add_macro_definition(
        "__CHAR16_TYPE__=" XSTRINGIFY(__CHAR16_TYPE__), true);
#endif
#ifdef __CHAR32_TYPE__
    ctx.add_macro_definition(
        "__CHAR32_TYPE__=" XSTRINGIFY(__CHAR32_TYPE__), true);
#endif
#ifdef __CHAR_BIT__
    ctx.add_macro_definition(
        "__CHAR_BIT__=" XSTRINGIFY(__CHAR_BIT__), true);
#endif
#ifdef __CONSTANT_CFSTRINGS__
    ctx.add_macro_definition(
        "__CONSTANT_CFSTRINGS__=" XSTRINGIFY(__CONSTANT_CFSTRINGS__),
        true);
#endif
#ifdef __CYGWIN__
    ctx.add_macro_definition("__CYGWIN__=" XSTRINGIFY(__CYGWIN__), true);
#endif
#ifdef __DBL_DECIMAL_DIG__
    ctx.add_macro_definition(
        "__DBL_DECIMAL_DIG__=" XSTRINGIFY(__DBL_DECIMAL_DIG__), true);
#endif
#ifdef __DBL_DENORM_MIN__
    ctx.add_macro_definition(
        "__DBL_DENORM_MIN__=" XSTRINGIFY(__DBL_DENORM_MIN__), true);
#endif
#ifdef __DBL_DIG__
    ctx.add_macro_definition("__DBL_DIG__=" XSTRINGIFY(__DBL_DIG__), true);
#endif
#ifdef __DBL_EPSILON__
    ctx.add_macro_definition(
        "__DBL_EPSILON__=" XSTRINGIFY(__DBL_EPSILON__), true);
#endif
#ifdef __DBL_HAS_DENORM__
    ctx.add_macro_definition(
        "__DBL_HAS_DENORM__=" XSTRINGIFY(__DBL_HAS_DENORM__), true);
#endif
#ifdef __DBL_HAS_INFINITY__
    ctx.add_macro_definition(
        "__DBL_HAS_INFINITY__=" XSTRINGIFY(__DBL_HAS_INFINITY__), true);
#endif
#ifdef __DBL_HAS_QUIET_NAN__
    ctx.add_macro_definition(
        "__DBL_HAS_QUIET_NAN__=" XSTRINGIFY(__DBL_HAS_QUIET_NAN__), true);
#endif
#ifdef __DBL_MANT_DIG__
    ctx.add_macro_definition(
        "__DBL_MANT_DIG__=" XSTRINGIFY(__DBL_MANT_DIG__), true);
#endif
#ifdef __DBL_MAX_10_EXP__
    ctx.add_macro_definition(
        "__DBL_MAX_10_EXP__=" XSTRINGIFY(__DBL_MAX_10_EXP__), true);
#endif
#ifdef __DBL_MAX_EXP__
    ctx.add_macro_definition(
        "__DBL_MAX_EXP__=" XSTRINGIFY(__DBL_MAX_EXP__), true);
#endif
#ifdef __DBL_MAX__
    ctx.add_macro_definition("__DBL_MAX__=" XSTRINGIFY(__DBL_MAX__), true);
#endif
#ifdef __DBL_MIN_10_EXP__
    ctx.add_macro_definition(
        "__DBL_MIN_10_EXP__=" XSTRINGIFY(__DBL_MIN_10_EXP__), true);
#endif
#ifdef __DBL_MIN_EXP__
    ctx.add_macro_definition(
        "__DBL_MIN_EXP__=" XSTRINGIFY(__DBL_MIN_EXP__), true);
#endif
#ifdef __DBL_MIN__
    ctx.add_macro_definition("__DBL_MIN__=" XSTRINGIFY(__DBL_MIN__), true);
#endif
#ifdef __DEC128_DEN__
    ctx.add_macro_definition(
        "__DEC128_DEN__=" XSTRINGIFY(__DEC128_DEN__), true);
#endif
#ifdef __DEC128_EPSILON__
    ctx.add_macro_definition(
        "__DEC128_EPSILON__=" XSTRINGIFY(__DEC128_EPSILON__), true);
#endif
#ifdef __DEC128_MANT_DIG__
    ctx.add_macro_definition(
        "__DEC128_MANT_DIG__=" XSTRINGIFY(__DEC128_MANT_DIG__), true);
#endif
#ifdef __DEC128_MAX_EXP__
    ctx.add_macro_definition(
        "__DEC128_MAX_EXP__=" XSTRINGIFY(__DEC128_MAX_EXP__), true);
#endif
#ifdef __DEC128_MAX__
    ctx.add_macro_definition(
        "__DEC128_MAX__=" XSTRINGIFY(__DEC128_MAX__), true);
#endif
#ifdef __DEC128_MIN_EXP__
    ctx.add_macro_definition(
        "__DEC128_MIN_EXP__=" XSTRINGIFY(__DEC128_MIN_EXP__), true);
#endif
#ifdef __DEC128_MIN__
    ctx.add_macro_definition(
        "__DEC128_MIN__=" XSTRINGIFY(__DEC128_MIN__), true);
#endif
#ifdef __DEC128_SUBNORMAL_MIN__
    ctx.add_macro_definition(
        "__DEC128_SUBNORMAL_MIN__=" XSTRINGIFY(__DEC128_SUBNORMAL_MIN__),
        true);
#endif
#ifdef __DEC32_DEN__
    ctx.add_macro_definition(
        "__DEC32_DEN__=" XSTRINGIFY(__DEC32_DEN__), true);
#endif
#ifdef __DEC32_EPSILON__
    ctx.add_macro_definition(
        "__DEC32_EPSILON__=" XSTRINGIFY(__DEC32_EPSILON__), true);
#endif
#ifdef __DEC32_MANT_DIG__
    ctx.add_macro_definition(
        "__DEC32_MANT_DIG__=" XSTRINGIFY(__DEC32_MANT_DIG__), true);
#endif
#ifdef __DEC32_MAX_EXP__
    ctx.add_macro_definition(
        "__DEC32_MAX_EXP__=" XSTRINGIFY(__DEC32_MAX_EXP__), true);
#endif
#ifdef __DEC32_MAX__
    ctx.add_macro_definition(
        "__DEC32_MAX__=" XSTRINGIFY(__DEC32_MAX__), true);
#endif
#ifdef __DEC32_MIN_EXP__
    ctx.add_macro_definition(
        "__DEC32_MIN_EXP__=" XSTRINGIFY(__DEC32_MIN_EXP__), true);
#endif
#ifdef __DEC32_MIN__
    ctx.add_macro_definition(
        "__DEC32_MIN__=" XSTRINGIFY(__DEC32_MIN__), true);
#endif
#ifdef __DEC32_SUBNORMAL_MIN__
    ctx.add_macro_definition(
        "__DEC32_SUBNORMAL_MIN__=" XSTRINGIFY(__DEC32_SUBNORMAL_MIN__),
        true);
#endif
#ifdef __DEC64_DEN__
    ctx.add_macro_definition(
        "__DEC64_DEN__=" XSTRINGIFY(__DEC64_DEN__), true);
#endif
#ifdef __DEC64_EPSILON__
    ctx.add_macro_definition(
        "__DEC64_EPSILON__=" XSTRINGIFY(__DEC64_EPSILON__), true);
#endif
#ifdef __DEC64_MANT_DIG__
    ctx.add_macro_definition(
        "__DEC64_MANT_DIG__=" XSTRINGIFY(__DEC64_MANT_DIG__), true);
#endif
#ifdef __DEC64_MAX_EXP__
    ctx.add_macro_definition(
        "__DEC64_MAX_EXP__=" XSTRINGIFY(__DEC64_MAX_EXP__), true);
#endif
#ifdef __DEC64_MAX__
    ctx.add_macro_definition(
        "__DEC64_MAX__=" XSTRINGIFY(__DEC64_MAX__), true);
#endif
#ifdef __DEC64_MIN_EXP__
    ctx.add_macro_definition(
        "__DEC64_MIN_EXP__=" XSTRINGIFY(__DEC64_MIN_EXP__), true);
#endif
#ifdef __DEC64_MIN__
    ctx.add_macro_definition(
        "__DEC64_MIN__=" XSTRINGIFY(__DEC64_MIN__), true);
#endif
#ifdef __DEC64_SUBNORMAL_MIN__
    ctx.add_macro_definition(
        "__DEC64_SUBNORMAL_MIN__=" XSTRINGIFY(__DEC64_SUBNORMAL_MIN__),
        true);
#endif
#ifdef __DECIMAL_BID_FORMAT__
    ctx.add_macro_definition(
        "__DECIMAL_BID_FORMAT__=" XSTRINGIFY(__DECIMAL_BID_FORMAT__),
        true);
#endif
#ifdef __DECIMAL_DIG__
    ctx.add_macro_definition(
        "__DECIMAL_DIG__=" XSTRINGIFY(__DECIMAL_DIG__), true);
#endif
#ifdef __DEC_EVAL_METHOD__
    ctx.add_macro_definition(
        "__DEC_EVAL_METHOD__=" XSTRINGIFY(__DEC_EVAL_METHOD__), true);
#endif
#ifdef __DragonFly__
    ctx.add_macro_definition(
        "__DragonFly__=" XSTRINGIFY(__DragonFly__), true);
#endif
#ifdef __ELF__
    ctx.add_macro_definition("__ELF__=" XSTRINGIFY(__ELF__), true);
#endif
#ifdef __FINITE_MATH_ONLY__
    ctx.add_macro_definition(
        "__FINITE_MATH_ONLY__=" XSTRINGIFY(__FINITE_MATH_ONLY__), true);
#endif
#ifdef __FLOAT_WORD_ORDER__
    ctx.add_macro_definition(
        "__FLOAT_WORD_ORDER__=" XSTRINGIFY(__FLOAT_WORD_ORDER__), true);
#endif
#ifdef __FLT_DECIMAL_DIG__
    ctx.add_macro_definition(
        "__FLT_DECIMAL_DIG__=" XSTRINGIFY(__FLT_DECIMAL_DIG__), true);
#endif
#ifdef __FLT_DENORM_MIN__
    ctx.add_macro_definition(
        "__FLT_DENORM_MIN__=" XSTRINGIFY(__FLT_DENORM_MIN__), true);
#endif
#ifdef __FLT_DIG__
    ctx.add_macro_definition("__FLT_DIG__=" XSTRINGIFY(__FLT_DIG__), true);
#endif
#ifdef __FLT_EPSILON__
    ctx.add_macro_definition(
        "__FLT_EPSILON__=" XSTRINGIFY(__FLT_EPSILON__), true);
#endif
#ifdef __FLT_EVAL_METHOD__
    ctx.add_macro_definition(
        "__FLT_EVAL_METHOD__=" XSTRINGIFY(__FLT_EVAL_METHOD__), true);
#endif
#ifdef __FLT_HAS_DENORM__
    ctx.add_macro_definition(
        "__FLT_HAS_DENORM__=" XSTRINGIFY(__FLT_HAS_DENORM__), true);
#endif
#ifdef __FLT_HAS_INFINITY__
    ctx.add_macro_definition(
        "__FLT_HAS_INFINITY__=" XSTRINGIFY(__FLT_HAS_INFINITY__), true);
#endif
#ifdef __FLT_HAS_QUIET_NAN__
    ctx.add_macro_definition(
        "__FLT_HAS_QUIET_NAN__=" XSTRINGIFY(__FLT_HAS_QUIET_NAN__), true);
#endif
#ifdef __FLT_MANT_DIG__
    ctx.add_macro_definition(
        "__FLT_MANT_DIG__=" XSTRINGIFY(__FLT_MANT_DIG__), true);
#endif
#ifdef __FLT_MAX_10_EXP__
    ctx.add_macro_definition(
        "__FLT_MAX_10_EXP__=" XSTRINGIFY(__FLT_MAX_10_EXP__), true);
#endif
#ifdef __FLT_MAX_EXP__
    ctx.add_macro_definition(
        "__FLT_MAX_EXP__=" XSTRINGIFY(__FLT_MAX_EXP__), true);
#endif
#ifdef __FLT_MAX__
    ctx.add_macro_definition("__FLT_MAX__=" XSTRINGIFY(__FLT_MAX__), true);
#endif
#ifdef __FLT_MIN_10_EXP__
    ctx.add_macro_definition(
        "__FLT_MIN_10_EXP__=" XSTRINGIFY(__FLT_MIN_10_EXP__), true);
#endif
#ifdef __FLT_MIN_EXP__
    ctx.add_macro_definition(
        "__FLT_MIN_EXP__=" XSTRINGIFY(__FLT_MIN_EXP__), true);
#endif
#ifdef __FLT_MIN__
    ctx.add_macro_definition("__FLT_MIN__=" XSTRINGIFY(__FLT_MIN__), true);
#endif
#ifdef __FLT_RADIX__
    ctx.add_macro_definition(
        "__FLT_RADIX__=" XSTRINGIFY(__FLT_RADIX__), true);
#endif
#ifdef __FreeBSD__
    ctx.add_macro_definition("__FreeBSD__=" XSTRINGIFY(__FreeBSD__), true);
#endif
#ifdef __FreeBSD_cc_version
    ctx.add_macro_definition(
        "__FreeBSD_cc_version=" XSTRINGIFY(__FreeBSD_cc_version), true);
#endif
#ifdef __GCC_ATOMIC_BOOL_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_BOOL_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_BOOL_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_CHAR16_T_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_CHAR16_T_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_CHAR16_T_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_CHAR32_T_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_CHAR32_T_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_CHAR32_T_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_CHAR_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_CHAR_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_CHAR_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_INT_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_INT_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_INT_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_LLONG_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_LLONG_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_LLONG_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_LONG_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_LONG_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_LONG_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_POINTER_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_POINTER_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_POINTER_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_SHORT_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_SHORT_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_SHORT_LOCK_FREE),
        true);
#endif
#ifdef __GCC_ATOMIC_TEST_AND_SET_TRUEVAL
    ctx.add_macro_definition(
        "__GCC_ATOMIC_TEST_AND_SET_TRUEVAL=" XSTRINGIFY(
            __GCC_ATOMIC_TEST_AND_SET_TRUEVAL),
        true);
#endif
#ifdef __GCC_ATOMIC_WCHAR_T_LOCK_FREE
    ctx.add_macro_definition(
        "__GCC_ATOMIC_WCHAR_T_LOCK_FREE=" XSTRINGIFY(
            __GCC_ATOMIC_WCHAR_T_LOCK_FREE),
        true);
#endif
#ifdef __GCC_HAVE_DWARF2_CFI_ASM
    ctx.add_macro_definition(
        "__GCC_HAVE_DWARF2_CFI_ASM=" XSTRINGIFY(__GCC_HAVE_DWARF2_CFI_ASM),
        true);
#endif
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1
    ctx.add_macro_definition(
        "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1=" XSTRINGIFY(
            __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1),
        true);
#endif
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
    ctx.add_macro_definition(
        "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2=" XSTRINGIFY(
            __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2),
        true);
#endif
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
    ctx.add_macro_definition(
        "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4=" XSTRINGIFY(
            __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4),
        true);
#endif
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8
    ctx.add_macro_definition(
        "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8=" XSTRINGIFY(
            __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8),
        true);
#endif
#ifdef __GNUC_GNU_INLINE__
    ctx.add_macro_definition(
        "__GNUC_GNU_INLINE__=" XSTRINGIFY(__GNUC_GNU_INLINE__), true);
#endif
#ifdef __GNUC_MINOR__
    ctx.add_macro_definition(
        "__GNUC_MINOR__=" XSTRINGIFY(__GNUC_MINOR__), true);
#endif
#ifdef __GNUC_PATCHLEVEL__
    ctx.add_macro_definition(
        "__GNUC_PATCHLEVEL__=" XSTRINGIFY(__GNUC_PATCHLEVEL__), true);
#endif
#ifdef __GNUC_STDC_INLINE__
    ctx.add_macro_definition(
        "__GNUC_STDC_INLINE__=" XSTRINGIFY(__GNUC_STDC_INLINE__), true);
#endif
#ifdef __GNUC__
    ctx.add_macro_definition("__GNUC__=" XSTRINGIFY(__GNUC__), true);
#endif
#ifdef __GXX_ABI_VERSION
    ctx.add_macro_definition(
        "__GXX_ABI_VERSION=" XSTRINGIFY(__GXX_ABI_VERSION), true);
#endif
#ifdef __GXX_RTTI
    ctx.add_macro_definition("__GXX_RTTI=" XSTRINGIFY(__GXX_RTTI), true);
#endif
#ifdef __ILP64__
    ctx.add_macro_definition("__ILP64__=" XSTRINGIFY(__ILP64__), true);
#endif
#ifdef __INT16_C
    ctx.add_macro_definition(
        "__INT16_C(c)=" XSTRINGIFY(__INT16_C(c)), true);
#endif
#ifdef __INT16_MAX__
    ctx.add_macro_definition(
        "__INT16_MAX__=" XSTRINGIFY(__INT16_MAX__), true);
#endif
#ifdef __INT16_TYPE__
    ctx.add_macro_definition(
        "__INT16_TYPE__=" XSTRINGIFY(__INT16_TYPE__), true);
#endif
#ifdef __INT32_C
    ctx.add_macro_definition(
        "__INT32_C(c)=" XSTRINGIFY(__INT32_C(c)), true);
#endif
#ifdef __INT32_MAX__
    ctx.add_macro_definition(
        "__INT32_MAX__=" XSTRINGIFY(__INT32_MAX__), true);
#endif
#ifdef __INT32_TYPE__
    ctx.add_macro_definition(
        "__INT32_TYPE__=" XSTRINGIFY(__INT32_TYPE__), true);
#endif
#ifdef __INT64_C
    ctx.add_macro_definition(
        "__INT64_C(c)=" XSTRINGIFY(__INT64_C(c)), true);
#endif
#ifdef __INT64_C_SUFFIX__
    ctx.add_macro_definition(
        "__INT64_C_SUFFIX__=" XSTRINGIFY(__INT64_C_SUFFIX__), true);
#endif
#ifdef __INT64_MAX__
    ctx.add_macro_definition(
        "__INT64_MAX__=" XSTRINGIFY(__INT64_MAX__), true);
#endif
#ifdef __INT64_TYPE__
    ctx.add_macro_definition(
        "__INT64_TYPE__=" XSTRINGIFY(__INT64_TYPE__), true);
#endif
#ifdef __INT8_C
    ctx.add_macro_definition("__INT8_C(c)=" XSTRINGIFY(__INT8_C(c)), true);
#endif
#ifdef __INT8_MAX__
    ctx.add_macro_definition(
        "__INT8_MAX__=" XSTRINGIFY(__INT8_MAX__), true);
#endif
#ifdef __INT8_TYPE__
    ctx.add_macro_definition(
        "__INT8_TYPE__=" XSTRINGIFY(__INT8_TYPE__), true);
#endif
#ifdef __INTMAX_C
    ctx.add_macro_definition(
        "__INTMAX_C(c)=" XSTRINGIFY(__INTMAX_C(c)), true);
#endif
#ifdef __INTMAX_MAX__
    ctx.add_macro_definition(
        "__INTMAX_MAX__=" XSTRINGIFY(__INTMAX_MAX__), true);
#endif
#ifdef __INTMAX_TYPE__
    ctx.add_macro_definition(
        "__INTMAX_TYPE__=" XSTRINGIFY(__INTMAX_TYPE__), true);
#endif
#ifdef __INTMAX_WIDTH__
    ctx.add_macro_definition(
        "__INTMAX_WIDTH__=" XSTRINGIFY(__INTMAX_WIDTH__), true);
#endif
#ifdef __INTPTR_MAX__
    ctx.add_macro_definition(
        "__INTPTR_MAX__=" XSTRINGIFY(__INTPTR_MAX__), true);
#endif
#ifdef __INTPTR_TYPE__
    ctx.add_macro_definition(
        "__INTPTR_TYPE__=" XSTRINGIFY(__INTPTR_TYPE__), true);
#endif
#ifdef __INTPTR_WIDTH__
    ctx.add_macro_definition(
        "__INTPTR_WIDTH__=" XSTRINGIFY(__INTPTR_WIDTH__), true);
#endif
#ifdef __INT_FAST16_MAX__
    ctx.add_macro_definition(
        "__INT_FAST16_MAX__=" XSTRINGIFY(__INT_FAST16_MAX__), true);
#endif
#ifdef __INT_FAST16_TYPE__
    ctx.add_macro_definition(
        "__INT_FAST16_TYPE__=" XSTRINGIFY(__INT_FAST16_TYPE__), true);
#endif
#ifdef __INT_FAST32_MAX__
    ctx.add_macro_definition(
        "__INT_FAST32_MAX__=" XSTRINGIFY(__INT_FAST32_MAX__), true);
#endif
#ifdef __INT_FAST32_TYPE__
    ctx.add_macro_definition(
        "__INT_FAST32_TYPE__=" XSTRINGIFY(__INT_FAST32_TYPE__), true);
#endif
#ifdef __INT_FAST64_MAX__
    ctx.add_macro_definition(
        "__INT_FAST64_MAX__=" XSTRINGIFY(__INT_FAST64_MAX__), true);
#endif
#ifdef __INT_FAST64_TYPE__
    ctx.add_macro_definition(
        "__INT_FAST64_TYPE__=" XSTRINGIFY(__INT_FAST64_TYPE__), true);
#endif
#ifdef __INT_FAST8_MAX__
    ctx.add_macro_definition(
        "__INT_FAST8_MAX__=" XSTRINGIFY(__INT_FAST8_MAX__), true);
#endif
#ifdef __INT_FAST8_TYPE__
    ctx.add_macro_definition(
        "__INT_FAST8_TYPE__=" XSTRINGIFY(__INT_FAST8_TYPE__), true);
#endif
#ifdef __INT_LEAST16_MAX__
    ctx.add_macro_definition(
        "__INT_LEAST16_MAX__=" XSTRINGIFY(__INT_LEAST16_MAX__), true);
#endif
#ifdef __INT_LEAST16_TYPE__
    ctx.add_macro_definition(
        "__INT_LEAST16_TYPE__=" XSTRINGIFY(__INT_LEAST16_TYPE__), true);
#endif
#ifdef __INT_LEAST32_MAX__
    ctx.add_macro_definition(
        "__INT_LEAST32_MAX__=" XSTRINGIFY(__INT_LEAST32_MAX__), true);
#endif
#ifdef __INT_LEAST32_TYPE__
    ctx.add_macro_definition(
        "__INT_LEAST32_TYPE__=" XSTRINGIFY(__INT_LEAST32_TYPE__), true);
#endif
#ifdef __INT_LEAST64_MAX__
    ctx.add_macro_definition(
        "__INT_LEAST64_MAX__=" XSTRINGIFY(__INT_LEAST64_MAX__), true);
#endif
#ifdef __INT_LEAST64_TYPE__
    ctx.add_macro_definition(
        "__INT_LEAST64_TYPE__=" XSTRINGIFY(__INT_LEAST64_TYPE__), true);
#endif
#ifdef __INT_LEAST8_MAX__
    ctx.add_macro_definition(
        "__INT_LEAST8_MAX__=" XSTRINGIFY(__INT_LEAST8_MAX__), true);
#endif
#ifdef __INT_LEAST8_TYPE__
    ctx.add_macro_definition(
        "__INT_LEAST8_TYPE__=" XSTRINGIFY(__INT_LEAST8_TYPE__), true);
#endif
#ifdef __INT_MAX__
    ctx.add_macro_definition("__INT_MAX__=" XSTRINGIFY(__INT_MAX__), true);
#endif
#ifdef __KPRINTF_ATTRIBUTE__
    ctx.add_macro_definition(
        "__KPRINTF_ATTRIBUTE__=" XSTRINGIFY(__KPRINTF_ATTRIBUTE__), true);
#endif
#ifdef __LDBL_DENORM_MIN__
    ctx.add_macro_definition(
        "__LDBL_DENORM_MIN__=" XSTRINGIFY(__LDBL_DENORM_MIN__), true);
#endif
#ifdef __LDBL_DIG__
    ctx.add_macro_definition(
        "__LDBL_DIG__=" XSTRINGIFY(__LDBL_DIG__), true);
#endif
#ifdef __LDBL_EPSILON__
    ctx.add_macro_definition(
        "__LDBL_EPSILON__=" XSTRINGIFY(__LDBL_EPSILON__), true);
#endif
#ifdef __LDBL_HAS_DENORM__
    ctx.add_macro_definition(
        "__LDBL_HAS_DENORM__=" XSTRINGIFY(__LDBL_HAS_DENORM__), true);
#endif
#ifdef __LDBL_HAS_INFINITY__
    ctx.add_macro_definition(
        "__LDBL_HAS_INFINITY__=" XSTRINGIFY(__LDBL_HAS_INFINITY__), true);
#endif
#ifdef __LDBL_HAS_QUIET_NAN__
    ctx.add_macro_definition(
        "__LDBL_HAS_QUIET_NAN__=" XSTRINGIFY(__LDBL_HAS_QUIET_NAN__),
        true);
#endif
#ifdef __LDBL_MANT_DIG__
    ctx.add_macro_definition(
        "__LDBL_MANT_DIG__=" XSTRINGIFY(__LDBL_MANT_DIG__), true);
#endif
#ifdef __LDBL_MAX_10_EXP__
    ctx.add_macro_definition(
        "__LDBL_MAX_10_EXP__=" XSTRINGIFY(__LDBL_MAX_10_EXP__), true);
#endif
#ifdef __LDBL_MAX_EXP__
    ctx.add_macro_definition(
        "__LDBL_MAX_EXP__=" XSTRINGIFY(__LDBL_MAX_EXP__), true);
#endif
#ifdef __LDBL_MAX__
    ctx.add_macro_definition(
        "__LDBL_MAX__=" XSTRINGIFY(__LDBL_MAX__), true);
#endif
#ifdef __LDBL_MIN_10_EXP__
    ctx.add_macro_definition(
        "__LDBL_MIN_10_EXP__=" XSTRINGIFY(__LDBL_MIN_10_EXP__), true);
#endif
#ifdef __LDBL_MIN_EXP__
    ctx.add_macro_definition(
        "__LDBL_MIN_EXP__=" XSTRINGIFY(__LDBL_MIN_EXP__), true);
#endif
#ifdef __LDBL_MIN__
    ctx.add_macro_definition(
        "__LDBL_MIN__=" XSTRINGIFY(__LDBL_MIN__), true);
#endif
#ifdef __LITTLE_ENDIAN__
    ctx.add_macro_definition(
        "__LITTLE_ENDIAN__=" XSTRINGIFY(__LITTLE_ENDIAN__), true);
#endif
#ifdef __LLP64__
    ctx.add_macro_definition("__LLP64__=" XSTRINGIFY(__LLP64__), true);
#endif
#ifdef __LONG_LONG_MAX__
    ctx.add_macro_definition(
        "__LONG_LONG_MAX__=" XSTRINGIFY(__LONG_LONG_MAX__), true);
#endif
#ifdef __LONG_MAX__
    ctx.add_macro_definition(
        "__LONG_MAX__=" XSTRINGIFY(__LONG_MAX__), true);
#endif
#ifdef __LP64__
    ctx.add_macro_definition("__LP64__=" XSTRINGIFY(__LP64__), true);
#endif
#ifdef __MINGW32__
    ctx.add_macro_definition("__MINGW32__=" XSTRINGIFY(__MINGW32__), true);
#endif
#ifdef __MMX__
    ctx.add_macro_definition("__MMX__=" XSTRINGIFY(__MMX__), true);
#endif
#ifdef __MT__
    ctx.add_macro_definition("__MT__=" XSTRINGIFY(__MT__), true);
#endif
#ifdef __NO_INLINE__
    ctx.add_macro_definition(
        "__NO_INLINE__=" XSTRINGIFY(__NO_INLINE__), true);
#endif
#ifdef __NO_MATH_INLINES
    ctx.add_macro_definition(
        "__NO_MATH_INLINES=" XSTRINGIFY(__NO_MATH_INLINES), true);
#endif
#ifdef __NetBSD__
    ctx.add_macro_definition("__NetBSD__=" XSTRINGIFY(__NetBSD__), true);
#endif
#ifdef __ORDER_BIG_ENDIAN__
    ctx.add_macro_definition(
        "__ORDER_BIG_ENDIAN__=" XSTRINGIFY(__ORDER_BIG_ENDIAN__), true);
#endif
#ifdef __ORDER_LITTLE_ENDIAN__
    ctx.add_macro_definition(
        "__ORDER_LITTLE_ENDIAN__=" XSTRINGIFY(__ORDER_LITTLE_ENDIAN__),
        true);
#endif
#ifdef __ORDER_PDP_ENDIAN__
    ctx.add_macro_definition(
        "__ORDER_PDP_ENDIAN__=" XSTRINGIFY(__ORDER_PDP_ENDIAN__), true);
#endif
#ifdef __OS2__
    ctx.add_macro_definition("__OS2__=" XSTRINGIFY(__OS2__), true);
#endif
#ifdef __OS400__
    ctx.add_macro_definition("__OS400__=" XSTRINGIFY(__OS400__), true);
#endif
#ifdef __OpenBSD__
    ctx.add_macro_definition("__OpenBSD__=" XSTRINGIFY(__OpenBSD__), true);
#endif
#ifdef __PAS__
    ctx.add_macro_definition("__PAS__=" XSTRINGIFY(__PAS__), true);
#endif
#ifdef __POINTER_WIDTH__
    ctx.add_macro_definition(
        "__POINTER_WIDTH__=" XSTRINGIFY(__POINTER_WIDTH__), true);
#endif
#ifdef __PRAGMA_REDEFINE_EXTNAME
    ctx.add_macro_definition(
        "__PRAGMA_REDEFINE_EXTNAME=" XSTRINGIFY(__PRAGMA_REDEFINE_EXTNAME),
        true);
#endif
#ifdef __PTRDIFF_MAX__
    ctx.add_macro_definition(
        "__PTRDIFF_MAX__=" XSTRINGIFY(__PTRDIFF_MAX__), true);
#endif
#ifdef __PTRDIFF_TYPE__
    ctx.add_macro_definition(
        "__PTRDIFF_TYPE__=" XSTRINGIFY(__PTRDIFF_TYPE__), true);
#endif
#ifdef __PTRDIFF_WIDTH__
    ctx.add_macro_definition(
        "__PTRDIFF_WIDTH__=" XSTRINGIFY(__PTRDIFF_WIDTH__), true);
#endif
#ifdef __REGISTER_PREFIX__
    ctx.add_macro_definition(
        "__REGISTER_PREFIX__=" XSTRINGIFY(__REGISTER_PREFIX__), true);
#endif
#ifdef __SCHAR_MAX__
    ctx.add_macro_definition(
        "__SCHAR_MAX__=" XSTRINGIFY(__SCHAR_MAX__), true);
#endif
#ifdef __SHRT_MAX__
    ctx.add_macro_definition(
        "__SHRT_MAX__=" XSTRINGIFY(__SHRT_MAX__), true);
#endif
#ifdef __SIG_ATOMIC_MAX__
    ctx.add_macro_definition(
        "__SIG_ATOMIC_MAX__=" XSTRINGIFY(__SIG_ATOMIC_MAX__), true);
#endif
#ifdef __SIG_ATOMIC_MIN__
    ctx.add_macro_definition(
        "__SIG_ATOMIC_MIN__=" XSTRINGIFY(__SIG_ATOMIC_MIN__), true);
#endif
#ifdef __SIG_ATOMIC_TYPE__
    ctx.add_macro_definition(
        "__SIG_ATOMIC_TYPE__=" XSTRINGIFY(__SIG_ATOMIC_TYPE__), true);
#endif
#ifdef __SIG_ATOMIC_WIDTH__
    ctx.add_macro_definition(
        "__SIG_ATOMIC_WIDTH__=" XSTRINGIFY(__SIG_ATOMIC_WIDTH__), true);
#endif
#ifdef __SIZEOF_DOUBLE__
    ctx.add_macro_definition(
        "__SIZEOF_DOUBLE__=" XSTRINGIFY(__SIZEOF_DOUBLE__), true);
#endif
#ifdef __SIZEOF_FLOAT__
    ctx.add_macro_definition(
        "__SIZEOF_FLOAT__=" XSTRINGIFY(__SIZEOF_FLOAT__), true);
#endif
#ifdef __SIZEOF_INT128__
    ctx.add_macro_definition(
        "__SIZEOF_INT128__=" XSTRINGIFY(__SIZEOF_INT128__), true);
#endif
#ifdef __SIZEOF_INT__
    ctx.add_macro_definition(
        "__SIZEOF_INT__=" XSTRINGIFY(__SIZEOF_INT__), true);
#endif
#ifdef __SIZEOF_LONG_DOUBLE__
    ctx.add_macro_definition(
        "__SIZEOF_LONG_DOUBLE__=" XSTRINGIFY(__SIZEOF_LONG_DOUBLE__),
        true);
#endif
#ifdef __SIZEOF_LONG_LONG__
    ctx.add_macro_definition(
        "__SIZEOF_LONG_LONG__=" XSTRINGIFY(__SIZEOF_LONG_LONG__), true);
#endif
#ifdef __SIZEOF_LONG__
    ctx.add_macro_definition(
        "__SIZEOF_LONG__=" XSTRINGIFY(__SIZEOF_LONG__), true);
#endif
#ifdef __SIZEOF_POINTER__
    ctx.add_macro_definition(
        "__SIZEOF_POINTER__=" XSTRINGIFY(__SIZEOF_POINTER__), true);
#endif
#ifdef __SIZEOF_PTRDIFF_T__
    ctx.add_macro_definition(
        "__SIZEOF_PTRDIFF_T__=" XSTRINGIFY(__SIZEOF_PTRDIFF_T__), true);
#endif
#ifdef __SIZEOF_SHORT__
    ctx.add_macro_definition(
        "__SIZEOF_SHORT__=" XSTRINGIFY(__SIZEOF_SHORT__), true);
#endif
#ifdef __SIZEOF_SIZE_T__
    ctx.add_macro_definition(
        "__SIZEOF_SIZE_T__=" XSTRINGIFY(__SIZEOF_SIZE_T__), true);
#endif
#ifdef __SIZEOF_WCHAR_T__
    ctx.add_macro_definition(
        "__SIZEOF_WCHAR_T__=" XSTRINGIFY(__SIZEOF_WCHAR_T__), true);
#endif
#ifdef __SIZEOF_WINT_T__
    ctx.add_macro_definition(
        "__SIZEOF_WINT_T__=" XSTRINGIFY(__SIZEOF_WINT_T__), true);
#endif
#ifdef __SIZE_MAX__
    ctx.add_macro_definition(
        "__SIZE_MAX__=" XSTRINGIFY(__SIZE_MAX__), true);
#endif
#ifdef __SIZE_TYPE__
    ctx.add_macro_definition(
        "__SIZE_TYPE__=" XSTRINGIFY(__SIZE_TYPE__), true);
#endif
#ifdef __SIZE_WIDTH__
    ctx.add_macro_definition(
        "__SIZE_WIDTH__=" XSTRINGIFY(__SIZE_WIDTH__), true);
#endif
#ifdef __SSE2_MATH__
    ctx.add_macro_definition(
        "__SSE2_MATH__=" XSTRINGIFY(__SSE2_MATH__), true);
#endif
#ifdef __SSE2__
    ctx.add_macro_definition("__SSE2__=" XSTRINGIFY(__SSE2__), true);
#endif
#ifdef __SSE_MATH__
    ctx.add_macro_definition(
        "__SSE_MATH__=" XSTRINGIFY(__SSE_MATH__), true);
#endif
#ifdef __SSE__
    ctx.add_macro_definition("__SSE__=" XSTRINGIFY(__SSE__), true);
#endif
#ifdef __SSP__
    ctx.add_macro_definition("__SSP__=" XSTRINGIFY(__SSP__), true);
#endif
#ifdef __STDCPP_STRICT_POINTER_SAFETY__
    ctx.add_macro_definition(
        "__STDCPP_STRICT_POINTER_SAFETY__=" XSTRINGIFY(
            __STDCPP_STRICT_POINTER_SAFETY__),
        true);
#endif
#ifdef __STDCPP_THREADS__
    ctx.add_macro_definition(
        "__STDCPP_THREADS__=" XSTRINGIFY(__STDCPP_THREADS__), true);
#endif
#ifdef __STDC_HOSTED__
    ctx.add_macro_definition(
        "__STDC_HOSTED__=" XSTRINGIFY(__STDC_HOSTED__), true);
#endif
#ifdef __STDC_IEC_559_COMPLEX__
    ctx.add_macro_definition(
        "__STDC_IEC_559_COMPLEX__=" XSTRINGIFY(__STDC_IEC_559_COMPLEX__),
        true);
#endif
#ifdef __STDC_IEC_559__
    ctx.add_macro_definition(
        "__STDC_IEC_559__=" XSTRINGIFY(__STDC_IEC_559__), true);
#endif
#ifdef __STDC_ISO_10646__
    ctx.add_macro_definition(
        "__STDC_ISO_10646__=" XSTRINGIFY(__STDC_ISO_10646__), true);
#endif
#ifdef __STDC_MB_MIGHT_NEQ_WC__
    ctx.add_macro_definition(
        "__STDC_MB_MIGHT_NEQ_WC__=" XSTRINGIFY(__STDC_MB_MIGHT_NEQ_WC__),
        true);
#endif
#ifdef __STDC_VERSION__
    ctx.add_macro_definition(
        "__STDC_VERSION__=" XSTRINGIFY(__STDC_VERSION__), true);
#endif
#ifdef __STDC__
    ctx.add_macro_definition("__STDC__=" XSTRINGIFY(__STDC__), true);
#endif
#ifdef __TURBOC__
    ctx.add_macro_definition("__TURBOC__=" XSTRINGIFY(__TURBOC__), true);
#endif
#ifdef __UINT16_C
    ctx.add_macro_definition(
        "__UINT16_C(c)=" XSTRINGIFY(__UINT16_C(c)), true);
#endif
#ifdef __UINT16_MAX__
    ctx.add_macro_definition(
        "__UINT16_MAX__=" XSTRINGIFY(__UINT16_MAX__), true);
#endif
#ifdef __UINT16_TYPE__
    ctx.add_macro_definition(
        "__UINT16_TYPE__=" XSTRINGIFY(__UINT16_TYPE__), true);
#endif
#ifdef __UINT32_C
    ctx.add_macro_definition(
        "__UINT32_C(c)=" XSTRINGIFY(__UINT32_C(c)), true);
#endif
#ifdef __UINT32_MAX__
    ctx.add_macro_definition(
        "__UINT32_MAX__=" XSTRINGIFY(__UINT32_MAX__), true);
#endif
#ifdef __UINT32_TYPE__
    ctx.add_macro_definition(
        "__UINT32_TYPE__=" XSTRINGIFY(__UINT32_TYPE__), true);
#endif
#ifdef __UINT64_C
    ctx.add_macro_definition(
        "__UINT64_C(c)=" XSTRINGIFY(__UINT64_C(c)), true);
#endif
#ifdef __UINT64_MAX__
    ctx.add_macro_definition(
        "__UINT64_MAX__=" XSTRINGIFY(__UINT64_MAX__), true);
#endif
#ifdef __UINT64_TYPE__
    ctx.add_macro_definition(
        "__UINT64_TYPE__=" XSTRINGIFY(__UINT64_TYPE__), true);
#endif
#ifdef __UINT8_C
    ctx.add_macro_definition(
        "__UINT8_C(c)=" XSTRINGIFY(__UINT8_C(c)), true);
#endif
#ifdef __UINT8_MAX__
    ctx.add_macro_definition(
        "__UINT8_MAX__=" XSTRINGIFY(__UINT8_MAX__), true);
#endif
#ifdef __UINT8_TYPE__
    ctx.add_macro_definition(
        "__UINT8_TYPE__=" XSTRINGIFY(__UINT8_TYPE__), true);
#endif
#ifdef __UINTMAX_C
    ctx.add_macro_definition(
        "__UINTMAX_C(c)=" XSTRINGIFY(__UINTMAX_C(c)), true);
#endif
#ifdef __UINTMAX_MAX__
    ctx.add_macro_definition(
        "__UINTMAX_MAX__=" XSTRINGIFY(__UINTMAX_MAX__), true);
#endif
#ifdef __UINTMAX_TYPE__
    ctx.add_macro_definition(
        "__UINTMAX_TYPE__=" XSTRINGIFY(__UINTMAX_TYPE__), true);
#endif
#ifdef __UINTPTR_MAX__
    ctx.add_macro_definition(
        "__UINTPTR_MAX__=" XSTRINGIFY(__UINTPTR_MAX__), true);
#endif
#ifdef __UINTPTR_TYPE__
    ctx.add_macro_definition(
        "__UINTPTR_TYPE__=" XSTRINGIFY(__UINTPTR_TYPE__), true);
#endif
#ifdef __UINT_FAST16_MAX__
    ctx.add_macro_definition(
        "__UINT_FAST16_MAX__=" XSTRINGIFY(__UINT_FAST16_MAX__), true);
#endif
#ifdef __UINT_FAST16_TYPE__
    ctx.add_macro_definition(
        "__UINT_FAST16_TYPE__=" XSTRINGIFY(__UINT_FAST16_TYPE__), true);
#endif
#ifdef __UINT_FAST32_MAX__
    ctx.add_macro_definition(
        "__UINT_FAST32_MAX__=" XSTRINGIFY(__UINT_FAST32_MAX__), true);
#endif
#ifdef __UINT_FAST32_TYPE__
    ctx.add_macro_definition(
        "__UINT_FAST32_TYPE__=" XSTRINGIFY(__UINT_FAST32_TYPE__), true);
#endif
#ifdef __UINT_FAST64_MAX__
    ctx.add_macro_definition(
        "__UINT_FAST64_MAX__=" XSTRINGIFY(__UINT_FAST64_MAX__), true);
#endif
#ifdef __UINT_FAST64_TYPE__
    ctx.add_macro_definition(
        "__UINT_FAST64_TYPE__=" XSTRINGIFY(__UINT_FAST64_TYPE__), true);
#endif
#ifdef __UINT_FAST8_MAX__
    ctx.add_macro_definition(
        "__UINT_FAST8_MAX__=" XSTRINGIFY(__UINT_FAST8_MAX__), true);
#endif
#ifdef __UINT_FAST8_TYPE__
    ctx.add_macro_definition(
        "__UINT_FAST8_TYPE__=" XSTRINGIFY(__UINT_FAST8_TYPE__), true);
#endif
#ifdef __UINT_LEAST16_MAX__
    ctx.add_macro_definition(
        "__UINT_LEAST16_MAX__=" XSTRINGIFY(__UINT_LEAST16_MAX__), true);
#endif
#ifdef __UINT_LEAST16_TYPE__
    ctx.add_macro_definition(
        "__UINT_LEAST16_TYPE__=" XSTRINGIFY(__UINT_LEAST16_TYPE__), true);
#endif
#ifdef __UINT_LEAST32_MAX__
    ctx.add_macro_definition(
        "__UINT_LEAST32_MAX__=" XSTRINGIFY(__UINT_LEAST32_MAX__), true);
#endif
#ifdef __UINT_LEAST32_TYPE__
    ctx.add_macro_definition(
        "__UINT_LEAST32_TYPE__=" XSTRINGIFY(__UINT_LEAST32_TYPE__), true);
#endif
#ifdef __UINT_LEAST64_MAX__
    ctx.add_macro_definition(
        "__UINT_LEAST64_MAX__=" XSTRINGIFY(__UINT_LEAST64_MAX__), true);
#endif
#ifdef __UINT_LEAST64_TYPE__
    ctx.add_macro_definition(
        "__UINT_LEAST64_TYPE__=" XSTRINGIFY(__UINT_LEAST64_TYPE__), true);
#endif
#ifdef __UINT_LEAST8_MAX__
    ctx.add_macro_definition(
        "__UINT_LEAST8_MAX__=" XSTRINGIFY(__UINT_LEAST8_MAX__), true);
#endif
#ifdef __UINT_LEAST8_TYPE__
    ctx.add_macro_definition(
        "__UINT_LEAST8_TYPE__=" XSTRINGIFY(__UINT_LEAST8_TYPE__), true);
#endif
#ifdef __USER_LABEL_PREFIX__
    ctx.add_macro_definition(
        "__USER_LABEL_PREFIX__=" XSTRINGIFY(__USER_LABEL_PREFIX__), true);
#endif
#ifdef __VERSION__
    ctx.add_macro_definition("__VERSION__=" XSTRINGIFY(__VERSION__), true);
#endif
#ifdef __WCHAR_MAX__
    ctx.add_macro_definition(
        "__WCHAR_MAX__=" XSTRINGIFY(__WCHAR_MAX__), true);
#endif
#ifdef __WCHAR_MIN__
    ctx.add_macro_definition(
        "__WCHAR_MIN__=" XSTRINGIFY(__WCHAR_MIN__), true);
#endif
#ifdef __WCHAR_TYPE__
    ctx.add_macro_definition(
        "__WCHAR_TYPE__=" XSTRINGIFY(__WCHAR_TYPE__), true);
#endif
#ifdef __WCHAR_WIDTH__
    ctx.add_macro_definition(
        "__WCHAR_WIDTH__=" XSTRINGIFY(__WCHAR_WIDTH__), true);
#endif
#ifdef __WIDL__
    ctx.add_macro_definition("__WIDL__=" XSTRINGIFY(__WIDL__), true);
#endif
#ifdef __WIN32__
    ctx.add_macro_definition("__WIN32__=" XSTRINGIFY(__WIN32__), true);
#endif
#ifdef __WINT_MAX__
    ctx.add_macro_definition(
        "__WINT_MAX__=" XSTRINGIFY(__WINT_MAX__), true);
#endif
#ifdef __WINT_MIN__
    ctx.add_macro_definition(
        "__WINT_MIN__=" XSTRINGIFY(__WINT_MIN__), true);
#endif
#ifdef __WINT_TYPE__
    ctx.add_macro_definition(
        "__WINT_TYPE__=" XSTRINGIFY(__WINT_TYPE__), true);
#endif
#ifdef __WINT_WIDTH__
    ctx.add_macro_definition(
        "__WINT_WIDTH__=" XSTRINGIFY(__WINT_WIDTH__), true);
#endif
#ifdef __aarch64__
    ctx.add_macro_definition("__aarch64__=" XSTRINGIFY(__aarch64__), true);
#endif
#ifdef __alpha__
    ctx.add_macro_definition("__alpha__=" XSTRINGIFY(__alpha__), true);
#endif
#ifdef __amd64
    ctx.add_macro_definition("__amd64=" XSTRINGIFY(__amd64), true);
#endif
#ifdef __amd64__
    ctx.add_macro_definition("__amd64__=" XSTRINGIFY(__amd64__), true);
#endif
#ifdef __arm__
    ctx.add_macro_definition("__arm__=" XSTRINGIFY(__arm__), true);
#endif
#ifdef __clang__
    ctx.add_macro_definition("__clang__=" XSTRINGIFY(__clang__), true);
#endif
#ifdef __clang_major__
    ctx.add_macro_definition(
        "__clang_major__=" XSTRINGIFY(__clang_major__), true);
#endif
#ifdef __clang_minor__
    ctx.add_macro_definition(
        "__clang_minor__=" XSTRINGIFY(__clang_minor__), true);
#endif
#ifdef __clang_patchlevel__
    ctx.add_macro_definition(
        "__clang_patchlevel__=" XSTRINGIFY(__clang_patchlevel__), true);
#endif
#ifdef __clang_version__
    ctx.add_macro_definition(
        "__clang_version__=" XSTRINGIFY(__clang_version__), true);
#endif
#ifdef __gnu_linux__
    ctx.add_macro_definition(
        "__gnu_linux__=" XSTRINGIFY(__gnu_linux__), true);
#endif
#ifdef __i386
    ctx.add_macro_definition("__i386=" XSTRINGIFY(__i386), true);
#endif
#ifdef __i386__
    ctx.add_macro_definition("__i386__=" XSTRINGIFY(__i386__), true);
#endif
#ifdef __ia64__
    ctx.add_macro_definition("__ia64__=" XSTRINGIFY(__ia64__), true);
#endif
#ifdef __k8
    ctx.add_macro_definition("__k8=" XSTRINGIFY(__k8), true);
#endif
#ifdef __k8__
    ctx.add_macro_definition("__k8__=" XSTRINGIFY(__k8__), true);
#endif
#ifdef __linux
    ctx.add_macro_definition("__linux=" XSTRINGIFY(__linux), true);
#endif
#ifdef __linux__
    ctx.add_macro_definition("__linux__=" XSTRINGIFY(__linux__), true);
#endif
#ifdef __llvm__
    ctx.add_macro_definition("__llvm__=" XSTRINGIFY(__llvm__), true);
#endif
#ifdef __midl
    ctx.add_macro_definition("__midl=" XSTRINGIFY(__midl), true);
#endif
#ifdef __mips
    ctx.add_macro_definition("__mips=" XSTRINGIFY(__mips), true);
#endif
#ifdef __mips__
    ctx.add_macro_definition("__mips__=" XSTRINGIFY(__mips__), true);
#endif
#ifdef __ppc__
    ctx.add_macro_definition("__ppc__=" XSTRINGIFY(__ppc__), true);
#endif
#ifdef __sparc__
    ctx.add_macro_definition("__sparc__=" XSTRINGIFY(__sparc__), true);
#endif
#ifdef __tune_k8__
    ctx.add_macro_definition("__tune_k8__=" XSTRINGIFY(__tune_k8__), true);
#endif
#ifdef __unix
    ctx.add_macro_definition("__unix=" XSTRINGIFY(__unix), true);
#endif
#ifdef __unix__
    ctx.add_macro_definition("__unix__=" XSTRINGIFY(__unix__), true);
#endif
#ifdef __x86_64
    ctx.add_macro_definition("__x86_64=" XSTRINGIFY(__x86_64), true);
#endif
#ifdef __x86_64__
    ctx.add_macro_definition("__x86_64__=" XSTRINGIFY(__x86_64__), true);
#endif
#ifdef i386
    ctx.add_macro_definition("i386=" XSTRINGIFY(i386), true);
#endif
#ifdef linux
    ctx.add_macro_definition("linux=" XSTRINGIFY(linux), true);
#endif
#ifdef unix
    ctx.add_macro_definition("unix=" XSTRINGIFY(unix), true);
#endif
}
