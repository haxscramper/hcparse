#ifndef BOOST_WAVE_HPP
#define BOOST_WAVE_HPP

#include "boost_wave_global.hpp"
#include "wave_c_api.h"

#include <boost/wave/cpp_context.hpp>
#include <boost/wave/cpplexer/cpp_lex_iterator.hpp>
#include <boost/wave/cpplexer/cpp_lex_token.hpp>
#include <boost/wave/preprocessing_hooks.hpp>
#include <boost/wave/util/file_position.hpp>

#include <iostream>
#include <memory>
#include <queue>
#include <string>

template <std::size_t ExpectedSize, std::size_t RealSize>
void check_size() {
    static_assert(
        ExpectedSize == RealSize,
        "Cannot nuclear cast two objects of different size");
}

template <typename Out, typename In>
Out nuclear_cast(In in) {
    check_size<sizeof(In), sizeof(Out)>();
    Out result;

    char(*resPtr)[sizeof(In)] = (char(*)[sizeof(In)])(&result);
    char(*inPtr)[sizeof(Out)] = (char(*)[sizeof(Out)])(&in);
    for (size_t i = 0; i < sizeof(Out); ++i) {
        (*resPtr)[i] = (*inPtr)[i];
    }

    //    std::memcpy(&result, &in, sizeof(Out));

    //    In* tmp = (In*)(&result);


    return result;
}


inline char* copyalloc(const char* in) {
    int   len = strlen(in);
    char* res = (char*)std::malloc(sizeof(char) * (len + 1));
    std::memcpy(res, in, len);
    return res;
}

template <typename ResultT, typename... Arguments>
struct method_impl {
    ResultT (*impl)(Arguments..., void* env);
    void* env;

    method_impl() {
        env  = nullptr;
        impl = nullptr;
    };

    method_impl(
        ResultT (*_impl)(Arguments..., void* env),
        void* _env = nullptr)
        : impl(_impl), env(_env) {}

    ResultT operator()(Arguments... arguments) {
        return (*impl)(arguments..., env);
    }

    inline bool isActive() { return impl != nullptr; }
};

using namespace boost::wave;

struct WaveHooksImpl;

using WaveToken = cpplexer::lex_token<>;

using WaveTokenList = std::list<
    WaveToken,
    boost::fast_pool_allocator<
        cpplexer::lex_token<>,
        boost::default_user_allocator_new_delete>>;

using WaveTokenVector     = std::vector<WaveToken>;
using WaveTokenListVector = std::vector<WaveTokenList>;

using WaveContextImpl = context<
    std::string::iterator,
    cpplexer::lex_iterator<WaveToken>,
    iteration_context_policies::load_file_to_string,
    WaveHooksImpl>;

struct WaveContext;

struct WaveHooksImpl
    : public context_policies::default_preprocessing_hooks {

    WaveContext* context = nullptr;


    // FoundDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const WaveToken*>
         found_directive_impl;
    bool found_directive(
        WaveContextImpl const& ctx,
        WaveToken const&       token);


    // FoundUnknownDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const WaveTokenListHandle*,
        WaveTokenListHandle*>
         found_unknown_directive_impl;
    bool found_unknown_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&   line,
        WaveTokenList&         pending);

    void throw_exception(
        WaveContextImpl const& ctx,
        std::exception const&  e);


    method_impl<bool, const WaveContextImplHandle*, WaveToken*, bool*>
         may_skip_whitespace_impl;
    bool may_skip_whitespace(
        WaveContextImpl const& ctx,
        WaveToken&             token,
        bool&                  skipped_newline);

    method_impl<
        bool,
        const WaveContextImplHandle*,
        const WaveTokenHandle*,
        const WaveTokenListHandle*,
        bool>
         evaluated_conditional_expression_impl;
    bool evaluated_conditional_expression(
        WaveContextImpl const& ctx,
        WaveToken const&       directive,
        WaveTokenList const&   expression,
        bool                   expression_value);


    method_impl<void, const WaveContextImplHandle*, const WaveTokenHandle*>
         skipped_token_impl;
    void skipped_token(WaveContextImpl const& ctx, WaveToken const& token);

    method_impl<
        void,
        const WaveContextImplHandle*,
        const WaveTokenHandle*,
        WaveTokenHandle*>
                     generated_token_impl;
    WaveToken const& generated_token(
        WaveContextImpl const& ctx,
        WaveToken const&       token);


    method_impl<bool, void*> expanding_function_like_macro_impl;

    template <typename Iterator>
    bool expanding_function_like_macro(
        WaveContextImpl const&     ctx,
        WaveToken const&           macrodef,
        WaveTokenVector const&     formal_args,
        WaveTokenList const&       definition,
        WaveToken const&           macrocall,
        WaveTokenListVector const& arguments,
        Iterator const&            seqstart,
        Iterator const&            seqend);

    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const WaveTokenHandle*,
        const WaveTokenListHandle*,
        const WaveTokenHandle*>
         expanding_object_like_macro_impl;
    bool expanding_object_like_macro(
        WaveContextImpl const& ctx,
        WaveToken const&       macro,
        WaveTokenList const&   definition,
        WaveToken const&       macrocall);

    method_impl<
        void,
        const WaveContextImplHandle*,
        const WaveTokenListHandle*>
         expanded_macro_impl;
    void expanded_macro(
        WaveContextImpl const& ctx,
        WaveTokenList const&   result);

    method_impl<
        void,
        const WaveContextImplHandle*,
        const WaveTokenListHandle*>
         rescanned_macro_impl;
    void rescanned_macro(
        WaveContextImpl const& ctx,
        WaveTokenList const&   result);

    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const char*,
        bool>
         found_include_directive_impl;
    bool found_include_directive(
        WaveContextImpl const& ctx,
        std::string const&     filename,
        bool                   include_next);

    method_impl<
        EntryHandling,
        WaveContextImplHandle*,
        char*,
        bool,
        char const*,
        char*,
        char*>
         locate_include_file_impl;
    bool locate_include_file(
        WaveContextImpl& ctx,
        std::string&     file_path,
        bool             is_system,
        char const*      current_name,
        std::string&     dir_path,
        std::string&     native_name);

    method_impl<
        void,
        const WaveContextImplHandle*,
        const char*,
        const char*,
        bool>
         opened_include_file_impl;
    void opened_include_file(
        WaveContextImpl const& ctx,
        std::string const&     rel_filename,
        std::string const&     abs_filename,
        bool                   is_system_include);


    method_impl<void, const WaveContextImplHandle*>
         returning_from_include_file_impl;
    void returning_from_include_file(WaveContextImpl const& ctx);

    method_impl<
        void,
        const WaveContextImplHandle*,
        const char*,
        const char*>
         detected_include_guard_impl;
    void detected_include_guard(
        WaveContextImpl const& ctx,
        std::string const&     filename,
        std::string const&     include_guard);

    method_impl<
        void,
        const WaveContextImplHandle*,
        const WaveTokenHandle*,
        const char*>
         detected_pragma_once_impl;
    void detected_pragma_once(
        WaveContextImpl const& ctx,
        WaveToken const&       pragma_token,
        std::string const&     filename);

    method_impl<
        bool,
        const WaveContextImplHandle*,
        WaveTokenListHandle*,
        const WaveTokenHandle*,
        const WaveTokenListHandle*,
        const WaveTokenHandle*>
         interpret_pragma_impl;
    bool interpret_pragma(
        WaveContextImpl const& ctx,
        WaveTokenList&         pending,
        WaveToken const&       option,
        WaveTokenList const&   values,
        WaveToken const&       pragma_token);

    //    template <
    //        typename ContextT,
    //        typename TokenT,
    //        typename ParametersT,
    //        typename DefinitionT>
    //    void defined_macro(
    //        ContextT const&    ctx,
    //        TokenT const&      name,
    //        bool               is_functionlike,
    //        ParametersT const& parameters,
    //        DefinitionT const& definition,
    //        bool               is_predefined);


    method_impl<void, const WaveContextImplHandle*, const WaveTokenHandle*>
         undefined_macro_impl;
    void undefined_macro(
        WaveContextImpl const& ctx,
        WaveToken const&       name);


    // FoundWarningDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const WaveTokenListHandle*>
         found_error_directive_impl;
    bool found_error_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&   message);

    // FoundWarningDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const WaveTokenListHandle*,
        unsigned int,
        const char*>
         found_line_directive_impl;
    void found_line_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&   arguments,
        unsigned int           line,
        std::string const&     filename);

    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        WaveTokenListHandle*,
        const WaveTokenHandle*>
         emit_line_directive_impl;
    bool emit_line_directive(
        WaveContextImpl const& ctx,
        WaveTokenList&         pending,
        WaveToken const&       act_token);


    // FoundWarningDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImplHandle*,
        const WaveTokenListHandle*>
         found_warning_directive_impl;
    bool found_warning_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&   message);
};


using WaveIterator = WaveContextImpl::iterator_type;

struct WaveContext {
    std::unique_ptr<WaveContextImpl> context;
    std::string                      text;
    util::file_position_type         current_position;
    void*                            contextData = nullptr;
    bool                             hasError    = false;
    std::queue<WaveDiagnostics>      diagnostics;

    WaveContext(std::string _text, const char* filename);
    void processAll();
};

// DECL_STRUCT(CWaveProcessingHooks);
// DECL_STRUCT(CWaveContext);
// DECL_STRUCT(CWaveIterator);
// DECL_STRUCT(CWaveToken);
// DECL_STRUCT(CWaveIterator);
// DECL_STRUCT(CWaveToken);
// DECL_STRUCT(CWaveContextImpl);

struct CxxWaveIterator {
    WaveIterator d;
};


inline WaveContext* toCxx(WaveContextHandle* context) {
    return (WaveContext*)(context);
}

inline WaveTokenList* toCxx(WaveTokenListHandle* context) {
    return (WaveTokenList*)(context);
}

inline const WaveTokenList* toCxx(const WaveTokenListHandle* context) {
    return (WaveTokenList*)(context);
}

inline const WaveTokenVector* toCxx(const WaveTokenVectorHandle* context) {
    return (WaveTokenVector*)(context);
}

inline CxxWaveIterator* toCxx(WaveIteratorHandle* context) {
    return (CxxWaveIterator*)(context);
}

inline const CxxWaveIterator* toCxx(const WaveIteratorHandle* context) {
    return (CxxWaveIterator*)(context);
}

inline WaveToken* toCxx(const WaveTokenHandle* tok) {
    return (WaveToken*)(tok);
}

template <typename Iterator>
bool WaveHooksImpl::expanding_function_like_macro(
    WaveContextImpl const&     ctx,
    WaveToken const&           macrodef,
    WaveTokenVector const&     formal_args,
    WaveTokenList const&       definition,
    WaveToken const&           macrocall,
    WaveTokenListVector const& arguments,
    const Iterator&            seqstart,
    const Iterator&            seqend) {


    if (expanding_function_like_macro_impl.isActive()) {
        method_impl<
            bool,
            WaveContextImpl const*,
            WaveToken const*,
            const WaveTokenVectorHandle*,
            const WaveTokenList*,
            const WaveToken*,
            WaveTokenVectorHandle const*,
            void*,
            void*>
            real_impl;

        real_impl.impl = (decltype(real_impl.impl))
                             expanding_function_like_macro_impl.impl;
        real_impl.env = expanding_function_like_macro_impl.env;

        return real_impl(
            &ctx,                                       // ctx,
            &macrodef,                                  // macrodef,
            (const WaveTokenVectorHandle*)&formal_args, // formal_args,
            &definition,                                // definition,
            &macrocall,                                 // macrocall,
            (const WaveTokenVectorHandle*)&arguments,   // arguments,
            (void*)&seqstart,                           // seqstart,
            (void*)&seqstart                            // seqend
        );
    } else {
        return default_preprocessing_hooks::expanding_function_like_macro(
            ctx,
            macrodef,
            formal_args,
            definition,
            macrocall,
            arguments,
            seqstart,
            seqend);
    }
}

#endif // BOOST_WAVE_HPP
