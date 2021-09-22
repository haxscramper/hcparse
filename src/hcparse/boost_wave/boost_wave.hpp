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

    // FoundWarningDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImpl*,
        const WaveTokenList*>
         found_warning_directive_impl;
    bool found_warning_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&   message);


    // FoundDirectiveCbType
    method_impl<EntryHandling, const WaveContextImpl*, const WaveToken*>
         found_directive_impl;
    bool found_directive(
        WaveContextImpl const& ctx,
        WaveToken const&       token);


    // FoundUnknownDirectiveCbType
    method_impl<
        EntryHandling,
        const WaveContextImpl*,
        const WaveTokenList*,
        WaveTokenList*>
         found_unknown_directive_impl;
    bool found_unknown_directive(
        WaveContextImpl const& ctx,
        WaveTokenList const&   line,
        WaveTokenList&         pending);

    void throw_exception(
        WaveContextImpl const& ctx,
        std::exception const&  e);


    method_impl<bool, const WaveContextImpl*, WaveToken*, bool*>
         may_skip_whitespace_impl;
    bool may_skip_whitespace(
        WaveContextImpl const& ctx,
        WaveToken&             token,
        bool&                  skipped_newline);

    method_impl<
        bool,
        const WaveContextImpl*,
        const WaveToken*,
        const WaveTokenList*,
        bool>
         evaluated_conditional_expression_impl;
    bool evaluated_conditional_expression(
        WaveContextImpl const& ctx,
        WaveToken const&       directive,
        WaveTokenList const&   expression,
        bool                   expression_value);


    method_impl<void, const WaveContextImpl*, const WaveToken*>
         skipped_token_impl;
    void skipped_token(WaveContextImpl const& ctx, WaveToken const& token);

    method_impl<WaveToken, const WaveContextImpl*, const WaveToken*>
              generated_token_impl;
    WaveToken generated_token(
        WaveContextImpl const& ctx,
        WaveToken const&       token);

    method_impl<
        bool,
        CWaveContextImpl const*,
        CWaveToken const*,
        std::vector<WaveToken> const*,
        CWaveTokenList const*,
        CWaveToken const*,
        std::vector<WaveTokenList> const*,
        WaveContextImpl::iterator_type const*,
        WaveContextImpl::iterator_type const*>
        expanding_function_like_macro_impl;

    bool expanding_function_like_macro(
        WaveContextImpl const&                ctx,
        WaveToken const&                      macrodef,
        WaveTokenVector const&                formal_args,
        WaveTokenList const&                  definition,
        WaveToken const&                      macrocall,
        WaveTokenListVector const&            arguments,
        WaveContextImpl::iterator_type const& seqstart,
        WaveContextImpl::iterator_type const& seqend);
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

struct CxxWaveToken {
    WaveToken d;
};

struct CxxWaveTokenList {
    WaveTokenList d;
};

struct CxxWaveIterator {
    WaveIterator d;
};

struct CxxWaveContext {
    WaveContext d;
};

inline CxxWaveContext* toCxx(CWaveContext* context) {
    return (CxxWaveContext*)(context);
}

inline CxxWaveTokenList* toCxx(CWaveTokenList* context) {
    return (CxxWaveTokenList*)(context);
}

inline const CxxWaveTokenList* toCxx(const CWaveTokenList* context) {
    return (CxxWaveTokenList*)(context);
}

inline CxxWaveIterator* toCxx(CWaveIterator* context) {
    return (CxxWaveIterator*)(context);
}

inline const CxxWaveIterator* toCxx(const CWaveIterator* context) {
    return (CxxWaveIterator*)(context);
}

inline CxxWaveToken* toCxx(const CWaveToken* tok) {
    return (CxxWaveToken*)(tok);
}

#endif // BOOST_WAVE_HPP
