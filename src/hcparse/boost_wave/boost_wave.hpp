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
#include <string>

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

using WaveTokenT = cpplexer::lex_token<>;

using WaveTokenList = std::list<
    WaveTokenT,
    boost::fast_pool_allocator<
        cpplexer::lex_token<>,
        boost::default_user_allocator_new_delete>>;

using WaveContextImpl = context<
    std::string::iterator,
    cpplexer::lex_iterator<WaveTokenT>,
    iteration_context_policies::load_file_to_string,
    WaveHooksImpl>;

using FoundWarningDirectiveCbType = method_impl<
    EntryHandling,
    const WaveContextImpl*,
    const WaveTokenList*>;

struct WaveHooksImpl
    : public context_policies::default_preprocessing_hooks {
    FoundWarningDirectiveCbType found_warning_directive_impl;
    inline bool                 found_warning_directive(
                        WaveContextImpl const& ctx,
                        WaveTokenList const&   message);
};


struct WaveContext {
    WaveContextImpl          context;
    std::string              text;
    util::file_position_type current_position;

    void set_found_warning_directive_impl(
        FoundWarningDirectiveCbType impl);


    WaveContext(std::string&& _text, const char* filename);
    void processAll();
};


inline WaveContext* toCxx(CWaveContext* context) {
    return (WaveContext*)(context);
}

inline const WaveTokenList* toCxx(const CWaveTokenList* context) {
    return (WaveTokenList*)(context);
}

#endif // BOOST_WAVE_HPP
