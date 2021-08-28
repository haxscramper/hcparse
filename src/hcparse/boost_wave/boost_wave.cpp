#include "boost_wave.hpp"
#include "wave_c_api.h"

#include <iostream>
#include <string>

#include <boost/wave/cpp_context.hpp>
#include <boost/wave/cpplexer/cpp_lex_iterator.hpp>
#include <boost/wave/cpplexer/cpp_lex_token.hpp>
#include <boost/wave/preprocessing_hooks.hpp>
#include <boost/wave/util/file_position.hpp>

using namespace boost::wave;

struct WaveProcessingHooks
    : context_policies::default_preprocessing_hooks {
    WaveProcessingHooks() {
    }
};

WaveProcessingHooks* wave_newProcessingHooks() {
    return new WaveProcessingHooks();
}

void wave_destroyProcessingHooks(WaveProcessingHooks* hooks) {
    delete hooks;
}

// using position_type = util::file_position<std::string>;
typedef boost::wave::cpplexer::lex_token<>              token_type;
typedef boost::wave::cpplexer::lex_iterator<token_type> lex_iterator_type;


using context_type = context<
    std::string::iterator,
    lex_iterator_type,
    iteration_context_policies::load_file_to_string,
    WaveProcessingHooks>;

struct WaveContext {
    context_type context;

    WaveContext(
        std::string::iterator begin,
        std::string::iterator end,
        const char*           filename)
        : context(begin, end, filename) {
    }
};

WaveContext* wave_newWaveContext(
    const char* instring,
    const char* filename) {
    std::string str(instring);

    std::cout << "Constructed new wave context\n";
    return new WaveContext(str.begin(), str.end(), filename);
}

void wave_destroyContext(WaveContext* context) {
    delete context;
}


struct ContextLexIterator {
    context_type::iterator_type iter;
};
