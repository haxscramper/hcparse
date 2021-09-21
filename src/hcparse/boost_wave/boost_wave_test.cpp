#include "boost_wave.hpp"

EntryHandling found_warning_directive_impl(
    const CWaveContextImpl* ctx,
    const CWaveTokenList*   message,
    void*                   env) {
    std::cout << "Found warning directive with message [["
              << util::impl::as_string(*toCxx(message)) << "]]\n";
    return EntryHandlingSkip;
}


int main() {
    const char* text
        = "#pragma once\n"
          "#warning \"asdfasdf\"\n"
          "#warning \"zzzz\"\n";

    CWaveContext* context = wave_newWaveContext(text, "file");

    wave_setFoundWarningDirective(context, &found_warning_directive_impl);

    std::string text1(text);

    WaveContext ctx(text1, "file");


    WaveIterator beginInit = ctx.context->begin();
    WaveIterator endInit   = ctx.context->end();


    try {
        while (beginInit != endInit) {
            ++beginInit;
        }
    } catch (cpplexer::lexing_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;
        return 1;

    } catch (cpp_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;
        return 2;
    }
}
