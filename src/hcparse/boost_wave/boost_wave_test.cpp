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
    CWaveContext* context = wave_newWaveContext(
        "#pragma once\n"
        "#warning \"asdfasdf\"\n"
        "#warning \"zzzz\"\n",
        "file");

    wave_setFoundWarningDirective(context, &found_warning_directive_impl);


    try {
        wave_processAll(context);
    } catch (cpplexer::lexing_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;

    } catch (cpp_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;
        return 2;
    }
}
