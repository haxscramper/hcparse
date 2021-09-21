#include "boost_wave.hpp"

EntryHandling found_warning_directive_impl(
    context_type const& ctx,
    list_type const&    message,
    void*               env) {
    std::cout << "Found warning directive with message [["
              << util::impl::as_string(message) << "]]\n";
    return EntryHandlingSkip;
}


int main() {
    WaveContext* context = wave_newWaveContext(
        "#pragma once\n"
        "#warning \"asdfasdf\"\n"
        "#warning \"zzzz\"\n",
        "file");

    context->set_found_warning_directive_impl(
        found_warning_directive_impl);


    try {
        context->processAll();
    } catch (cpplexer::lexing_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;

    } catch (cpp_exception const& e) {
        std::cerr << e.file_name() << "(" << e.line_no()
                  << "): " << e.description() << std::endl;
        return 2;
    }
}
