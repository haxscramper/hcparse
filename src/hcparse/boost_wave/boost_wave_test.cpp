#include "boost_wave.hpp"

EntryHandling found_warning_directive_impl(
    const CWaveContextImpl* ctx,
    const CWaveTokenList*   message,
    void*                   env) {
    std::cout << "Found warning directive with message [["
              << util::impl::as_string(toCxx(message)->d) << "]]\n";
    return EntryHandlingSkip;
}


int main() {
    const char* text = "test()\n";

    std::string text1(text);

    WaveContextImpl ctx(text1.begin(), text1.end(), "file");

    auto first = ctx.begin();
    auto last  = ctx.end();

    while (first != last) {
        std::cout << (*first).get_value() << std::endl;
        ++first;
    }

    CWaveContext* context = wave_newWaveContext(text, "file");

    //    wave_setFoundWarningDirective(context,
    //    &found_warning_directive_impl);


    CWaveIterator* begin = wave_beginIterator(context);
    CWaveIterator* end   = wave_endIterator(context);

    CxxWaveIterator* beginCxx = toCxx(begin);
    CxxWaveIterator* endCxx   = toCxx(end);

    while (!wave_contextHasError(context) && beginCxx->d != endCxx->d) {
        CWaveToken* tok = wave_iterGetTok(begin);
        std::cout << "token " << wave_tokGetValue(tok) << "\n";
        wave_advanceIterator(begin);

        wave_deleteTok(tok);
    }

    if (wave_contextHasError(context)) {
        std::cout << "Error encountered during parsing\n";
    }

    if (wave_contextHasWarnings(context)) {
        std::cout << "Warning encountered during parsing\n";
        auto warn = wave_contextPopWarning(context);
        std::cout << "> " << warn.errorText << " in " << warn.filename
                  << "\n";
    }

    std::cout << "Done\n";
}
