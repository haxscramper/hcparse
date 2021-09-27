#include "boost_wave.hpp"

EntryHandling found_warning_directive_impl(
    const WaveContextImplHandle* ctx,
    const WaveTokenListHandle*   message,
    void*                        env) {
    std::cout << "Found warning directive with message [["
              << util::impl::as_string(*toCxx(message)) << "]]\n";
    return EntryHandlingSkip;
}


int main() {
    const char*        text    = "test()\n";
    WaveContextHandle* context = wave_newWaveContext(text, "file");

    //    wave_setFoundWarningDirective(context,
    //    &found_warning_directive_impl, NULL);


    WaveIteratorHandle* begin = wave_beginIterator(context);
    WaveIteratorHandle* end   = wave_endIterator(context);

    while (!wave_contextHasError(context)
           && wave_neqIterator(begin, end)) {
        WaveTokenHandle* tok = wave_iterGetTok(begin);
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
