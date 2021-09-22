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
    const char* text = "#pragma once\n";

    CWaveContext* context = wave_newWaveContext(text, "file");

    wave_setFoundWarningDirective(context, &found_warning_directive_impl);

    WaveIterator beginInit = toCxx(context)->context->begin();
    WaveIterator endInit   = toCxx(context)->context->end();

    CWaveIterator beginCopy = nuclear_cast<CWaveIterator>(beginInit);
    CWaveIterator endCopy   = nuclear_cast<CWaveIterator>(endInit);

    WaveIterator* beginPtr = (WaveIterator*)(&beginCopy);
    WaveIterator* endPtr   = (WaveIterator*)(&endCopy);

    CWaveIterator beginC = wave_beginIterator(context);
    CWaveIterator endC   = wave_endIterator(context);

    if (beginInit != endInit) {
        ++beginInit;
    }

    if (*beginPtr != *endPtr) {
        ++*beginPtr;
    }

    while (wave_neqIterator(&beginC, &endC)) {
        std::cout << "token" << std::endl;
        wave_advanceIterator(&beginC);
    }

    std::cout << "Done\n";
}
