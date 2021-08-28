#ifndef WAVE_C_API_HPP
#define WAVE_C_API_HPP

#include "boost_wave_global.hpp"

#ifdef __cplusplus
extern "C" {

#    define STRUCT struct
#    define STRUCT_NAME(x)

#else

#    define STRUCT typedef struct
#    define STRUCT_NAME(x) x

#endif


STRUCT MethodImpl {
    void* impl;
    void* payload;
}
STRUCT_NAME(MethodImpl);

STRUCT WaveProcessingHooks STRUCT_NAME(WaveProcessingHooks);

BOOST_WAVE_EXPORT WaveProcessingHooks* wave_newProcessingHooks();
BOOST_WAVE_EXPORT void                 wave_destroyProcessingHooks(
                    WaveProcessingHooks* hooks);

// struct WaveToken;


STRUCT WaveContext STRUCT_NAME(WaveContext);

BOOST_WAVE_EXPORT WaveContext* wave_newWaveContext(
    const char* instring,
    const char* filename);
BOOST_WAVE_EXPORT void wave_destroyContext(WaveContext* context);

#ifdef __cplusplus
}
#endif

#endif // WAVE_C_API_HPP
