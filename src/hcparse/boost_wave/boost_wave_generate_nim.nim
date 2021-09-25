import
  compiler/[ast, renderer],
  ".."/[hc_parsefront, hc_codegen, hc_impls],
  ../interop_ir/wrap_store,
  hmisc/other/oswrap

let code = expandViaCc(cwd() /. "wave_c_api.h", baseCParseConf)

var conf = cCodegenConf

echo code.
  wrapViaTs(false, cxxDynlib("libbost_wave.so"), dropPrefix("wave")).
  toString(conf)
