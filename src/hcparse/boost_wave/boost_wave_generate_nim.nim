import
  compiler/[ast, renderer],
  ".."/[hc_parsefront, hc_codegen, hc_impls],
  ../interop_ir/wrap_store,
  hmisc/other/oswrap

let code = expandViaCc(cwd() /. "wave_c_api.h", baseCParseConf)

echo toNumerated(code, 300 .. 320)

var conf = cCodegenConf

echo code.
  wrapViaTs(false, cxxHeader("wave_c_api.h"), dropPrefix("wave")).
  toString(conf)
