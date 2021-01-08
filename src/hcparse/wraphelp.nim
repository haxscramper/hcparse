func closureToCdecl*[T0, T1](
    cb: proc(a: var T0, b: T1) {.closure.}
  ): proc(a: var T0, b: T1, env: pointer) {.cdecl.} =

  discard


type
  StdInitializerList*[T] {.
    importcpp: "std::initializer_list",
    header: "<initializer_list>"
  .} = object

proc cxxInitList*[T](args: T): StdInitializerList[T]
  {.importcpp: "{@}", varargs, constructor.}
