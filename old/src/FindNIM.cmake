execute_process(
  COMMAND nim --version
  OUTPUT_VARIABLE nim_version_out
  )

string(REGEX MATCH "Version ([0-9]+\\.[0-9]+\\.[0-9]+)" _ ${nim_version_out})
set(nim_version ${CMAKE_MATCH_1})

message(STATUS "Using nim version " ${nim_version})

set(NIM_INCLUDE_DIRS "$ENV{HOME}/.choosenim/toolchains/nim-${nim_version}/lib")

message(DEBUG "Nim base include dirs" ${NIM_INCLUDE_DIRS})
