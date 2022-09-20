cmake_minimum_required(VERSION 3.1)
project(hcparse)
set(CMAKE_CXX_COMPILER clang++)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION} in ${LLVM_INSTALL_PREFIX}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
find_package(Clang REQUIRED CONFIG HINTS
             "${LLVM_INSTALL_PREFIX}/lib/cmake/clang")
message(STATUS "Found Clang in ${CLANG_INSTALL_PREFIX}")

file(GLOB SRC_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp)

add_executable(
  hcparse
  # Regular sources
  ${SRC_FILES}
  # `configure_file` projects
  ${CMAKE_CURRENT_BINARY_DIR}/projectmanager_systemprojects.cpp)

# Add include directories in order for `configure_file`-based `.cpp` files to
# work properly.
target_include_directories(hcparse PRIVATE "${CMAKE_CURRENT_LIST_DIR}/src")

target_link_libraries(
  hcparse
  PRIVATE # I'm targeting only with recent clang versions - they are packaged in
          # a single linkable library.
          clang-cpp LLVM)

install(TARGETS hcparse RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
target_include_directories(hcparse PUBLIC ${CLANG_INCLUDE_DIRS})
set_property(TARGET hcparse PROPERTY CXX_STANDARD 20)

target_compile_options(hcparse PRIVATE "-ferror-limit=1" "-fcoroutines-ts")

# Don't link with libs that overlaps our options
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--as-needed")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -fno-rtti")

# Embed the clang header into the binary:
string(REPLACE "svn" "" LLVM_VERSION "${LLVM_VERSION}")
string(REGEX REPLACE "git.*$" "" LLVM_VERSION "${LLVM_VERSION}")

if(NOT CLANG_BUILTIN_HEADERS_DIR)
  set(CLANG_BUILTIN_HEADERS_DIR
      "${LLVM_LIBRARY_DIR}/clang/${LLVM_VERSION}/include")
endif()

file(GLOB BUILTINS_HEADERS "${CLANG_BUILTIN_HEADERS_DIR}/*.h")
if(NOT BUILTINS_HEADERS)
  message(
    FATAL_ERROR
      "Could not find any clang builtins headers in ${CLANG_BUILTIN_HEADERS_DIR}"
  )
endif()

foreach(BUILTIN_HEADER ${BUILTINS_HEADERS})
  # filter files that are way to big
  if(NOT BUILTIN_HEADER MATCHES
     ".*/(arm_neon.h|altivec.h|vecintrin.h|avx512.*intrin.h)")
    file(READ ${BUILTIN_HEADER} BINARY_DATA)
    string(REPLACE "\\" "\\\\" BINARY_DATA "${BINARY_DATA}")
    string(REPLACE "\"" "\\\"" BINARY_DATA "${BINARY_DATA}")
    string(REPLACE "\n" "\\n\"\n\"" BINARY_DATA "${BINARY_DATA}")
    # workaround the fact that stdint.h includes itself
    string(REPLACE "__CLANG_STDINT_H" "__CLANG_STDINT_H2" BINARY_DATA
                   "${BINARY_DATA}")
    string(REPLACE "${CLANG_BUILTIN_HEADERS_DIR}/" "/builtins/" FN
                   "${BUILTIN_HEADER}")
    set(EMBEDDED_DATA
        "${EMBEDDED_DATA} { \"${FN}\" , \"${BINARY_DATA}\" } , \n")
  endif()
endforeach()

configure_file(src/embedded_includes.hpp.in embedded_includes.hpp)
target_include_directories(hcparse PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
