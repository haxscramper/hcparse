cmake_minimum_required(VERSION 3.5)

set(B hcparse)
project(${B} LANGUAGES CXX C)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CONAN_DISABLE_CHECK_COMPILER ON)
set(CMAKE_CXX_COMPILER clang++)

foreach(dir ${CMAKE_MODULE_PATH})
  message(${dir})
endforeach()

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

add_definitions(${LLVM_DEFINITIONS})
add_definitions(${CLANG_DEFINITIONS})

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set(DEPS_DIR ${CMAKE_SOURCE_DIR}/build/dependencies/conan)
list(PREPEND CMAKE_MODULE_PATH ${DEPS_DIR})
include(${DEPS_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

file(GLOB SRC_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp)
file(GLOB HEADER_FILES ${CMAKE_SOURCE_DIR}/src/*.hpp)
add_executable(${B} ${SRC_FILES})
target_sources(${B} PRIVATE ${SRC_FILES} ${HEADER_FILES})
target_include_directories(${B} PUBLIC external/hcxx_common/src)
target_compile_options(${B} PUBLIC -ferror-limit=2 -fcoroutines-ts)

target_link_libraries(${B} PUBLIC LLVM clang-cpp ${CONAN_LIBS})
target_include_directories(${B} PRIVATE ${CONAN_INCLUDE_DIRS})
target_link_directories(${B} PRIVATE ${CONAN_LIB_DIRS})
