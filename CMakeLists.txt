cmake_minimum_required(VERSION 3.5)

project(hcparse LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CONAN_DISABLE_CHECK_COMPILER ON)

set(DEPS_DIR ${CMAKE_SOURCE_DIR}/build/dependencies/conan)
list(PREPEND CMAKE_MODULE_PATH ${DEPS_DIR})
include(${DEPS_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

message(WARNING ${CONAN_LIBS})

file(GLOB SRC_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp)
file(GLOB HEADER_FILES ${CMAKE_SOURCE_DIR}/src/*.hpp)
add_executable(hcparse ${SRC_FILES})
target_sources(hcparse PRIVATE ${SRC_FILES} ${HEADER_FILES})

target_link_libraries(hcparse PUBLIC ${CONAN_LIBS})
target_include_directories(hcparse PRIVATE ${CONAN_INCLUDE_DIRS})
target_link_directories(hcparse PRIVATE ${CONAN_LIB_DIRS})

set_target_properties(hcparse PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                         "${CMAKE_SOURCE_DIR}")
