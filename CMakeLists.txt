cmake_minimum_required(VERSION 3.5)

project(hcparse LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CONAN_DISABLE_CHECK_COMPILER ON)

include(${CMAKE_SOURCE_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

add_executable(hcparse main.cpp)
target_link_libraries(hcparse ${CONAN_LIBS})
target_include_directories(hcparse PRIVATE ${CONAN_INCLUDE_DIRS})
target_link_directories(hcparse PRIVATE ${CONAN_LIB_DIRS})

set_target_properties(hcparse PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                         "${CMAKE_SOURCE_DIR}")
