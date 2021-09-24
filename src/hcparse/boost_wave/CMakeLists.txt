cmake_minimum_required(VERSION 3.14)
project(wave CXX C)

project(boost_wave LANGUAGES CXX)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../")

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(NIM REQUIRED)
find_package(Boost COMPONENTS system filesystem thread wave REQUIRED)

add_library(boost_wave SHARED
  boost_wave_global.hpp
  boost_wave.cpp
  boost_wave.hpp
  wave_c_api.h
)

add_executable(boost_wave_test
  boost_wave_test.cpp
  boost_wave.cpp
  boost_wave.hpp
  scratch.cpp
)


set(NIM_OUT_DIR "${CMAKE_BINARY_DIR}/nim_source_dir")

execute_process(
  COMMAND nim compile --noLinking --nimcache:${NIM_OUT_DIR} "boost_wave_test.nim"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
)


file(GLOB NIM_OUT_SOURCES "${NIM_OUT_DIR}/*.c")
add_executable(boost_wave_nim ${NIM_OUT_SOURCES})
target_sources(boost_wave_nim PUBLIC ${NIM_OUT_SOURCES})
target_include_directories(boost_wave_nim PUBLIC ${NIM_INCLUDE_DIRS})


target_link_libraries(boost_wave
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_WAVE_LIBRARY}
  ${Boost_THREAD_LIBRARY}
)

target_link_libraries(boost_wave_test
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_WAVE_LIBRARY}
  ${Boost_THREAD_LIBRARY}
)


target_compile_definitions(boost_wave PRIVATE BOOST_WAVE_LIBRARY)

set_target_properties(boost_wave
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
)
