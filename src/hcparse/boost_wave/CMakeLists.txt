cmake_minimum_required(VERSION 3.14)
project(wave CXX C)

project(boost_wave LANGUAGES CXX)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../")

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(NIM REQUIRED)
find_package(Boost COMPONENTS system filesystem thread wave REQUIRED)

add_library(boost_cwave SHARED
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

target_link_libraries(boost_cwave
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

file(GLOB NIM_IN_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/*.nim")

#add_custom_target(
#    boost_wave_nim ALL
#    COMMAND
#        nim c
#            --nimcache:"${CMAKE_BINARY_DIR}/nim_source_dir"
#            --out:"${CMAKE_CURRENT_SOURCE_DIR}/boost_wave_nim"
#            "boost_wave_test.nim"

#    SOURCES ${NIM_IN_SOURCES}
#    COMMENT "Build nim test file"
#    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
#)



target_compile_definitions(boost_cwave PRIVATE BOOST_WAVE_LIBRARY)

set_target_properties(boost_cwave
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
)

#add_dependencies(boost_wave_nim boost_cwave)
