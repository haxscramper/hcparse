cmake_minimum_required(VERSION 3.14)
project(wave CXX C)

project(boost_wave LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(${CMAKE_CURRENT_SOURCE_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

add_library(boost_cwave SHARED
  boost_wave_global.hpp
  boost_wave.cpp
  boost_wave.hpp
  wave_c_api.h
)

target_link_libraries(boost_cwave PRIVATE
  boost_wave
  boost_system
  boost_thread
  boost_filesystem
)

target_compile_definitions(boost_cwave PRIVATE BOOST_WAVE_LIBRARY)

set_target_properties(boost_cwave
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
)
