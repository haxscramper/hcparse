cmake_minimum_required(VERSION 3.14)

set(QT_CREATOR_SKIP_CONAN_SETUP ON)
set(CMAKE_VERBOSE_MAKEFILE ON)

project(wave CXX C)

project(boost_wave LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

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

#include(${CMAKE_CURRENT_SOURCE_DIR}/conanbuildinfo.cmake)
#conan_basic_setup(TARGETS)
#include(${CMAKE_CURRENT_SOURCE_DIR}/conan_paths.cmake)

find_package(Boost COMPONENTS system filesystem thread wave REQUIRED)

target_link_libraries(boost_cwave
  Boost::system
  Boost::filesystem
  Boost::wave
  Boost::thread
)

target_link_libraries(boost_wave_test
  Boost::system
  Boost::filesystem
  Boost::wave
  Boost::thread
)

target_compile_definitions(boost_cwave PRIVATE BOOST_WAVE_LIBRARY)

set_target_properties(boost_cwave
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
)
