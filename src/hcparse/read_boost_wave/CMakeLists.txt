cmake_minimum_required(VERSION 3.14)
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


include(${CMAKE_CURRENT_SOURCE_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)


target_link_libraries(boost_cwave PRIVATE ${CONAN_LINK_LIBRARIES})
target_include_directories(boost_cwave PRIVATE ${CONAN_LIBS})
message(STATUS ${CONAN_LIBS})

target_compile_definitions(boost_cwave PRIVATE BOOST_WAVE_LIBRARY)

set_target_properties(boost_cwave
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
)
