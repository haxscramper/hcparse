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

Set(Boost_INSTALL_DIR ${CONAN_BOOST_ROOT})
find_package(Boost COMPONENTS system filesystem thread wave REQUIRED)

target_link_libraries(
  boost_cwave
  PRIVATE
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_WAVE_LIBRARY}
  ${Boost_THREAD_LIBRARY}
)

target_include_directories(
  boost_cwave
  PRIVATE
  ${Boost_FILESYSTEM_LIBRARY}
  ${Boost_SYSTEM_LIBRARY}
  ${Boost_WAVE_LIBRARY}
  ${Boost_THREAD_LIBRARY}
)

get_target_property(LIBS boost_cwave LINK_LIBRARIES)
message(STATUS "Link libraries")
foreach(DIR ${LIBS})
  message(STATUS "-- " ${DIR})
endforeach()

message(STATUS "Include directories")
get_target_property(DIRS boost_cwave INCLUDE_DIRECTORIES)
foreach(DIR ${DIRS})
  message(STATUS "-- " ${DIR})
endforeach()

target_compile_definitions(boost_cwave PRIVATE BOOST_WAVE_LIBRARY)

set_target_properties(boost_cwave
    PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../../lib"
)
