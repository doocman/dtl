

cmake_minimum_required(VERSION 3.27)
project(dtl)
if(PROJECT_IS_TOP_LEVEL)
    set(CMAKE_CXX_STANDARD 23)
endif()

add_library(dtl INTERFACE)
add_library(dtl::dtl ALIAS dtl)
target_include_directories(dtl INTERFACE inc)

add_executable(LazyFormatAndOstreamAdaptor examples/lazy_format_and_ostream_adaptor.cpp)
target_link_libraries(LazyFormatAndOstreamAdaptor PRIVATE dtl::dtl)
