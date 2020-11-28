cmake_minimum_required(VERSION 3.19)

project(advent_of_code)
set(CMAKE_CXX_STANDARD 17)

add_library(input utils/input.cpp)
add_executable(2015_01 2015/day_01.cpp)
target_link_libraries(2015_01 input)

add_executable(2015_02 2015/day_02.cpp)
target_link_libraries(2015_02 input)

add_executable(2015_03 2015/day_03.cpp)
target_link_libraries(2015_03 input)