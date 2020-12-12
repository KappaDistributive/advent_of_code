cmake_minimum_required(VERSION 3.19)

project(advent_of_code)
set(CMAKE_CXX_STANDARD 17)

add_library(input utils/input.cpp)
add_library(md5 includes/md5.cpp)

add_executable(2015_01 2015/day_01.cpp)
target_link_libraries(2015_01 input)

add_executable(2015_02 2015/day_02.cpp)
target_link_libraries(2015_02 input)

add_executable(2015_03 2015/day_03.cpp)
target_link_libraries(2015_03 input)

add_executable(2015_04 2015/day_04.cpp)
target_link_libraries(2015_04 input md5)

add_executable(2015_05 2015/day_05.cpp)
target_link_libraries(2015_05 input)

add_executable(2015_06 2015/day_06.cpp)
target_link_libraries(2015_06 input)

add_executable(2015_07 2015/day_07.cpp)
target_link_libraries(2015_07 input)

add_executable(2015_08 2015/day_08.cpp)
target_link_libraries(2015_08 input)

add_executable(2015_09 2015/day_09.cpp)
target_link_libraries(2015_09 input)

add_executable(2015_10 2015/day_10.cpp)
target_link_libraries(2015_10 input)

add_executable(2015_11 2015/day_11.cpp)
target_link_libraries(2015_11 input)

add_executable(2015_12 2015/day_12.cpp)
target_link_libraries(2015_12 input)

add_executable(2015_13 2015/day_13.cpp)
target_link_libraries(2015_13 input)

add_executable(2015_14 2015/day_14.cpp)
target_link_libraries(2015_14 input)

add_executable(2015_15 2015/day_15.cpp)
target_link_libraries(2015_15 input)

add_executable(2020_01 2020/day_01.cpp)
target_link_libraries(2020_01 input)

add_executable(2020_02 2020/day_02.cpp)
target_link_libraries(2020_02 input)

add_executable(2020_03 2020/day_03.cpp)
target_link_libraries(2020_03 input)

add_executable(2020_04 2020/day_04.cpp)
target_link_libraries(2020_04 input)

add_executable(2020_05 2020/day_05.cpp)
target_link_libraries(2020_05 input)

add_executable(2020_06 2020/day_06.cpp)
target_link_libraries(2020_06 input)

add_executable(2020_07 2020/day_07.cpp)
target_link_libraries(2020_07 input)

add_executable(2020_08 2020/day_08.cpp)
target_link_libraries(2020_08 input)

add_executable(2020_09 2020/day_09.cpp)
target_link_libraries(2020_09 input)

add_executable(2020_10 2020/day_10.cpp)
target_link_libraries(2020_10 input)

add_executable(2020_11 2020/day_11.cpp)
target_link_libraries(2020_11 input)

add_executable(2020_12 2020/day_12.cpp)
target_link_libraries(2020_12 input)
