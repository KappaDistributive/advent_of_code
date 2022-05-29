cmake_minimum_required(VERSION 3.21)

project(advent_of_code)
set(CMAKE_C_STANDARD 17)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
set(CMAKE_C_FLAGS "-Wall -Wextra")

add_executable(2021_01 src/2021/day_01.c)
