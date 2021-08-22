cmake_minimum_required(VERSION 3.17)

project(advent_of_code)
set(CMAKE_CXX_STANDARD 17)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
set(CMAKE_CXX_FLAGS "-Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

add_library(input utils/input.cpp)
add_library(hexgrid utils/hexgrid.cpp)
add_library(md5 includes/md5.cpp)

find_package(GTest)
if (GTEST_FOUND)
  enable_testing()
  include_directories(${GTEST_INCLUDE_DIRS})

  add_executable(HexgridUnitTests tests/HexgridUnitTests.cpp)
  target_link_libraries(HexgridUnitTests ${GTEST_BOTH_LIBRARIES} pthread input hexgrid)
  GTEST_DISCOVER_TESTS(HexgridUnitTests)

  add_executable(InputUnitTests tests/InputUnitTests.cpp)
  target_link_libraries(InputUnitTests ${GTEST_BOTH_LIBRARIES} pthread input)
  GTEST_DISCOVER_TESTS(InputUnitTests)

  add_executable(TreeUnitTests tests/TreeUnitTests.cpp)
  target_link_libraries(TreeUnitTests ${GTEST_BOTH_LIBRARIES} pthread input)
  GTEST_DISCOVER_TESTS(TreeUnitTests)
endif()


find_package(Boost REQUIRED)

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

# add_executable(2015_07 2015/day_07.cpp)
# target_link_libraries(2015_07 input)

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

add_executable(2015_16 2015/day_16.cpp)
target_link_libraries(2015_16 input)

add_executable(2015_17 2015/day_17.cpp)
target_link_libraries(2015_17 input)

add_executable(2015_18 2015/day_18.cpp)
target_link_libraries(2015_18 input)

add_executable(2015_19 2015/day_19.cpp)
target_link_libraries(2015_19 input)

add_executable(2015_20 2015/day_20.cpp)
target_link_libraries(2015_20 input)

add_executable(2015_21 2015/day_21.cpp)
target_link_libraries(2015_21 input)

add_executable(2015_22 2015/day_22.cpp)
target_link_libraries(2015_22 input)

add_executable(2015_23 2015/day_23.cpp)
target_link_libraries(2015_23 input)

add_executable(2015_24 2015/day_24.cpp)
target_link_libraries(2015_24 input)

add_executable(2015_25 2015/day_25.cpp)
target_link_libraries(2015_25 input)

add_executable(2016_01 2016/day_01.cpp)
target_link_libraries(2016_01 input)

add_executable(2016_02 2016/day_02.cpp)
target_link_libraries(2016_02 input)

add_executable(2016_03 2016/day_03.cpp)
target_link_libraries(2016_03 input)

add_executable(2016_04 2016/day_04.cpp)
target_link_libraries(2016_04 input)

add_executable(2016_05 2016/day_05.cpp)
target_link_libraries(2016_05 input md5)

add_executable(2016_06 2016/day_06.cpp)
target_link_libraries(2016_06 input md5)

add_executable(2016_07 2016/day_07.cpp)
target_link_libraries(2016_07 input)

add_executable(2016_08 2016/day_08.cpp)
target_link_libraries(2016_08 input)

add_executable(2016_09 2016/day_09.cpp)
target_link_libraries(2016_09 input)

add_executable(2016_10 2016/day_10.cpp)
target_link_libraries(2016_10 input)

add_executable(2016_11 2016/day_11.cpp)
target_link_libraries(2016_11 input)

add_executable(2016_12 2016/day_12.cpp)
target_link_libraries(2016_12 input)

add_executable(2016_13 2016/day_13.cpp)
target_link_libraries(2016_13 input)

add_executable(2016_14 2016/day_14.cpp)
target_link_libraries(2016_14 input md5)

add_executable(2016_15 2016/day_15.cpp)
target_link_libraries(2016_15 input)

add_executable(2016_16 2016/day_16.cpp)
target_link_libraries(2016_16 input)

add_executable(2016_17 2016/day_17.cpp)
target_link_libraries(2016_17 input md5)

add_executable(2016_18 2016/day_18.cpp)
target_link_libraries(2016_18 input )

add_executable(2016_19 2016/day_19.cpp)
target_link_libraries(2016_19 input )

add_executable(2016_20 2016/day_20.cpp)
target_link_libraries(2016_20 input )

add_executable(2016_21 2016/day_21.cpp)
target_link_libraries(2016_21 input )

add_executable(2016_22 2016/day_22.cpp)
target_link_libraries(2016_22 input )

add_executable(2016_23 2016/day_23.cpp)
target_link_libraries(2016_23 input )

add_executable(2017_01 2017/day_01.cpp)
target_link_libraries(2017_01 input)

add_executable(2017_02 2017/day_02.cpp)
target_link_libraries(2017_02 input)

add_executable(2017_03 2017/day_03.cpp)
target_link_libraries(2017_03 input)

add_executable(2017_04 2017/day_04.cpp)
target_link_libraries(2017_04 input)

add_executable(2017_05 2017/day_05.cpp)
target_link_libraries(2017_05 input)

add_executable(2017_06 2017/day_06.cpp)
target_link_libraries(2017_06 input)

add_executable(2017_07 2017/day_07.cpp)
target_link_libraries(2017_07 input)

add_executable(2017_08 2017/day_08.cpp)
target_link_libraries(2017_08 input)

add_executable(2017_09 2017/day_09.cpp)
target_link_libraries(2017_09 input)

add_executable(2017_10 2017/day_10.cpp)
target_link_libraries(2017_10 input)

add_executable(2017_11 2017/day_11.cpp)
target_link_libraries(2017_11 input hexgrid)

add_executable(2017_12 2017/day_12.cpp)
target_link_libraries(2017_12 input)

add_executable(2017_13 2017/day_13.cpp)
target_link_libraries(2017_13 input)

add_executable(2017_14 2017/day_14.cpp)
target_link_libraries(2017_14 input)

add_executable(2017_15 2017/day_15.cpp)
target_link_libraries(2017_15 input)

add_executable(2017_16 2017/day_16.cpp)
target_link_libraries(2017_16 input)

add_executable(2017_17 2017/day_17.cpp)
target_link_libraries(2017_17 input)

add_executable(2017_18 2017/day_18.cpp)
target_link_libraries(2017_18 input)

add_executable(2017_19 2017/day_19.cpp)
target_link_libraries(2017_19 input)

add_executable(2017_20 2017/day_20.cpp)
target_link_libraries(2017_20 input)

add_executable(2017_21 2017/day_21.cpp)
target_link_libraries(2017_21 input)

add_executable(2018_01 2018/day_01.cpp)
target_link_libraries(2018_01 input)

add_executable(2018_02 2018/day_02.cpp)
target_link_libraries(2018_02 input)

add_executable(2018_03 2018/day_03.cpp)
target_link_libraries(2018_03 input)

add_executable(2018_04 2018/day_04.cpp)
target_link_libraries(2018_04 input)

add_executable(2018_05 2018/day_05.cpp)
target_link_libraries(2018_05 input)

add_executable(2018_06 2018/day_06.cpp)
target_link_libraries(2018_06 input)

add_executable(2018_07 2018/day_07.cpp)
target_link_libraries(2018_07 input)

add_executable(2018_08 2018/day_08.cpp)
target_link_libraries(2018_08 input)

add_executable(2018_09 2018/day_09.cpp)
target_link_libraries(2018_09 input)

add_executable(2018_10 2018/day_10.cpp)
target_link_libraries(2018_10 input)

add_executable(2018_11 2018/day_11.cpp)
target_link_libraries(2018_11 input)

add_executable(2018_12 2018/day_12.cpp)
target_link_libraries(2018_12 input)

add_executable(2018_13 2018/day_13.cpp)
target_link_libraries(2018_13 input)

add_executable(2018_14 2018/day_14.cpp)
target_link_libraries(2018_14 input)

add_executable(2018_15 2018/day_15.cpp)
target_link_libraries(2018_15 input)

add_executable(2018_16 2018/day_16.cpp)
target_link_libraries(2018_16 input)

add_executable(2018_17 2018/day_17.cpp)
target_link_libraries(2018_17 input)

add_executable(2019_01 2019/day_01.cpp)
target_link_libraries(2019_01 input)

add_executable(2019_02 2019/day_02.cpp)
target_link_libraries(2019_02 input)

add_executable(2019_03 2019/day_03.cpp)
target_link_libraries(2019_03 input)

add_executable(2019_04 2019/day_04.cpp)
target_link_libraries(2019_04 input)

add_executable(2019_05 2019/day_05.cpp)
target_link_libraries(2019_05 input)

add_executable(2019_06 2019/day_06.cpp)
target_link_libraries(2019_06 input)

add_executable(2019_07 2019/day_07.cpp)
target_link_libraries(2019_07 input)

add_executable(2019_08 2019/day_08.cpp)
target_link_libraries(2019_08 input)

add_executable(2019_09 2019/day_09.cpp)
target_link_libraries(2019_09 input)

add_executable(2019_10 2019/day_10.cpp)
target_link_libraries(2019_10 input)

add_executable(2019_11 2019/day_11.cpp)
target_link_libraries(2019_11 input)

add_executable(2019_12 2019/day_12.cpp)
target_link_libraries(2019_12 input)

add_executable(2019_13 2019/day_13.cpp)
target_link_libraries(2019_13 input)

add_executable(2019_14 2019/day_14.cpp)
target_link_libraries(2019_14 input)

add_executable(2019_17 2019/day_17.cpp)
target_link_libraries(2019_17 input)

add_executable(2019_22 2019/day_22.cpp)
target_link_libraries(2019_22 input)

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

add_executable(2020_13 2020/day_13.cpp)
target_link_libraries(2020_13 input)

add_executable(2020_14 2020/day_14.cpp)
target_link_libraries(2020_14 input)

add_executable(2020_15 2020/day_15.cpp)
target_link_libraries(2020_15 input)

add_executable(2020_16 2020/day_16.cpp)
target_link_libraries(2020_16 input)

add_executable(2020_17 2020/day_17.cpp)
target_link_libraries(2020_17 input)

# add_executable(2020_18 2020/day_18.cpp)
# target_link_libraries(2020_18 input)

# add_executable(2020_20 2020/day_20.cpp)
# target_link_libraries(2020_20 input)

# add_executable(2020_21 2020/day_21.cpp)
# target_link_libraries(2020_21 input)

add_executable(2020_22 2020/day_22.cpp)
target_link_libraries(2020_22 input)

add_executable(2020_23 2020/day_23.cpp)
target_link_libraries(2020_23 input)

add_executable(2020_24 2020/day_24.cpp)
target_link_libraries(2020_24 input)

add_executable(2020_25 2020/day_25.cpp)
target_link_libraries(2020_25 input)
