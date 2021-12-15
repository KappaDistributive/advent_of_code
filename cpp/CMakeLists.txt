cmake_minimum_required(VERSION 3.17)

project(advent_of_code)
set(CMAKE_CXX_STANDARD 20)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=stdlibc++")

add_subdirectory(vendors/fmt EXCLUDE_FROM_ALL)

add_library(input src/utils/input.cpp)
target_link_libraries(input fmt)

add_library(hexgrid src/utils/hexgrid.cpp)

add_library(md5 includes/md5.cpp)

find_package(GTest)
if (GTEST_FOUND)
  enable_testing()
  include_directories(${GTEST_INCLUDE_DIRS})

  add_executable(GeometryUnitTests tests/GeometryUnitTests.cpp)
  target_link_libraries(GeometryUnitTests ${GTEST_BOTH_LIBRARIES} pthread input)
  GTEST_DISCOVER_TESTS(GeometryUnitTests)

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

add_executable(2015_01 src/2015/day_01.cpp)
target_link_libraries(2015_01 input)

add_executable(2015_02 src/2015/day_02.cpp)
target_link_libraries(2015_02 input)

add_executable(2015_03 src/2015/day_03.cpp)
target_link_libraries(2015_03 input)

add_executable(2015_04 src/2015/day_04.cpp)
target_link_libraries(2015_04 input md5)

add_executable(2015_05 src/2015/day_05.cpp)
target_link_libraries(2015_05 input)

add_executable(2015_06 src/2015/day_06.cpp)
target_link_libraries(2015_06 input)

# add_executable(2015_07 src/2015/day_07.cpp)
# target_link_libraries(2015_07 input)

add_executable(2015_08 src/2015/day_08.cpp)
target_link_libraries(2015_08 input)

add_executable(2015_09 src/2015/day_09.cpp)
target_link_libraries(2015_09 input)

add_executable(2015_10 src/2015/day_10.cpp)
target_link_libraries(2015_10 input)

add_executable(2015_11 src/2015/day_11.cpp)
target_link_libraries(2015_11 input)

add_executable(2015_12 src/2015/day_12.cpp)
target_link_libraries(2015_12 input)

add_executable(2015_13 src/2015/day_13.cpp)
target_link_libraries(2015_13 input)

add_executable(2015_14 src/2015/day_14.cpp)
target_link_libraries(2015_14 input)

add_executable(2015_15 src/2015/day_15.cpp)
target_link_libraries(2015_15 input)

add_executable(2015_16 src/2015/day_16.cpp)
target_link_libraries(2015_16 input)

add_executable(2015_17 src/2015/day_17.cpp)
target_link_libraries(2015_17 input)

add_executable(2015_18 src/2015/day_18.cpp)
target_link_libraries(2015_18 input)

add_executable(2015_19 src/2015/day_19.cpp)
target_link_libraries(2015_19 input)

add_executable(2015_20 src/2015/day_20.cpp)
target_link_libraries(2015_20 input)

add_executable(2015_21 src/2015/day_21.cpp)
target_link_libraries(2015_21 input)

add_executable(2015_22 src/2015/day_22.cpp)
target_link_libraries(2015_22 input)

add_executable(2015_23 src/2015/day_23.cpp)
target_link_libraries(2015_23 input)

add_executable(2015_24 src/2015/day_24.cpp)
target_link_libraries(2015_24 input)

add_executable(2015_25 src/2015/day_25.cpp)
target_link_libraries(2015_25 input)

add_executable(2016_01 src/2016/day_01.cpp)
target_link_libraries(2016_01 input)

add_executable(2016_02 src/2016/day_02.cpp)
target_link_libraries(2016_02 input)

add_executable(2016_03 src/2016/day_03.cpp)
target_link_libraries(2016_03 input)

add_executable(2016_04 src/2016/day_04.cpp)
target_link_libraries(2016_04 input)

add_executable(2016_05 src/2016/day_05.cpp)
target_link_libraries(2016_05 input md5)

add_executable(2016_06 src/2016/day_06.cpp)
target_link_libraries(2016_06 input md5)

add_executable(2016_07 src/2016/day_07.cpp)
target_link_libraries(2016_07 input)

add_executable(2016_08 src/2016/day_08.cpp)
target_link_libraries(2016_08 input)

add_executable(2016_09 src/2016/day_09.cpp)
target_link_libraries(2016_09 input)

add_executable(2016_10 src/2016/day_10.cpp)
target_link_libraries(2016_10 input)

add_executable(2016_11 src/2016/day_11.cpp)
target_link_libraries(2016_11 input)

add_executable(2016_12 src/2016/day_12.cpp)
target_link_libraries(2016_12 input)

add_executable(2016_13 src/2016/day_13.cpp)
target_link_libraries(2016_13 input)

add_executable(2016_14 src/2016/day_14.cpp)
target_link_libraries(2016_14 input md5)

add_executable(2016_15 src/2016/day_15.cpp)
target_link_libraries(2016_15 input)

add_executable(2016_16 src/2016/day_16.cpp)
target_link_libraries(2016_16 input)

add_executable(2016_17 src/2016/day_17.cpp)
target_link_libraries(2016_17 input md5)

add_executable(2016_18 src/2016/day_18.cpp)
target_link_libraries(2016_18 input )

add_executable(2016_19 src/2016/day_19.cpp)
target_link_libraries(2016_19 input )

add_executable(2016_20 src/2016/day_20.cpp)
target_link_libraries(2016_20 input )

add_executable(2016_21 src/2016/day_21.cpp)
target_link_libraries(2016_21 input )

add_executable(2016_22 src/2016/day_22.cpp)
target_link_libraries(2016_22 input )

add_executable(2016_23 src/2016/day_23.cpp)
target_link_libraries(2016_23 input )

add_executable(2016_24 src/2016/day_24.cpp)
target_link_libraries(2016_24 input )

add_executable(2017_01 src/2017/day_01.cpp)
target_link_libraries(2017_01 input)

add_executable(2017_02 src/2017/day_02.cpp)
target_link_libraries(2017_02 input)

add_executable(2017_03 src/2017/day_03.cpp)
target_link_libraries(2017_03 input)

add_executable(2017_04 src/2017/day_04.cpp)
target_link_libraries(2017_04 input)

add_executable(2017_05 src/2017/day_05.cpp)
target_link_libraries(2017_05 input)

add_executable(2017_06 src/2017/day_06.cpp)
target_link_libraries(2017_06 input)

add_executable(2017_07 src/2017/day_07.cpp)
target_link_libraries(2017_07 input)

add_executable(2017_08 src/2017/day_08.cpp)
target_link_libraries(2017_08 input)

add_executable(2017_09 src/2017/day_09.cpp)
target_link_libraries(2017_09 input)

add_executable(2017_10 src/2017/day_10.cpp)
target_link_libraries(2017_10 input)

add_executable(2017_11 src/2017/day_11.cpp)
target_link_libraries(2017_11 input hexgrid)

add_executable(2017_12 src/2017/day_12.cpp)
target_link_libraries(2017_12 input)

add_executable(2017_13 src/2017/day_13.cpp)
target_link_libraries(2017_13 input)

add_executable(2017_14 src/2017/day_14.cpp)
target_link_libraries(2017_14 input)

add_executable(2017_15 src/2017/day_15.cpp)
target_link_libraries(2017_15 input)

add_executable(2017_16 src/2017/day_16.cpp)
target_link_libraries(2017_16 input)

add_executable(2017_17 src/2017/day_17.cpp)
target_link_libraries(2017_17 input)

add_executable(2017_18 src/2017/day_18.cpp)
target_link_libraries(2017_18 input)

add_executable(2017_19 src/2017/day_19.cpp)
target_link_libraries(2017_19 input)

add_executable(2017_20 src/2017/day_20.cpp)
target_link_libraries(2017_20 input)

add_executable(2017_21 src/2017/day_21.cpp)
target_link_libraries(2017_21 input)

add_executable(2017_22 src/2017/day_22.cpp)
target_link_libraries(2017_22 input)

add_executable(2017_23 src/2017/day_23.cpp)
target_link_libraries(2017_23 input)

add_executable(2017_24 src/2017/day_24.cpp)
target_link_libraries(2017_24 input)

add_executable(2017_25 src/2017/day_25.cpp)
target_link_libraries(2017_25 input)

add_executable(2018_01 src/2018/day_01.cpp)
target_link_libraries(2018_01 input)

add_executable(2018_02 src/2018/day_02.cpp)
target_link_libraries(2018_02 input)

add_executable(2018_03 src/2018/day_03.cpp)
target_link_libraries(2018_03 input)

add_executable(2018_04 src/2018/day_04.cpp)
target_link_libraries(2018_04 input)

add_executable(2018_05 src/2018/day_05.cpp)
target_link_libraries(2018_05 input)

add_executable(2018_06 src/2018/day_06.cpp)
target_link_libraries(2018_06 input)

add_executable(2018_07 src/2018/day_07.cpp)
target_link_libraries(2018_07 input)

add_executable(2018_08 src/2018/day_08.cpp)
target_link_libraries(2018_08 input)

add_executable(2018_09 src/2018/day_09.cpp)
target_link_libraries(2018_09 input)

add_executable(2018_10 src/2018/day_10.cpp)
target_link_libraries(2018_10 input)

add_executable(2018_11 src/2018/day_11.cpp)
target_link_libraries(2018_11 input)

add_executable(2018_12 src/2018/day_12.cpp)
target_link_libraries(2018_12 input)

add_executable(2018_13 src/2018/day_13.cpp)
target_link_libraries(2018_13 input)

add_executable(2018_14 src/2018/day_14.cpp)
target_link_libraries(2018_14 input)

add_executable(2018_15 src/2018/day_15.cpp)
target_link_libraries(2018_15 input)

add_executable(2018_16 src/2018/day_16.cpp)
target_link_libraries(2018_16 input)

add_executable(2018_17 src/2018/day_17.cpp)
target_link_libraries(2018_17 input)

add_executable(2018_18 src/2018/day_18.cpp)
target_link_libraries(2018_18 input)

add_executable(2018_23 src/2018/day_23.cpp)
target_link_libraries(2018_23 input)

add_executable(2019_01 src/2019/day_01.cpp)
target_link_libraries(2019_01 input)

add_executable(2019_02 src/2019/day_02.cpp)
target_link_libraries(2019_02 input)

add_executable(2019_03 src/2019/day_03.cpp)
target_link_libraries(2019_03 input)

add_executable(2019_04 src/2019/day_04.cpp)
target_link_libraries(2019_04 input)

add_executable(2019_05 src/2019/day_05.cpp)
target_link_libraries(2019_05 input)

add_executable(2019_06 src/2019/day_06.cpp)
target_link_libraries(2019_06 input)

add_executable(2019_07 src/2019/day_07.cpp)
target_link_libraries(2019_07 input)

add_executable(2019_08 src/2019/day_08.cpp)
target_link_libraries(2019_08 input)

add_executable(2019_09 src/2019/day_09.cpp)
target_link_libraries(2019_09 input)

add_executable(2019_10 src/2019/day_10.cpp)
target_link_libraries(2019_10 input)

add_executable(2019_11 src/2019/day_11.cpp)
target_link_libraries(2019_11 input)

add_executable(2019_12 src/2019/day_12.cpp)
target_link_libraries(2019_12 input)

add_executable(2019_13 src/2019/day_13.cpp)
target_link_libraries(2019_13 input)

add_executable(2019_14 src/2019/day_14.cpp)
target_link_libraries(2019_14 input)

add_executable(2019_15 src/2019/day_15.cpp)
target_link_libraries(2019_15 input)

add_executable(2019_17 src/2019/day_17.cpp)
target_link_libraries(2019_17 input)

add_executable(2019_22 src/2019/day_22.cpp)
target_link_libraries(2019_22 input)

add_executable(2020_01 src/2020/day_01.cpp)
target_link_libraries(2020_01 input)

add_executable(2020_02 src/2020/day_02.cpp)
target_link_libraries(2020_02 input)

add_executable(2020_03 src/2020/day_03.cpp)
target_link_libraries(2020_03 input)

add_executable(2020_04 src/2020/day_04.cpp)
target_link_libraries(2020_04 input)

add_executable(2020_05 src/2020/day_05.cpp)
target_link_libraries(2020_05 input)

add_executable(2020_06 src/2020/day_06.cpp)
target_link_libraries(2020_06 input)

add_executable(2020_07 src/2020/day_07.cpp)
target_link_libraries(2020_07 input)

add_executable(2020_08 src/2020/day_08.cpp)
target_link_libraries(2020_08 input)

add_executable(2020_09 src/2020/day_09.cpp)
target_link_libraries(2020_09 input)

add_executable(2020_10 src/2020/day_10.cpp)
target_link_libraries(2020_10 input)

add_executable(2020_11 src/2020/day_11.cpp)
target_link_libraries(2020_11 input)

add_executable(2020_12 src/2020/day_12.cpp)
target_link_libraries(2020_12 input)

add_executable(2020_13 src/2020/day_13.cpp)
target_link_libraries(2020_13 input)

add_executable(2020_14 src/2020/day_14.cpp)
target_link_libraries(2020_14 input)

add_executable(2020_15 src/2020/day_15.cpp)
target_link_libraries(2020_15 input)

add_executable(2020_16 src/2020/day_16.cpp)
target_link_libraries(2020_16 input)

add_executable(2020_17 src/2020/day_17.cpp)
target_link_libraries(2020_17 input)

# add_executable(2020_18 src/2020/day_18.cpp)
# target_link_libraries(2020_18 input)

# add_executable(2020_20 src/2020/day_20.cpp)
# target_link_libraries(2020_20 input)

# add_executable(2020_21 src/2020/day_21.cpp)
# target_link_libraries(2020_21 input)

add_executable(2020_22 src/2020/day_22.cpp)
target_link_libraries(2020_22 input)

add_executable(2020_23 src/2020/day_23.cpp)
target_link_libraries(2020_23 input)

add_executable(2020_24 src/2020/day_24.cpp)
target_link_libraries(2020_24 input)

add_executable(2020_25 src/2020/day_25.cpp)
target_link_libraries(2020_25 input)

add_executable(2021_01 src/2021/day_01.cpp)
target_link_libraries(2021_01 input)

add_executable(2021_02 src/2021/day_02.cpp)
target_link_libraries(2021_02 input)

add_executable(2021_03 src/2021/day_03.cpp)
target_link_libraries(2021_03 input)

add_executable(2021_04 src/2021/day_04.cpp)
target_link_libraries(2021_04 input)

add_executable(2021_05 src/2021/day_05.cpp)
target_link_libraries(2021_05 input)

add_executable(2021_06 src/2021/day_06.cpp)
target_link_libraries(2021_06 input)

add_executable(2021_07 src/2021/day_07.cpp)
target_link_libraries(2021_07 input)

add_executable(2021_08 src/2021/day_08.cpp)
target_link_libraries(2021_08 input)

add_executable(2021_09 src/2021/day_09.cpp)
target_link_libraries(2021_09 input)

add_executable(2021_10 src/2021/day_10.cpp)
target_link_libraries(2021_10 input)

add_executable(2021_11 src/2021/day_11.cpp)
target_link_libraries(2021_11 input)

add_executable(2021_12 src/2021/day_12.cpp)
target_link_libraries(2021_12 input)

add_executable(2021_13 src/2021/day_13.cpp)
target_link_libraries(2021_13 input)

add_executable(2021_15 src/2021/day_15.cpp)
target_link_libraries(2021_15 input)
