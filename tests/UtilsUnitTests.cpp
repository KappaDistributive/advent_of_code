#include <gtest/gtest.h>

#include "../utils/input.hpp"

namespace {
TEST(SplitString, EmptyResult) {
  std::vector<std::string> want;
  auto got = utils::split_string("eeeeeeeee", 'e');
  EXPECT_EQ(want, got);
}

TEST(SplitString, StripsDelimiter) {
  std::vector<std::string> want{{"this", "is", "a", "test"}};
  auto got = utils::split_string(" this is a test ", ' ');
  EXPECT_EQ(want, got);
}

TEST(SplitString, RepeatDelimiters) {
  std::vector<std::string> want{{"this", "is", "a", "test"}};
  auto got = utils::split_string("..this.is..a.test.", '.');
  EXPECT_EQ(want, got);
}

TEST(RotateVector, NoOp) {
  std::vector<int> want{{1, 2, 3}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, 0);
  EXPECT_EQ(want, got);
}

TEST(RotateVector, RotateClockwise_test1) {
  std::vector<int> want{{3, 1, 2}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, 1);
  EXPECT_EQ(want, got);
}

TEST(RotateVector, RotateClockwise_test2) {
  std::vector<int> want{{2, 3, 1}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, 2);
  EXPECT_EQ(want, got);
}

TEST(RotateVector, RotateClockwise_test3) {
  std::vector<int> want{{1, 2, 3}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, 3);
  EXPECT_EQ(want, got);
}

TEST(RotateVector, RotateCounterClockwise_test1) {
  std::vector<int> want{{2, 3, 1}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, -1);
  EXPECT_EQ(want, got);
}

TEST(RotateVector, RotateCounterClockwise_test2) {
  std::vector<int> want{{3, 1, 2}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, -2);
  EXPECT_EQ(want, got);
}

TEST(RotateVector, RotateCounterClockwise_test3) {
  std::vector<int> want{{1, 2, 3}};
  auto got = utils::rotate_vector(std::vector<int>{{1, 2, 3}}, -3);
  EXPECT_EQ(want, got);
}

}  // namespace
