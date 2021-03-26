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

TEST(Stringify, VectorEmpty) {
  std::string want{""};
  auto got = utils::stringify(std::vector<char>());
  EXPECT_EQ(want, got);
}

TEST(Stringify, VectorNonEmpty) {
  std::string want{"abc def"};
  auto got = utils::stringify(
    std::vector<char>({'a', 'b', 'c', ' ', 'd', 'e', 'f'}));
  EXPECT_EQ(want, got);
}

TEST(ReplaceAllSubstrings, NoOp) {
  std::string want{"this is a test"};
  std::string got{"this is a test"};
  utils::replace_all_substrings(
    &got,
    "was",
    "will");
  EXPECT_EQ(want, got);
}

TEST(ReplaceAllSubstrings, ReplaceOne) {
  std::string want{"that is a test"};
  std::string got{"this is a test"};
  utils::replace_all_substrings(
    &got,
    "this",
    "that");
  EXPECT_EQ(want, got);
}

TEST(ReplaceAllSubstrings, ReplaceMany) {
  std::string want{"thwas was a test"};
  std::string got{"this is a test"};
  utils::replace_all_substrings(
    &got,
    "is",
    "was");
  EXPECT_EQ(want, got);
}

}  // namespace
