#include <gtest/gtest.h>

#include "../src/utils/input.hpp"

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

TEST(ExtractNumbers, Simple) {
  std::vector<int> want{{1, -2, 3, 4, 5}};
  auto got = utils::extract_numbers<int>("1 -2 +3 4 5");
  EXPECT_EQ(want, got);
}

TEST(ExtractNumbers, Organic) {
  std::vector<int> want{{3, 0, -2, -2}};
  auto got = utils::extract_numbers<int>("p=+3,0 v=-2,-2");
  EXPECT_EQ(want, got);
}

TEST(ExtractNumbers, Math) {
  std::vector<int> want{{3, 14, 2, 71828, -1, 10, -2}};
  auto got = utils::extract_numbers<int>("pi=3.14 e=2.71828 i=-1 x=10E-2");
  EXPECT_EQ(want, got);
}

TEST(ExtractNumbers64, Simple) {
  std::vector<int64_t> want{{1, -2, 3, 4, 5}};
  auto got = utils::extract_numbers<int64_t>("1 -2 +3 4 5");
  EXPECT_EQ(want, got);
}

TEST(ExtractNumbers64, Organic) {
  std::vector<int64_t> want{{3, 0, -2, -2}};
  auto got = utils::extract_numbers<int64_t>("p=+3,0 v=-2,-2");
  EXPECT_EQ(want, got);
}

TEST(ExtractNumbers64, Math) {
  std::vector<int64_t> want{{3, 14, 2, 71828, -1, 10, -2}};
  auto got = utils::extract_numbers<int64_t>("pi=3.14 e=2.71828 i=-1 x=10E-2");
  EXPECT_EQ(want, got);
}

TEST(ExtractNumbers64, Large) {
  std::vector<int64_t> want{{-23372036854775808, 23372036854775808}};
  auto got = utils::extract_numbers<int64_t>("-23372036854775808 23372036854775808");
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

TEST(Sign, Negative) {
  int want{-1};
  auto got = utils::sign(-2);

  EXPECT_EQ(want, got);
}

TEST(Sign, Zero) {
  int want{0};
  auto got = utils::sign(0);

  EXPECT_EQ(want, got);
}

TEST(Sign, Positive) {
  int want{1};
  auto got = utils::sign(123);

  EXPECT_EQ(want, got);
}

}  // namespace
