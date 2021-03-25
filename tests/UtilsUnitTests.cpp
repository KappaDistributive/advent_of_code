#include <gtest/gtest.h>

#include "../utils/input.hpp"

namespace {
TEST(SplitString, EmptyResult) {
  auto seen = utils::split_string("eeeeeeeee", 'e');
  std::vector<std::string> expected;
  EXPECT_EQ(seen, expected);
}

TEST(SplitString, StripsDelimiter) {
  auto seen = utils::split_string(" this is a test ", ' ');
  std::vector<std::string> expected{{"this", "is", "a", "test"}};
  EXPECT_EQ(seen, expected);
}

TEST(SplitString, RepeatDelimiters) {
  auto seen = utils::split_string("..this.is..a.test.", '.');
  std::vector<std::string> expected{{"this", "is", "a", "test"}};
  EXPECT_EQ(seen, expected);
}

}  // namespace
