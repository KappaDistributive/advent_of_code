#include <gtest/gtest.h>

#include "../utils/input.hpp"

namespace {
TEST(SplitStringOnSpaces, Positive) {
  auto seen = utils::split_string("this is a test", ' ');
  std::vector<std::string> expected{{"this", "is", "a", "test"}};
  EXPECT_EQ(seen, expected);
}
}  // namespace
