#include <gtest/gtest.h>
#include <sstream>

#include "../utils/hexgrid.hpp"

namespace  {
using utils::hexgrid::Point;

TEST(Point, Print) {
  Point point(-1, 2);
  std::stringstream ss;
  ss << point;

  std::string want{"(-1, 2)"};
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(Point, Equality_test_0) {
  Point lhs(1, 1);
  Point rhs(1, 1);
  EXPECT_TRUE(lhs == rhs);
}

TEST(Point, Equality_test_1) {
  Point lhs(0, 0);
  Point rhs(1, 1);
  EXPECT_FALSE(lhs == rhs);
}

TEST(Point, IncAddition_test_0) {
  Point got(0, 0);
  Point summand(1, 2);
  Point want(1, 2);
  got += summand;

  EXPECT_EQ(want, got);
}

TEST(Point, Addition_test_0) {
  Point lhs(0, 0);
  Point rhs(1, 2);
  Point want(1, 2);
  auto got = lhs + rhs;

  EXPECT_EQ(want, got);
}

}  // namespace
