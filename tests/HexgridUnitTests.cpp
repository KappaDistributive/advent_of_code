#include <gtest/gtest.h>
#include <sstream>

#include "../utils/hexgrid.hpp"

namespace  {
using utils::hexgrid::Direction;
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

TEST(Point, Distance_test_0) {
  Point lhs(0, 0);
  Point rhs(0, 0);
  size_t want{0};
  size_t got = lhs.distance(rhs);

  EXPECT_EQ(want, got);
}

TEST(Point, Distance_test_1) {
  Point lhs(0, 0);
  Point rhs(1, 1);
  size_t want{1};
  size_t got = lhs.distance(rhs);

  EXPECT_EQ(want, got);
}

TEST(Point, Distance_test_2) {
  Point lhs(2, 5);
  Point rhs(4, 2);
  size_t want{5};
  size_t got = lhs.distance(rhs);

  EXPECT_EQ(want, got);
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

TEST(Point, Step_test_0) {
  Point want(-1, -1);
  Point got(0, 0);
  got.step(Direction::kNorthWest);

  EXPECT_EQ(want, got);
}

TEST(Point, Step_test_1) {
  Point want(1, -1);
  Point got(0, 0);
  got.step(Direction::kNorthEast);

  EXPECT_EQ(want, got);
}

TEST(Point, Step_test_2) {
  Point want(1, 0);
  Point got(0, 0);
  got.step(Direction::kEast);

  EXPECT_EQ(want, got);
}

TEST(Point, Step_test_3) {
  Point want(1, 1);
  Point got(0, 0);
  got.step(Direction::kSouthEast);

  EXPECT_EQ(want, got);
}

TEST(Point, Step_test_4) {
  Point want(-1, 1);
  Point got(0, 0);
  got.step(Direction::kSouthWest);

  EXPECT_EQ(want, got);
}

TEST(Point, Step_test_5) {
  Point want(-1, 0);
  Point got(0, 0);
  got.step(Direction::kWest);

  EXPECT_EQ(want, got);
}

}  // namespace
