#include <gtest/gtest.h>

#include <sstream>

#include "../utils/geometry.hpp"

namespace geometry {
using utils::geometry::Point;


TEST(Point, DefaultConstructor) {
  Point<int, 3> point;
  auto got = point.coordinates();
  std::array<int, 3> want{{0, 0, 0}};

  EXPECT_EQ(want, got);
}
TEST(Point, Coordinates) {
  Point<int, 3> point(std::array<int, 3>{1, 2, 3});
  auto got = point.coordinates();
  std::array<int, 3> want{{1, 2, 3}};

  EXPECT_EQ(want, got);
}

TEST(Point, Print) {
  Point<int, 3> point(std::array<int, 3>{1, 2, 3});
  std::stringstream ss;
  ss << point;

  std::string want{"(1, 2, 3)"};
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(Point, PrintVector) {
  Point<int, 3> point(std::vector<int>{1, 2, 3});
  std::stringstream ss;
  ss << point;

  std::string want{"(1, 2, 3)"};
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(Point, CopyConstructor) {
  Point<int, 3> copy(std::vector<int>{3, 4, 5});
  {
    Point<int, 3> point(std::vector<int>{1, 2, 3});
    copy = point;
    EXPECT_EQ(point, copy);
  }

  std::stringstream ss;
  ss << copy;
  std::string want{"(1, 2, 3)"};
  std::string got{ss.str()};
}

TEST(Point, Addition) {
  Point<int, 3> lhs(std::vector<int>{1, 2, 3});
  Point<int, 3> rhs(std::vector<int>{4, 5, 6});

  auto got = lhs + rhs;
  Point<int, 3> want(std::vector<int>{5, 7, 9});

  EXPECT_EQ(want, got);
}

}  // namespace geometry
