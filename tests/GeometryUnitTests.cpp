#include <gtest/gtest.h>

#include <sstream>

#include "../utils/geometry.hpp"

namespace geometry {
using utils::geometry::Point;

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

}  // namespace geometry
