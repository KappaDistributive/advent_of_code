#include <gtest/gtest.h>
#include <sstream>

#include "../utils/geometry.hpp"

namespace geometry {
// using utils::geometry::Point;

TEST(Point, Print) {
  utils::geometry::Point<int, 3> point(std::vector<int>{1, 2, 3});
  std::stringstream ss;
  ss << point;

  std::string want{"(1, 2, 3)"};
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}
}  // namespace geometry
