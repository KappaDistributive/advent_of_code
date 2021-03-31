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

}  // namespace
