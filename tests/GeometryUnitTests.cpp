#include <gtest/gtest.h>

#include <array>
#include <sstream>

#include "../utils/geometry.hpp"

namespace geometry {
using utils::geometry::Point;
using utils::geometry::RasterCuboid;

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

TEST(RasterCuboid, Representation) {
  RasterCuboid<int, 3> cuboid;
  std::string want{"Base: (0, 0, 0) Lengths: (1, 1, 1)"};
  std::stringstream ss;
  ss << cuboid;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(RasterCuboid, RepresentationBase) {
  Point<int, 3> base{std::array<int, 3>{-1, 2, -3}};
  std::array<int, 3> lengths{4, 5, 6};
  RasterCuboid<int, 3> cuboid(base, lengths);
  std::string want{"Base: (-1, 2, -3) Lengths: (4, 5, 6)"};
  std::stringstream ss;
  ss << cuboid;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}


TEST(RasterCuboid, Corner) {
  RasterCuboid<int, 3> cuboid;
  std::vector<Point<int, 3>> wants{
      {Point<int, 3>(std::array<int, 3>{0, 0, 0}),
       Point<int, 3>(std::array<int, 3>{0, 0, 1}),
       Point<int, 3>(std::array<int, 3>{0, 1, 0}),
       Point<int, 3>(std::array<int, 3>{0, 1, 1}),
       Point<int, 3>(std::array<int, 3>{1, 0, 0}),
       Point<int, 3>(std::array<int, 3>{1, 0, 1}),
       Point<int, 3>(std::array<int, 3>{1, 1, 0}),
       Point<int, 3>(std::array<int, 3>{1, 1, 1})}};

  for (int corner{0}; corner < 8; ++corner) {
    std::cerr << std::bitset<3>(corner) << std::endl;
    auto want = wants[corner];
    auto got = cuboid.corner(std::bitset<3>(corner));
    EXPECT_EQ(want, got);
  }
}

TEST(RasterCuboid, Corners) {
  RasterCuboid<int, 3> cuboid;
  std::array<Point<int, 3>, 8> want{
      {Point<int, 3>(std::array<int, 3>{0, 0, 0}),
       Point<int, 3>(std::array<int, 3>{0, 0, 1}),
       Point<int, 3>(std::array<int, 3>{0, 1, 0}),
       Point<int, 3>(std::array<int, 3>{0, 1, 1}),
       Point<int, 3>(std::array<int, 3>{1, 0, 0}),
       Point<int, 3>(std::array<int, 3>{1, 0, 1}),
       Point<int, 3>(std::array<int, 3>{1, 1, 0}),
       Point<int, 3>(std::array<int, 3>{1, 1, 1})}};

  auto got = cuboid.corners();
  EXPECT_EQ(want, got);
}

}  // namespace geometry
