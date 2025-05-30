#include <stdexcept>
#include <gtest/gtest.h>

#include <array>
#include <sstream>

#include "../src/utils/geometry.hpp"

#define LOG_ERROR(X) {if(Log::error_is_active()){std::ostringstream o;o<<X;Log::error(o.str());}}

namespace geometry {
using utils::geometry::Direction;
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
  EXPECT_FALSE(want != got);
}

TEST(Point, Direction) {
  Point<int, 2> point(Direction::Up);
  auto got = point.coordinates();
  std::array<int, 2> want{{0, -1}};

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

TEST(Point, CopyConstructor) {
  Point<int, 3> copy(std::array<int, 3>{3, 4, 5});
  {
    Point<int, 3> point(std::array<int, 3>{1, 2, 3});
    copy = point;
    EXPECT_EQ(point, copy);
  }

  std::stringstream ss;
  ss << copy;
  std::string want{"(1, 2, 3)"};
  std::string got{ss.str()};
}

TEST(Point, Addition) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  Point<int, 3> rhs(std::array<int, 3>{4, 5, 6});

  auto got = lhs + rhs;
  Point<int, 3> want(std::array<int, 3>{5, 7, 9});

  EXPECT_EQ(want, got);
}

TEST(Point, AdditionDirection) {
  auto got = Point<int, 2>{{0, 0}} + Direction::Up;
  Point<int, 2> want{{0, -1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}} + Direction::Right;
  want = Point<int, 2>{{1, 0}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}} + Direction::Down;
  want = Point<int, 2>{{0, 1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}} + Direction::Left;
  want = Point<int, 2>{{-1, 0}};
  EXPECT_EQ(want, got);
}


TEST(Point, AdditionInPlace) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  Point<int, 3> rhs(std::array<int, 3>{4, 5, 6});

  auto got = lhs;
  got += rhs;
  Point<int, 3> want(std::array<int, 3>{5, 7, 9});

  EXPECT_EQ(want, got);
}

TEST(Point, AdditionInPlaceCoordinates) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  Point<int, 3> rhs(std::array<int, 3>{4, 5, 6});

  auto got = lhs;
  got += rhs.coordinates();
  Point<int, 3> want(std::array<int, 3>{5, 7, 9});

  EXPECT_EQ(want, got);
}

TEST(Point, AdditionInPlaceDirection) {
  Point<int, 2> got{{0, 0}};
  got += Direction::Up;
  Point<int, 2> want{{0, -1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}};
  got += Direction::Right;
  want = Point<int, 2>{{1, 0}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}};
  got += Direction::Down;
  want = Point<int, 2>{{0, 1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}};
  got += Direction::Left;
  want = Point<int, 2>{{-1, 0}};
  EXPECT_EQ(want, got);
}

TEST(Point, Negation) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});

  auto got = -lhs;
  Point<int, 3> want(std::array<int, 3>{-1, -2, -3});

  EXPECT_EQ(want, got);
}

TEST(Point, Subtraction) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  Point<int, 3> rhs(std::array<int, 3>{4, 5, 6});

  auto got = lhs - rhs;
  Point<int, 3> want(std::array<int, 3>{-3, -3, -3});

  EXPECT_EQ(want, got);
}

TEST(Point, SubtractionDirection) {
  auto got = Point<int, 2>{{0, 0}} - Direction::Up;
  Point<int, 2> want{{0, 1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}} - Direction::Right;
  want = Point<int, 2>{{-1, 0}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}} - Direction::Down;
  want = Point<int, 2>{{0, -1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}} - Direction::Left;
  want = Point<int, 2>{{1, 0}};
  EXPECT_EQ(want, got);
}

TEST(Point, SubtractionInPlace) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  Point<int, 3> rhs(std::array<int, 3>{4, 5, 6});

  auto got = lhs;
  got -= rhs;
  Point<int, 3> want(std::array<int, 3>{-3, -3, -3});

  EXPECT_EQ(want, got);
}

TEST(Point, SubtractionInPlaceCoordinates) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  Point<int, 3> rhs(std::array<int, 3>{4, 5, 6});

  auto got = lhs;
  got -= rhs.coordinates();
  Point<int, 3> want(std::array<int, 3>{-3, -3, -3});

  EXPECT_EQ(want, got);
}

TEST(Point, SubtractionInPlaceDirection) {
  Point<int, 2> got{{0, 0}};
  got -= Direction::Up;
  Point<int, 2> want{{0, 1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}};
  got -= Direction::Right;
  want = Point<int, 2>{{-1, 0}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}};
  got -= Direction::Down;
  want = Point<int, 2>{{0, -1}};
  EXPECT_EQ(want, got);

  got = Point<int, 2>{{0, 0}};
  got -= Direction::Left;
  want = Point<int, 2>{{1, 0}};
  EXPECT_EQ(want, got);
}


TEST(Point, Scaling) {
  Point<int, 3> lhs(std::array<int, 3>{1, 2, 3});
  auto got = lhs;
  got *= 2;
  Point<int, 3> want(std::array<int, 3>{2, 4, 6});

  EXPECT_EQ(want, got);
}

TEST(Point, Order) {
  Point<int, 3> a(std::array<int, 3>{-3, 1, -2});
  Point<int, 3> b(std::array<int, 3>{0, -1, -4});
  Point<int, 3> c(std::array<int, 3>{0, 0, 0});
  Point<int, 3> d(std::array<int, 3>{1, 2, 3});
  Point<int, 3> e(std::array<int, 3>{1, 2, 4});

  EXPECT_TRUE(a < b);
  EXPECT_TRUE(a < c);
  EXPECT_TRUE(a.operator<(d));
  EXPECT_TRUE(a < e);

  EXPECT_TRUE(b < c);
  EXPECT_TRUE(b < d);
  EXPECT_TRUE(b < e);

  EXPECT_TRUE(c < d);
  EXPECT_TRUE(c < e);
  
  EXPECT_TRUE(d.operator<(e));

  EXPECT_FALSE(a < a);
  EXPECT_FALSE(b < a);
  EXPECT_FALSE(c < a);
  EXPECT_FALSE(d < a);
  EXPECT_FALSE(e < a);

  EXPECT_FALSE(b < b);
  EXPECT_FALSE(c < b);
  EXPECT_FALSE(d < b);
  EXPECT_FALSE(e < b);

  EXPECT_FALSE(c < c);
  EXPECT_FALSE(d < c);
  EXPECT_FALSE(e < c);

  EXPECT_FALSE(d < d);
  EXPECT_FALSE(e < d);

  EXPECT_FALSE(e < e);
}

TEST(RaserCuboid, ConstructFromCorners) {
  std::array<Point<int, 3>, 8> corners{
      {Point<int, 3>(std::array<int, 3>{0, 0, 0}),
       Point<int, 3>(std::array<int, 3>{0, 0, 1}),
       Point<int, 3>(std::array<int, 3>{0, 1, 0}),
       Point<int, 3>(std::array<int, 3>{0, 1, 1}),
       Point<int, 3>(std::array<int, 3>{1, 0, 0}),
       Point<int, 3>(std::array<int, 3>{1, 0, 1}),
       Point<int, 3>(std::array<int, 3>{1, 1, 0}),
       Point<int, 3>(std::array<int, 3>{1, 1, 1})}};
  RasterCuboid<int, 3> cuboid{corners};

  std::string want{"Base: (0, 0, 0) Lengths: (1, 1, 1)"};
  std::stringstream ss;
  ss << cuboid;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(RaserCuboid, ConstructFromCornersConsistency) {
  Point<int, 3> base{std::array<int, 3>{-1, 2, -3}};
  std::array<int, 3> lengths{4, 5, 6};
  RasterCuboid<int, 3> want{base, lengths};
  auto got = RasterCuboid<int, 3>(want.corners());

  EXPECT_EQ(want, got);
}

TEST(RasterCuboid, ConstructFromIntervalsConsistency) {
  Point<int, 3> base{std::array<int, 3>{-1, 2, -3}};
  std::array<int, 3> lengths{4, 5, 6};
  RasterCuboid<int, 3> want{base, lengths};
  auto got = RasterCuboid<int, 3>(want.intervals());

  EXPECT_EQ(want, got);
}

TEST(RaserCuboid, ConstructFromCornersIllegal) {
  std::array<Point<int, 3>, 8> corners{
      {Point<int, 3>(std::array<int, 3>{0, 0, 0}),
       Point<int, 3>(std::array<int, 3>{0, 0, 1}),
       Point<int, 3>(std::array<int, 3>{0, 1, 0}),
       Point<int, 3>(std::array<int, 3>{0, 1, 1}),
       Point<int, 3>(std::array<int, 3>{1, 0, 0}),
       Point<int, 3>(std::array<int, 3>{1, 0, 1}),
       Point<int, 3>(std::array<int, 3>{1, 1, 0}),
       Point<int, 3>(std::array<int, 3>{1, 1, 2})}};
  using RasterCuboid = RasterCuboid<int, 3>;
  RasterCuboid cuboid;
  ASSERT_THROW(cuboid = RasterCuboid(corners), std::runtime_error);
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

TEST(RasterCuboid, Volume_3d) {
  Point<int, 3> base{std::array<int, 3>{1, 2, 3}};
  std::array<int, 3> lengths{4, 5, 6};
  RasterCuboid<int, 3> cuboid(base, lengths);

  int want = 120;
  auto got = cuboid.volume();

  ASSERT_EQ(want, got);
}

TEST(RasterCuboid, Intervals) {
  Point<int, 3> base{std::array<int, 3>{-1, 2, -3}};
  std::array<int, 3> lengths{4, 5, 6};
  RasterCuboid<int, 3> cuboid(base, lengths);
  std::array<std::pair<int, int>, 3> want{{{-1, 3}, {2, 7}, {-3, 3}}};
  auto got = cuboid.intervals();

  EXPECT_EQ(want, got);
}

TEST(RasterCuboid, IntersectionNoOp) {
  Point<int, 3> base{std::array<int, 3>{-1, 2, -3}};
  std::array<int, 3> lengths{4, 5, 6};
  RasterCuboid<int, 3> want(base, lengths);
  auto got = want.intersect(want);

  EXPECT_EQ(want, got);
}

TEST(RasterCuboid, IntersectionEmpty_1d) {
  Point<int, 1> base_lhs{std::array<int, 1>{0}};
  Point<int, 1> base_rhs{std::array<int, 1>{2}};
  std::array<int, 1> lengths{1};
  RasterCuboid<int, 1> lhs(base_lhs, lengths);
  RasterCuboid<int, 1> rhs(base_rhs, lengths);
  auto got = lhs.intersect(rhs);

  if (got.has_value()) {
    std::cerr << got.value() << std::endl;
  }

  EXPECT_FALSE(got.has_value());
}

TEST(RasterCuboid, IntersectionEmpty_3d) {
  Point<int, 3> base_lhs{std::array<int, 3>{1, 2, 3}};
  std::array<int, 3> lengths_lhs{1, 2, 3};
  Point<int, 3> base_rhs{std::array<int, 3>{4, 2, 3}};
  std::array<int, 3> lengths_rhs{2, 3, 4};
  RasterCuboid<int, 3> lhs(base_lhs, lengths_lhs);
  RasterCuboid<int, 3> rhs(base_rhs, lengths_rhs);
  auto got = lhs.intersect(rhs);

  if (got.has_value()) {
    std::cerr << got.value() << std::endl;
  }

  EXPECT_FALSE(got.has_value());
}

TEST(RasterCuboid, Order_2d) {
  using Point = Point<int, 2>;
  Point a_base{{1, 1}};
  std::array<int, 2> a_lengths{{3, 3}};
  RasterCuboid<int, 2> a{a_base, a_lengths};

  Point b_base{{0, 0}};
  std::array<int, 2> b_lengths{{2, 2}};
  RasterCuboid<int, 2> b{b_base, b_lengths};

  Point c_base{{1, 1}};
  std::array<int, 2> c_lengths{{1, 1}};
  RasterCuboid<int, 2> c{c_base, c_lengths};

  ASSERT_FALSE(a < a);
  ASSERT_FALSE(a < b);
  ASSERT_FALSE(a < c);
  ASSERT_TRUE(b < a);
  ASSERT_FALSE(b < b);
  ASSERT_FALSE(b < c);
  ASSERT_FALSE(c < a);
  ASSERT_FALSE(c < b);
  ASSERT_FALSE(c < c);
}
}  // namespace geometry
