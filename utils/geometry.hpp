#pragma once

#include <algorithm>
#include <array>
#include <bitset>
#include <iostream>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <vector>

namespace utils {
namespace geometry {

using std::size_t;

template <typename T, size_t d>
class Point {
 private:
  std::array<T, d> m_coordinates;

 public:
  Point<T, d>();
  Point<T, d>(const std::vector<T>& coordinates);
  Point<T, d>(const std::array<T, d>& coordinates);

  Point<T, d>(const Point<T, d>& point);

  ~Point<T, d>() = default;

  void operator=(const Point<T, d>& rhs);
  bool operator==(const Point<T, d>& rhs) const;
  std::array<T, d> coordinates() const;

  T manhatten_distance(const Point<T, d>& other) const;

  template <typename T_, size_t d_>
  friend Point<T_, d_> operator+(const Point<T_, d_>& lhs,
                                 const Point<T_, d_>& rhs);

  template <typename T_, size_t d_>
  friend std::ostream& operator<<(std::ostream&, const Point<T_, d_>& point);
};

template <typename T, size_t d>
size_t manhatten_distance(const Point<T, d>& origin,
                          const Point<T, d>& destination);

template <typename T>
constexpr auto constexpr_pow(T base, unsigned int exponent)
    -> decltype(base + 1) {
  return exponent ? base * constexpr_pow(base, exponent - 1) : 1;
}

template <size_t d>
constexpr size_t num_corners = constexpr_pow(2, d);

// A RasterCuboid is a Cuboid whose faces are parallel with those of the
// Cartesian coordinate system. In other words: Any RasterCuboid can be obtained
// by scaling and shifting the UnitCuboid, without rotation.
template <typename T, size_t d>
class RasterCuboid {
 private:
  Point<T, d> m_base;          // lexicographically-minimal corner
  std::array<T, d> m_lengths;  // lengths alogn Cartesian coordinate axes

 public:
  // creates the `unit RasterCuboid`
  RasterCuboid<T, d>();

  explicit RasterCuboid<T, d>(const Point<T, d>& base, const std::array<T, d>& lengths);

  explicit RasterCuboid<T, d>(const std::array<Point<T, d>, num_corners<d>>& corners);

  // The i-th entry in the bitset of a corner (from left to right) specifies the
  // i-th dimension in a Cartesian coordinate system. In the example below,
  // corners are labeled with their (x,y,z)-offsets:
  //        ┌──────────────────────────────────┐
  //       /│011                              /│111
  //      / │                                / │
  //     /  │                               /  │
  //    /   │                              /   │
  //   /                                  /    │
  //  ┌──────────────────────────────────┐     │
  //  │010                               │110  │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     │                            │     │
  //  │     └─────────────────────────── │ ────┘
  //  │    / 001                         │    / 101
  //  │   /                              │   /
  //  │  /                               │  /
  //  │ /                                │ /
  //  │/                                 │/
  //  └──────────────────────────────────┘
  //   000                                100
  Point<T, d> corner(std::bitset<d> corner) const;

  // Return all corners in the order of increasing bitset values (from 0 to 2^d - 1).
  std::array<Point<T, d>, num_corners<d>> corners() const;

  std::optional<RasterCuboid> intersect(
      const RasterCuboid<T, d>& other) const noexcept;

  bool operator==(const RasterCuboid<T, d>& rhs) const noexcept;

  template <typename T_, size_t d_>
  friend std::ostream& operator<<(std::ostream& os,
                                  const RasterCuboid<T_, d_>& RasterCuboid);
};

}  // namespace geometry
}  // namespace utils

#include "geometry.tpp"
