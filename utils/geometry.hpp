#pragma once

#include <array>
#include <bitset>
#include <iostream>
#include <optional>
#include <vector>
#include <set>

namespace utils {
namespace geometry {

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
constexpr auto constexpr_pow(T base, unsigned int exponent) -> decltype(base + 1) {
  return exponent ? base * constexpr_pow(base, exponent - 1) : 1;
}

template <size_t d>
constexpr size_t num_corners = constexpr_pow(2, d);

// A RasterCuboid is a Cuboid whose faces are parallel with those of the Cartesian coordinate system.
// In other words: Any RasterCuboid can be obtained by scaling and shifting the UnitCuboid, without rotation.
template <typename T, size_t d>
class RasterCuboid {
 private:
  Point<T, d> m_base;  // lexicographically-minimal corner
  std::array<T, d-1> m_lengths;

 public:
  // creates the `unit RasterCuboid`
  RasterCuboid<T, d>();

  Point<T, d> corner(std::bitset<d> corner) const noexcept;

  // std::optional<RasterCuboid> intersect(const RasterCuboid<T, d>& other) const;

  

  bool operator==(const RasterCuboid<T, d>& rhs) const noexcept;

  template <typename T_, size_t d_>
  friend std::ostream& operator<<(std::ostream& os,
                                  const RasterCuboid<T_, d_>& RasterCuboid);
};

}  // namespace geometry
}  // namespace utils

#include "geometry.tpp"
