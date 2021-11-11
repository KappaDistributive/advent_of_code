#pragma  once

#include <array>
#include <iostream>
#include <vector>

namespace utils {
namespace geometry {

template<typename T, size_t d>
class Point {
 private:
  std::array<T, d> m_coordinates;
 public:
  Point<T, d>(const std::vector<T>& coordinates);
  Point<T, d>(const std::array<T, d>& coordinates);


  Point<T, d>(const Point<T, d>& point);

  ~Point<T, d>() = default;

  bool operator==(const Point<T, d>& rhs) const;

  template<typename T_, size_t d_>
  friend std::ostream& operator<<(std::ostream&, const Point<T_, d_>& point);
};
}  // namespace geometry
}  // namespace utils

#include "geometry.tpp"
