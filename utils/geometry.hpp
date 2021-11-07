#pragma  once

#include <iostream>
#include <vector>

namespace utils {
namespace geometry {

template<typename T, size_t d>
class Point {
 private:
  std::vector<T> m_coordinates;
 public:
  Point<T, d>(const std::vector<T>& coordinates);

  template<typename T_, size_t d_>
  friend std::ostream& operator<<(std::ostream&, const Point<T_, d_>& point);
};
}  // namespace geometry
}  // namespace utils

#include "geometry.tpp"
