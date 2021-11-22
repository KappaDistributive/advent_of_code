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
  Point<T, d>();
  Point<T, d>(const std::vector<T>& coordinates);
  Point<T, d>(const std::array<T, d>& coordinates);

  Point<T, d>(const Point<T, d>& point);

  ~Point<T, d>() = default;

  bool operator==(const Point<T, d>& rhs) const;
  std::array<T, d> coordinates() const;

  template <typename T_, size_t d_>
  friend Point<T_, d_> operator+(const Point<T_, d_>& lhs, const Point<T_, d_>& rhs);

  template<typename T_, size_t d_>
  friend std::ostream& operator<<(std::ostream&, const Point<T_, d_>& point);
};

template<typename T, size_t d>
size_t manhatten_distance(const Point<T, d>& origin, const Point<T, d>& destination);

}  // namespace geometry
}  // namespace utils

#include "geometry.tpp"
