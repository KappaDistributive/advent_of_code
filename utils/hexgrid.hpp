#pragma once

#include <ostream>

namespace utils {
namespace hexgrid {

enum class Direction {
  kNorthWest,
  kNorthEast,
  kEast,
  kSouthEast,
  kSouthWest,
  kWest,
};

class Point {
 private:
  int m_x, m_y;

 public:
  Point(const int& x, const int& y);

  size_t distance(const Point& destination) const;

  void step(const Direction& direction);

  Point& operator+=(const Point& summand);

  friend bool operator==(const Point& lhs, const Point& rhs);

  friend Point operator+(const Point& lhs, const Point& rhs);

  friend std::ostream& operator<<(std::ostream& os, const Point& point);
};

}  // namespace hexgrid
}  // namespace utils
