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

  friend std::ostream& operator<<(std::ostream& os, const Point& point);
};

}  // namespace hexgrid
}  // namespace utils
