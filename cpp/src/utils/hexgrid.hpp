#pragma once

#include <algorithm>
#include <ostream>

namespace utils {
namespace hexgrid {

enum class Direction {
  kNorthWest,
  kNorth,
  kNorthEast,
  kSouthEast,
  kSouth,
  kSouthWest,
};

std::ostream&
operator<<(std::ostream&os, const Direction& direction);

/*
 * Hex Coordinate System:
 *           _ _
 *         /     \
 *    _ _ / (0, 1)\ _ _
 *  /     \       /     \
 * /(-1,0) \ _ _ / (1,1) \
 * \       /     \       /
 *  \ _ _ / (0,0) \ _ _ /
 *  /     \       /     \
 * /(-1,-1)\ _ _ / (1,0) \
 * \       /     \       /
 *  \ _ _ / (0,-1)\ _ _ /
 *        \       /
 *         \ _ _ /
 */
class Point {
 private:
  int m_x, m_y;

 public:
  Point(const int& x, const int& y);

  Point(const Point& point);

  size_t distance(const Point& destination) const;

  void step(const Direction& direction);

  Point& operator+=(const Point& summand);

  friend bool operator==(const Point& lhs, const Point& rhs);

  friend Point operator+(const Point& lhs, const Point& rhs);

  friend std::ostream& operator<<(std::ostream& os, const Point& point);
};

}  // namespace hexgrid
}  // namespace utils
