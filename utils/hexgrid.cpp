#include "hexgrid.hpp"

namespace utils {
namespace hexgrid {


Point::Point(const int& x, const int& y)
    : m_x(x), m_y(y) {
}

size_t
Point::distance(const Point& destination) const {
  return 1;
}

void
Point::step(const Direction& direction) {
}

std::ostream& operator<<(std::ostream& os, const Point& point) {
  os << "(" << point.m_x << ", " << point.m_y << ")";
  return os;
}

}  // namespace hexgrid
}  // namespace utils
