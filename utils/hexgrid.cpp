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
  switch (direction) {
    case Direction::kNorthWest:
      this->m_x--;
      this->m_y--;
      break;
  case Direction::kNorthEast:
      this->m_x++;
      this->m_y--;
      break;
  case Direction::kEast:
      this->m_x++;
      break;
  case Direction::kSouthEast:
      this->m_x++;
      this->m_y++;
      break;
  case Direction::kSouthWest:
      this->m_x--;
      this->m_y++;
      break;
  case Direction::kWest:
      this->m_x--;
      break;
  }
}

Point&
Point::operator+=(const Point& summand) {
  this->m_x += summand.m_x;
  this->m_y += summand.m_y;

  return *this;
}

bool
operator==(const Point& lhs, const Point& rhs) {
  return lhs.m_x == rhs.m_x && lhs.m_y == rhs.m_y;
}

Point
operator+(const Point& lhs, const Point& rhs) {
  Point point(lhs.m_x + rhs.m_x, lhs.m_y + rhs.m_y);

  return point;
}

std::ostream& operator<<(std::ostream& os, const Point& point) {
  os << "(" << point.m_x << ", " << point.m_y << ")";
  return os;
}

}  // namespace hexgrid
}  // namespace utils
