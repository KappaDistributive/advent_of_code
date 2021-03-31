#include "input.hpp"
#include "hexgrid.hpp"

namespace utils {
namespace hexgrid {

std::ostream&
operator<<(std::ostream& os, const Direction& direction) {
  switch (direction) {
    case Direction::kNorthWest: os << "northwest"; break;
    case Direction::kNorth: os << "north"; break;
    case Direction::kNorthEast: os << "northeast"; break;
    case Direction::kSouthWest: os << "southwest"; break;
    case Direction::kSouth: os << "south"; break;
    case Direction::kSouthEast: os << "southeast"; break;
  }

  return os;
}

Point::Point(const int& x, const int& y)
    : m_x(x), m_y(y) {
}

Point::Point(const Point& point)
    : m_x(point.m_x), m_y(point.m_y) {
}

size_t
Point::distance(const Point& destination) const {
  auto delta_x = destination.m_x - this->m_x;
  auto delta_y = destination.m_y - this->m_y;

  if ((delta_x > 0) == (delta_y > 0)) {
    return std::max(std::abs(delta_x), std::abs(delta_y));
  } else {
    return std::abs(delta_x) + std::abs(delta_y);
  }
}

void
Point::step(const Direction& direction) {
  switch (direction) {
    case Direction::kNorthWest:
      this->m_x--;
      break;
    case Direction::kNorth:
      this->m_y++;
      break;
    case Direction::kNorthEast:
      this->m_x++;
      this->m_y++;
      break;
    case Direction::kSouthEast:
      this->m_x++;
      break;
    case Direction::kSouth:
      this->m_y--;
      break;
    case Direction::kSouthWest:
      this->m_x--;
      this->m_y--;
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
