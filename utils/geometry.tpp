namespace utils {
namespace geometry {

template<typename  T, size_t d>
Point<T, d>::Point(const std::vector<T>& coordinates) {
  assert(coordinates.size() == d);
  this->m_coordinates = std::array<T, d>();
  for (size_t index{0}; index < coordinates.size(); ++index) {
    this->m_coordinates[index] = coordinates[index];
  }
}

template<typename  T, size_t d>
Point<T, d>::Point(const std::array<T, d>& coordinates) : m_coordinates(coordinates) {}

template<typename  T, size_t d>
Point<T, d>::Point(const Point<T, d>& point) : m_coordinates(point.m_coordinates) {}

template<typename T, size_t d>
bool Point<T, d>::operator==(const Point<T, d>& other) const {
  return this->m_coordinates == other.m_coordinates;
}

template<typename T, size_t d>
std::ostream& operator<<(std::ostream& os, const Point<T, d>& point) {
  os << '(';
  for (auto it{point.m_coordinates.begin()}; it != point.m_coordinates.end(); ++it) {
    os << *it;
    if (std::next(it) != point.m_coordinates.end()) {
      os << ", ";
    } else {
      os << ')';
    }
  }
  return os;
}


}  // namespace geometry
}  // namespace utils
