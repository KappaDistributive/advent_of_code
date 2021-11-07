namespace utils {
namespace geometry {

template<typename  T, size_t d>
Point<T, d>::Point(const std::vector<T>& coordinates) {
  this->m_coordinates = coordinates;
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
