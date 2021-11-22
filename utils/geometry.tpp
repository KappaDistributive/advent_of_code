namespace utils {
namespace geometry {

template <typename T, size_t d>
Point<T, d>::Point() : m_coordinates(std::array<T, d>()){};

template <typename T, size_t d>
Point<T, d>::Point(const std::vector<T>& coordinates) {
  assert(coordinates.size() == d);
  this->m_coordinates = std::array<T, d>();
  for (size_t index{0}; index < coordinates.size(); ++index) {
    this->m_coordinates[index] = coordinates[index];
  }
}

template <typename T, size_t d>
Point<T, d>::Point(const std::array<T, d>& coordinates)
    : m_coordinates(coordinates) {}

template <typename T, size_t d>
Point<T, d>::Point(const Point<T, d>& point)
    : m_coordinates(point.m_coordinates) {}

template <typename T, size_t d>
bool Point<T, d>::operator==(const Point<T, d>& other) const {
  return this->m_coordinates == other.m_coordinates;
}

template <typename T, size_t d>
std::array<T, d> Point<T, d>::coordinates() const {
  return this->m_coordinates;
}

template <typename T, size_t d>
Point<T, d> operator+(const Point<T, d>& lhs, const Point<T, d>& rhs) {
  assert(lhs.m_coordinates.size() == d);
  assert(rhs.m_coordinates.size() == d);

  std::array<T, d> coordinates;
  for (size_t index{0}; index < d; ++index) {
    coordinates[index] = lhs.m_coordinates[index] + rhs.m_coordinates[index];
  }
  Point<T, d> result(coordinates);
  return result;
}

template <typename T, size_t d>
std::ostream& operator<<(std::ostream& os, const Point<T, d>& point) {
  os << '(';
  for (auto it{point.m_coordinates.begin()}; it != point.m_coordinates.end();
       ++it) {
    os << *it;
    if (std::next(it) != point.m_coordinates.end()) {
      os << ", ";
    } else {
      os << ')';
    }
  }
  return os;
}

template <typename T, size_t d>
size_t manhatten_distance(const Point<T, d>& origin,
                          const Point<T, d>& destination) {
  size_t distance{0};
  auto lhs = origin.coordinates();
  auto rhs = destination.coordinates();
  for (size_t index{0}; index < d; ++index) {
    distance += static_cast<size_t>(std::abs(lhs[index] - rhs[index]));
  }

  return distance;
}

template <typename T, size_t d>
Cube<T, d>::Cube() : m_center(Point<T, d>()), m_radius(0) {};

template <typename T_, size_t d_>
std::ostream& operator<<(std::ostream& os, const Cube<T_, d_>& cube) {
  os << "Center: " << cube.m_center << " Radius: " << cube.m_radius;

  return os;
}

}  // namespace geometry
}  // namespace utils
