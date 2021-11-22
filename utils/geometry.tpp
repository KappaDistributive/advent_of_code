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
T Point<T, d>::manhatten_distance(const Point<T, d>& other) const {
  T distance{0};
  auto lhs = this->coordinates();
  auto rhs = other.coordinates();
  for (size_t index{0}; index < d; ++index) {
    distance += static_cast<T>(std::abs(lhs[index] - rhs[index]));
  }

  return distance;
}

template <typename T, size_t d>
RasterCuboid<T, d>::RasterCuboid() : m_base(Point<T, d>()), m_lengths(std::array<T, d-1>()){};

// template <typename T, size_t d>
// std::optional<RasterCuboid<T, d>> intersect(const RasterCuboid<T, d>& other) const {
//   if (this->manhatten_distance(other.center()) >
//       this->radius() + other.radius()) {
//     return std::nullptr;
//   }
// }

template <typename T, size_t d>
bool RasterCuboid<T, d>::operator==(const RasterCuboid<T, d>& rhs) const noexcept {
  return this->m_base == rhs.m_base && this->m_lengths == rhs.m_lengths;
}

template <typename T_, size_t d_>
std::ostream& operator<<(std::ostream& os, const RasterCuboid<T_, d_>& cuboid) {
  os << "Base: " << cuboid.m_base << " Lengths: (";
  for (size_t dimension{1}; dimension < d_; ++dimension) {
    os << cuboid.m_lengths[dimension -1];
    os << ((dimension + 1 < d_) ? ", " : ")");
  }

  return os;
}

}  // namespace geometry
}  // namespace utils
