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
void Point<T, d>::operator=(const Point<T, d>& other) {
  assert(this->m_coordinates.size() == d);
  assert(other.m_coordinates.size() == d);
  this->m_coordinates = other.m_coordinates;
}

template <typename T, size_t d>
bool Point<T, d>::operator==(const Point<T, d>& other) const {
  return this->m_coordinates == other.m_coordinates;
}

template <typename T, size_t d>
bool Point<T, d>::operator!=(const Point<T, d>& other) const {
  return !(*this == other);
}

template <typename T, size_t d>
bool Point<T, d>::operator<(const Point<T, d>& other) const {
  auto coordinates_lhs = this->coordinates();
  auto coordinates_rhs = other.coordinates();

  for (size_t index{0}; index < d; ++index) {
    if (coordinates_lhs[index] < coordinates_rhs[index]) {
      return true;
    } else if (coordinates_lhs[index] > coordinates_rhs[index]) {
      return false;
    }
  }
  return false;
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
Point<T, d>& Point<T, d>::operator+=(const Point<T, d>& other) {
  return this->operator+=(other.coordinates());
}

template <typename T, size_t d>
Point<T, d>& Point<T, d>::operator+=(const std::array<T, d>& coordinates) {
  for (size_t index{0}; index < d; ++index) {
    this->m_coordinates[index] += coordinates[index];
  }
  return *this;
}

template <typename T, size_t d>
Point<T, d> operator-(const Point<T, d>& lhs, const Point<T, d>& rhs) {
  assert(lhs.m_coordinates.size() == d);
  assert(rhs.m_coordinates.size() == d);

  std::array<T, d> coordinates;
  for (size_t index{0}; index < d; ++index) {
    coordinates[index] = lhs.m_coordinates[index] - rhs.m_coordinates[index];
  }
  Point<T, d> result(coordinates);
  return result;
}

template <typename T, size_t d>
Point<T, d> Point<T, d>::operator-() {
  std::array<T, d> coordinates;
  for (size_t index{0}; index < d; ++index) {
    coordinates[index] = -this->m_coordinates[index];
  }

  return Point<T, d>{coordinates};
}

template <typename T, size_t d>
Point<T, d>& Point<T, d>::operator-=(const Point<T, d>& other) {
  return this->operator-=(other.coordinates());
}

template <typename T, size_t d>
Point<T, d>& Point<T, d>::operator-=(const std::array<T, d>& coordinates) {
  for (size_t index{0}; index < d; ++index) {
    this->m_coordinates[index] -= coordinates[index];
  }
  return *this;
}

template <typename T, size_t d>
Point<T, d>& Point<T, d>::operator*=(const T& factor) {
  for (size_t index{0}; index < d; ++index) {
    this->m_coordinates[index] *= factor;
  }
  return *this;
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
RasterCuboid<T, d>::RasterCuboid() : m_base(Point<T, d>()) {
  std::fill(this->m_lengths.begin(), this->m_lengths.end(), 1);
};

template <typename T, size_t d>
RasterCuboid<T, d>::RasterCuboid(const Point<T, d>& base,
                                 const std::array<T, d>& lengths)
    : m_base(base), m_lengths(lengths) {}

template <typename T, size_t d>
RasterCuboid<T, d>::RasterCuboid(
    const std::array<Point<T, d>, num_corners<d>>& corners) {
  std::array<std::set<T>, d> borders;
  for (auto corner : corners) {
    auto coordinates = corner.coordinates();
    for (size_t dimension{0}; dimension < d; ++dimension) {
      borders[dimension].insert(coordinates[dimension]);
    }
  }
  std::array<T, d> base_coordinates;

  for (size_t dimension{0}; dimension < d; ++dimension) {
    auto min =
        *std::min_element(borders[dimension].begin(), borders[dimension].end());
    auto max =
        *std::max_element(borders[dimension].begin(), borders[dimension].end());
    base_coordinates[dimension] = min;
    this->m_lengths[dimension] = max - min;
  }

  this->m_base = Point<T, d>(base_coordinates);

  //////////////////////////
  // Perform a sanity check.
  //////////////////////////
  // Check that our RasterCuboid contains all corners
  auto got_corners = this->corners();
  for (auto corner : corners) {
    if (std::find(got_corners.begin(), got_corners.end(), corner) ==
        got_corners.end()) {
      std::stringstream ss;
      ss << "Encountered inconsistent corner information!\n";
      ss << "The provided corner " << corner
         << " is not contained in the RasterCuboid that has been "
            "constructed:\n";
      for (auto it{got_corners.begin()}; it != got_corners.end(); ++it) {
        ss << *it;
        if (std::next(it) != got_corners.end()) {
          ss << ", ";
        }
      }
      throw std::runtime_error(ss.str());
    }
  }

  // Check that our RasterCuboid only contains provided corners.
  for (auto corner : got_corners) {
    if (std::find(corners.begin(), corners.end(), corner) == corners.end()) {
      std::stringstream ss;
      ss << "Encountered inconsistent corner information!\n";
      ss << "The constructed corner " << corner
         << " is not contained in the corners you've provided:\n";
      for (auto it{corners.begin()}; it != corners.end(); ++it) {
        ss << *it;
        if (std::next(it) != corners.end()) {
          ss << ", ";
        }
      }
      throw std::runtime_error(ss.str());
    }
  }
}

template <typename T, size_t d>
RasterCuboid<T, d>::RasterCuboid(
    const std::array<std::pair<T, T>, d>& intervals) {
  std::array<T, d> base_coordinates;
  for (size_t dimension{0}; dimension < d; ++dimension) {
    base_coordinates[dimension] = intervals[dimension].first;
    assert(intervals[dimension].first <= intervals[dimension].second);
    this->m_lengths[dimension] =
        intervals[dimension].second - intervals[dimension].first;
  }

  this->m_base = Point<T, d>(base_coordinates);
}

template <typename T, size_t d>
std::optional<RasterCuboid<T, d>> RasterCuboid<T, d>::intersect(
    const RasterCuboid<T, d>& other) const {
  auto intervals_lhs = this->intervals();
  auto intervals_rhs = other.intervals();
  std::array<std::pair<T, T>, d> intervals;

  for (size_t dimension{0}; dimension < d; ++dimension) {
    intervals[dimension] =
        std::make_pair(std::max(intervals_lhs[dimension].first,
                                intervals_rhs[dimension].first),
                       std::min(intervals_lhs[dimension].second,
                                intervals_rhs[dimension].second));
    if (intervals[dimension].first > intervals[dimension].second) {
      return std::nullopt;
    }
  }

  return RasterCuboid<T, d>(intervals);
}

template <typename T, size_t d>
Point<T, d> RasterCuboid<T, d>::corner(std::bitset<d> corner) const {
  std::array<T, d> offset;
  for (size_t index{0}; index < d; ++index) {
    offset[index] = corner[d - index - 1] ? this->m_lengths[index] : 0;
  }

  return this->m_base + Point<T, d>(offset);
}

template <typename T, size_t d>
std::array<Point<T, d>, num_corners<d>> RasterCuboid<T, d>::corners() const {
  std::array<Point<T, d>, num_corners<d>> result;

  for (size_t index{0}; index < num_corners<d>; ++index) {
    result[index] = this->corner(std::bitset<d>(index));
  }

  return result;
}

template <typename T, size_t d>
std::array<std::pair<T, T>, d> RasterCuboid<T, d>::intervals() const {
  std::array<std::pair<T, T>, d> result;

  auto min_point_coordinates = this->corner(std::bitset<d>(0)).coordinates();
  auto max_point_coordinates = this->corner(~std::bitset<d>(0)).coordinates();

  for (size_t dimension{0}; dimension < d; ++dimension) {
    result[dimension] = std::make_pair(min_point_coordinates[dimension],
                                       max_point_coordinates[dimension]);
  }

  return result;
}

template <typename T, size_t d>
bool RasterCuboid<T, d>::operator==(
    const RasterCuboid<T, d>& other) const noexcept {
  return this->m_base == other.m_base && this->m_lengths == other.m_lengths;
}

template <typename T, size_t d>
bool RasterCuboid<T, d>::operator!=(
    const RasterCuboid<T, d>& other) const noexcept {
  return !(*this == other);
}

template <typename T_, size_t d_>
std::ostream& operator<<(std::ostream& os, const RasterCuboid<T_, d_>& cuboid) {
  os << "Base: " << cuboid.m_base << " Lengths: (";
  for (size_t dimension{0}; dimension < d_; ++dimension) {
    os << cuboid.m_lengths[dimension];
    os << ((dimension + 1 < d_) ? ", " : ")");
  }

  return os;
}

}  // namespace geometry
}  // namespace utils
