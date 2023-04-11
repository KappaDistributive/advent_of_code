#include "../../vendors/BigInt/release/BigInt.hpp"
#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<size_t, 2>;

size_t extract_depth(const std::vector<std::string> &input) {
  return std::stoull(input[0].substr(7, std::string::npos));
}

Point extract_target(const std::vector<std::string> &input) {
  auto splits = utils::split_string(input[1], ',');
  assert(splits.size() == 2);
  return Point{{std::stoull(splits[0].substr(8, std::string::npos)),
                std::stoull(splits[1])}};
}

enum class GroundType {
  Rocky,
  Wet,
  Narrow,
};

std::ostream &operator<<(std::ostream &os, const GroundType &ground_type) {
  switch (ground_type) {
  case GroundType::Rocky:
    os << '.';
    break;
  case GroundType::Wet:
    os << '=';
    break;
  case GroundType::Narrow:
    os << '=';
    break;
  }
  return os;
}

class Cave {
private:
  const size_t m_divisor;
  const size_t m_depth;
  const Point m_target;
  std::map<Point, size_t> m_geological_indeces;

public:
  Cave(const std::vector<std::string> &input, size_t divisor = 20183)
      : m_divisor(divisor), m_depth(extract_depth(input)),
        m_target(extract_target(input)) {}

  size_t geological_index(const Point &position) {
    if (this->m_geological_indeces.count(position)) {
      return this->m_geological_indeces.at(position);
    }
    size_t result{0};
    if ((position[0] == 0 && position[1] == 0) || position == this->m_target) {
      result = 0;
    } else if (position[1] == 0) {
      result = position[0] * 16807;
    } else if (position[0] == 0) {
      result = position[1] * 48271;
    } else {
      Point lhs{{position[0] - 1, position[1]}},
          rhs{{position[0], position[1] - 1}};
      result = erosion_level(lhs) * erosion_level(rhs);
    }
    this->m_geological_indeces.emplace(position, result);
    return result;
  }

  size_t erosion_level(const Point &position) {
    return (geological_index(position) + this->m_depth) % this->m_divisor;
  }

  GroundType ground_type(const Point &position) {
    size_t ground_value = this->erosion_level(position) % 3;

    switch (ground_value) {
    case 0:
      return GroundType::Rocky;
      break;
    case 1:
      return GroundType::Wet;
      break;
    case 2:
      return GroundType::Narrow;
      break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
    }
  }

  size_t risk_level() {
    Point pos{{0, 0}};
    size_t result{0};
    for (size_t y{0}; y <= this->m_target.coordinates()[1]; ++y) {
      for (size_t x{0}; x <= this->m_target.coordinates()[0]; ++x) {
        pos = Point{{x, y}};
        auto ground_type = this->ground_type(pos);
        switch (ground_type) {
        case GroundType::Rocky:
          break;
        case GroundType::Wet:
          ++result;
          break;
        case GroundType::Narrow:
          result += 2;
          break;
        default:
          throw std::runtime_error("This should never happen!");
          break;
        }
      }
    }
    return result;
  }

  friend std::ostream &operator<<(std::ostream &os, Cave &cave) {
    Point pos{{0, 0}};
    for (size_t y{0}; y <= cave.m_target.coordinates()[1]; ++y) {
      for (size_t x{0}; x <= cave.m_target.coordinates()[0]; ++x) {
        pos = Point{{x, y}};
        os << cave.ground_type(pos);
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Cave cave{input};
  std::cout << cave << std::endl;
  return cave.risk_level();
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2018/input_22_mock.txt"};
  std::filesystem::path input_path{"../../data/2018/input_22.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
