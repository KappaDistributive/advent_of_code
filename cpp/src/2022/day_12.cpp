#include <queue>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
using utils::geometry::ALL_DIRECTIONS;
using utils::geometry::Direction;

struct Candidate {
  Point m_pos;
  int m_risk;
};

struct Compare {
  bool operator()(const Candidate &lhs, const Candidate &rhs) {
    return lhs.m_risk > rhs.m_risk;
  }
};

char mark(const Direction &direction) {
  std::stringstream ss;
  ss << direction;
  std::string str = ss.str();
  assert(str.size() == 1);
  return str[0];
};

Point step(Point pos, Direction direction) {
  switch (direction) {
  case Direction::Up:
    pos += Point{{0, -1}};
    break;
  case Direction::Right:
    pos += Point{{1, 0}};
    break;
  case Direction::Down:
    pos += Point{{0, 1}};
    break;
  case Direction::Left:
    pos += Point{{-1, 0}};
    break;
  }
  return pos;
}

struct Terrain {
  Point m_start, m_end;
  std::vector<int> m_elevation;
  int m_width, m_height;

  Terrain(const std::vector<std::string> &input) : m_height(0) {

    this->m_width = input[0].size();
    Point pos{{0, 0}};
    for (const auto &line : input) {
      ++this->m_height;
      for (const auto &entry : line) {
        if (entry == 'S') {
          this->m_start = pos;
          // starting point (S) has elevation a
          this->m_elevation.push_back(0);
        } else if (entry == 'E') {
          this->m_end = pos;
          // end point (E) has elevation z
          this->m_elevation.push_back('z' - 'a');
        } else {
          this->m_elevation.push_back(entry - 'a');
        }
        pos += Point{{1, 0}};
      }
      pos += Point{{-pos[0], 1}};
    }
  }

  int operator[](const Point &pos) const {
    return this->m_elevation[pos[0] + this->m_width * pos[1]];
  }

  bool is_reachable(Point pos, const Direction &direction) const {
    auto old_elevation = this->operator[](pos);
    pos = step(pos, direction);
    return (pos[0] >= 0) && (pos[0] < this->m_width) && (pos[1] >= 0) &&
           (pos[1] < this->m_height) &&
           (this->operator[](pos) - old_elevation <= 1);
  }

  std::vector<Point> neighbors(const Point &position) const {
    std::vector<Point> result;
    for (const auto &direction : ALL_DIRECTIONS) {
      if (this->is_reachable(position, direction)) {
        result.push_back(step(position, direction));
      }
    }
    return result;
  }

  auto dijkstra() const {
    std::map<Point, int> minimal_risk;
    for (int y{0}; y < this->m_height; ++y) {
      for (int x{0}; x < this->m_width; ++x) {
        minimal_risk.insert(
            std::make_pair(Point{{x, y}}, std::numeric_limits<int>::max()));
      }
    }

    Point current_node{this->m_start};
    std::priority_queue<Candidate, std::vector<Candidate>, Compare> candidates;
    minimal_risk.at(current_node) = 0;
    candidates.push(Candidate{current_node, 0});

    while (current_node != this->m_end) {
      if (candidates.empty()) {
        return -1;
      }
      current_node = candidates.top().m_pos;
      candidates.pop();
      for (auto neighbor : this->neighbors(current_node)) {
        if (minimal_risk.at(current_node) < std::numeric_limits<int>::max()) {
          if (minimal_risk.at(current_node) + 1 < minimal_risk.at(neighbor)) {
            minimal_risk.at(neighbor) = minimal_risk.at(current_node) + 1;
            candidates.push(Candidate{neighbor, minimal_risk.at(neighbor)});
          }
        }
      }
    }

    return minimal_risk.at(this->m_end);
  }

  friend std::ostream &operator<<(std::ostream &os, const Terrain &terrain) {
    Point pos{{0, 0}};
    while (pos[1] < terrain.m_height) {
      while (pos[0] < terrain.m_width) {
        if (pos == terrain.m_start) {
          os << 'S';
        } else if (pos == terrain.m_end) {
          os << 'E';
        } else {
          os << static_cast<char>('a' + static_cast<char>(terrain[pos]));
        }
        pos += Point{{1, 0}};
      }
      os << '\n';
      pos += Point{{-pos[0], 1}};
    }
    return os;
  }
};

std::vector<Point> unroll(const std::vector<Direction> &path) {
  Point pos{{0, 0}};
  std::vector<Point> locations;
  locations.push_back(pos);
  for (const auto &direction : path) {
    pos = step(pos, direction);
    locations.push_back(pos);
  }
  return locations;
}

std::string visualize(const Terrain &terrain,
                      const std::vector<Direction> &path) {
  auto locations = unroll(path);
  std::string result;
  Point pos{{0, 0}};
  for (int y{0}; y < terrain.m_height; ++y) {
    pos[1] = y;
    for (int x{0}; x < terrain.m_width; ++x) {
      pos[0] = x;
      auto location = std::find(locations.begin(), locations.end(), pos);
      if (pos == terrain.m_end) {
        result.push_back('E');
      } else if (location != locations.end() &&
                 location - locations.begin() <
                     static_cast<long>(path.size())) {
        result.push_back(mark(path[location - locations.begin()]));
      } else {
        result.push_back('.');
      }
    }
    result.push_back('\n');
  }

  return result;
}

auto part_one(const std::vector<std::string> &input) {
  Terrain terrain(input);
  return terrain.dijkstra();
}

auto part_two(const std::vector<std::string> &input) {
  Terrain terrain(input);
  int result{std::numeric_limits<int>::max()};
  Point pos{{0, 0}};
  for (int y{0}; y < terrain.m_height; ++y) {
    for (int x{0}; x < terrain.m_width; ++x) {
      pos = Point{{x, y}};
      if (terrain[pos] == 0) {
        terrain.m_start = pos;
        auto temp = terrain.dijkstra();
        if (temp > 0 && temp < result) {
          result = temp;
        }
      }
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_12_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_12.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
