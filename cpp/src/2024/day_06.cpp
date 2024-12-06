#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

std::array<Point, 4> directions{
    {Point{{0, -1}}, Point{{1, 0}}, Point{{0, 1}}, Point{{-1, 0}}}};
std::array<char, 4> arrows{'^', '>', 'v', '<'};

Point turn_right(Point direction) {
  auto pos = std::find(directions.begin(), directions.end(), direction);
  return directions[(pos - directions.begin() + 1) % directions.size()];
}

class Guard {
private:
  size_t m_width, m_height;
  std::vector<Point> m_obstacles;
  Point m_position;
  Point m_direction;

public:
  Guard(const std::vector<std::string> &map) {
    this->m_height = map.size();
    assert(this->m_height > 0);
    this->m_width = map[0].size();

    for (int y{0}; y < static_cast<int>(this->m_height); ++y) {
      for (int x{0}; x < static_cast<int>(this->m_width); ++x) {
        if (map[y][x] == '#') {
          this->m_obstacles.push_back(Point{{x, y}});
        } else if (map[y][x] != '.') {
          this->m_position = Point{{x, y}};
          switch (map[y][x]) {
          case '^':
            this->m_direction = Point{{0, -1}};
            break;
          case '>':
            this->m_direction = Point{{1, 0}};
            break;
          case 'v':
            this->m_direction = Point{{0, 1}};
            break;
          case '<':
            this->m_direction = Point{{-1, 0}};
            break;
          default:
            assert(false);
          }
        }
      }
    }
  }

  char at(const Point &pos) const {
    return std::find(this->m_obstacles.begin(), this->m_obstacles.end(), pos) ==
                   this->m_obstacles.end()
               ? '.'
               : '#';
  }

  Point position() const { return this->m_position; }

  Point direction() const { return this->m_direction; }

  void insert_obstacle(const Point &pos) { this->m_obstacles.push_back(pos); }

  void orient() {
    auto attempt = this->m_position + this->m_direction;
    if (this->at(attempt) == '#') {
      this->m_direction = turn_right(this->m_direction);
      orient();
    }
  }

  bool step() {
    auto attempt = this->m_position + this->m_direction;
    if (attempt[0] < 0 || attempt[0] >= static_cast<int>(this->m_width) ||
        attempt[1] < 0 || attempt[1] >= static_cast<int>(this->m_height)) {
      return false;
    }
    if (this->at(attempt) == '#') {
      this->orient();
      return this->step();
    }
    this->m_position = attempt;
    this->orient();
    return true;
  }

  friend std::ostream &operator<<(std::ostream &os, const Guard &guard) {
    for (int y{0}; y < static_cast<int>(guard.m_height); ++y) {
      for (int x{0}; x < static_cast<int>(guard.m_width); ++x) {
        if (Point{{x, y}} == guard.m_position) {
          os << arrows[std::find(directions.begin(), directions.end(),
                                 guard.m_direction) -
                       directions.begin()];
        } else {
          os << guard.at(Point{{x, y}});
        }
      }
      os << std::endl;
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Guard guard{input};
  std::vector<Point> unique_visited;
  do {
    auto pos = guard.position();
    if (std::find(unique_visited.begin(), unique_visited.end(), pos) ==
        unique_visited.end()) {
      unique_visited.push_back(pos);
    }
  } while (guard.step());
  return unique_visited.size();
}

std::pair<bool, std::vector<std::pair<Point, Point>>> trace(Guard guard) {
  bool loop = false;
  std::vector<std::pair<Point, Point>> result;
  auto pos = guard.position();
  auto dir = guard.direction();
  result.push_back({pos, dir});
  while (guard.step()) {
    auto pos = guard.position();
    auto dir = guard.direction();
    loop = std::find(result.begin(), result.end(), std::make_pair(pos, dir)) !=
           result.end();
    result.push_back({pos, dir});
    if (loop) {
      break;
    }
  }

  return {loop, result};
}

auto part_two(const std::vector<std::string> &input) {
  Guard guard{input};
  auto unmodified_trace = trace(guard).second;
  std::vector<std::pair<Point, Point>> valid_obstacles;

  for (auto [pos, dir] : unmodified_trace) {
    if (std::make_pair(pos, dir) == unmodified_trace[0]) {
      continue;
    }
    Guard guard{input};
    guard.insert_obstacle(pos);
    auto [loop, new_trace] = trace(guard);
    if (loop) {
      valid_obstacles.push_back({pos, dir});
    }
  }
  size_t result{0};
  for (size_t index{0}; index < valid_obstacles.size(); ++index) {
    bool new_obstacle = true;
    for (size_t check_dup{0}; check_dup < index; ++check_dup) {
      if (valid_obstacles[check_dup].first == valid_obstacles[index].first) {
        new_obstacle = false;
        break;
      }
    }
    if (new_obstacle) {
      ++result;
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_06_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_06.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
