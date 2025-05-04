#include "../utils/geometry.hpp"
#include "../utils/input.hpp"
#include <sstream>

using utils::geometry::ALL_DIRECTIONS;
using utils::geometry::Direction;
using utils::geometry::rotate_clockwise;
using utils::geometry::rotate_counter_clockwise;
using Point = utils::geometry::Point<int, 2>;

struct Reindeer {
  Point position;
  Direction direction = Direction::Right;

  char show() const noexcept {
    if (this->direction == Direction::Up) {
      return '^';
    } else if (this->direction == Direction::Down) {
      return 'v';
    } else if (this->direction == Direction::Left) {
      return '<';
    } else if (this->direction == Direction::Right) {
      return '>';
    }
    return 'R';
  }

  bool operator==(const Reindeer &other) const noexcept {
    return this->position == other.position &&
           this->direction == other.direction;
  }

  bool operator<(const Reindeer &other) const noexcept {
    if (this->position < other.position) {
      return true;
    } else if (this->position == other.position) {
      return this->direction < other.direction;
    }
    return false;
  }
};

class Map {
private:
  int m_width, m_height;
  std::vector<std::string> m_data;
  Point m_start, m_end;

public:
  Map(const std::vector<std::string> &data) {
    this->m_height = data.size();
    this->m_width = data[0].size();
    this->m_data.reserve(this->m_height);
    for (int y{0}; y < this->m_height; y++) {
      std::string line;
      line.reserve(this->m_width);
      for (int x{0}; x < this->m_width; x++) {
        char c = data[y][x];
        if (c == 'E') {
          this->m_end = Point{{x, y}};
          line.push_back('.');
        } else if (c == 'S') {
          this->m_start = Point{{x, y}};
          line.push_back('.');
        } else {
          line.push_back(c);
        }
      }
      assert(static_cast<int>(line.size()) == this->m_width);
      this->m_data.push_back(line);
    }
  }

  char at(int x, int y) const noexcept {
    if (x < 0 || x >= this->m_width || y < 0 || y >= this->m_height) {
      return ' ';
    }
    return this->m_data[y][x];
  }

  char at(const Point &position) const noexcept {
    auto coordinates = position.coordinates();
    return this->at(coordinates[0], coordinates[1]);
  }

  int width() const noexcept { return this->m_width; }

  int height() const noexcept { return this->m_height; }

  Point start() const noexcept { return this->m_start; }

  Point end() const noexcept { return this->m_end; }

  bool legal_path(const std::vector<Reindeer> &path) const noexcept {
    if (path.size() == 0)
      return true;
    if (path[0].position != this->start() ||
        path[0].direction != Direction::Right)
      return false;
    for (size_t index{0}; index + 1 < path.size(); ++index) {
      const auto &before = path[index];
      const auto &after = path[index + 1];
      if (before.position == after.position) {
        if (before.direction == after.direction) {
          return false;
        }
        if (rotate_clockwise(before.direction) != after.direction ||
            rotate_counter_clockwise(before.direction) != after.direction) {
          return false;
        }
      } else {
        if (before.position + Direction::Up != after.position &&
            before.position + Direction::Right != after.position &&
            before.position + Direction::Down != after.position &&
            before.position + Direction::Left != after.position) {
          return false;
        }
      }
    }
    return true;
  }

  int score(const std::vector<Reindeer> &path) const noexcept {
    int result{0};
    if (path.size() == 0)
      return 0;
    if (!legal_path(path))
      return -1;
    result = static_cast<int>(path.size()) - 1;
    for (size_t index{0}; index + 1 < path.size(); ++index) {
      const auto &before = path[index];
      const auto &after = path[index + 1];
      if (before.direction != after.direction) {
        result += 1000;
      }
    }
    return result;
  }

  std::vector<Reindeer> expansions(Reindeer reindeer) const noexcept {
    std::vector<Reindeer> result;
    result.push_back(
        Reindeer{reindeer.position, rotate_clockwise(reindeer.direction)});
    result.push_back(Reindeer{reindeer.position,
                              rotate_counter_clockwise(reindeer.direction)});
    reindeer.position += reindeer.direction;
    if (this->at(reindeer.position) == '.') {
      result.push_back(reindeer);
    }
    return result;
  }

  std::ostringstream
  show(const std::vector<Reindeer> &path = {}) const noexcept {
    std::ostringstream os;
    for (int y{0}; y < this->m_height; ++y) {
      for (int x{0}; x < this->m_width; ++x) {
        Point position{{x, y}};
        auto it = std::find_if(path.cbegin(), path.cend(),
                               [&position](const Reindeer &reindeer) {
                                 return reindeer.position == position;
                               });
        if (it != path.cend()) {
          os << it->show();
        } else {
          if (position == this->m_start || position == this->m_end) {
            os << "\033[3m";
            ;
          }
          os << this->at(x, y);
          if (position == this->m_start || position == this->m_end) {
            os << "\033[23m";
          }
        }
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &data) {
  Map map(data);
  std::map<Reindeer, int64_t> visited;
  std::map<Reindeer, int64_t> unvisited;

  // Initialize the unvisited queue with all positions
  for (int y{0}; y < map.height(); ++y) {
    for (int x{0}; x < map.width(); ++x) {
      Point position{{x, y}};
      if (map.at(position) == '.') {
        for (auto direction : ALL_DIRECTIONS) {
          if (position == map.start() && direction == Direction::Right) {
            unvisited.insert({Reindeer{position, direction}, 0});
          } else {
            unvisited.insert({Reindeer{position, direction},
                              std::numeric_limits<int64_t>::max()});
          }
        }
      }
    }
  }

  while (!unvisited.empty()) {
    auto it = std::min_element(
        unvisited.begin(), unvisited.end(),
        [](const auto &a, const auto &b) { return a.second < b.second; });
    auto [current, score] = *it;
    visited.insert({current, score});
    unvisited.erase(current);

    for (auto next : map.expansions(current)) {
      if (visited.find(next) != visited.end()) {
        continue;
      }
      int weight = next.position == current.position ? 1000 : 1;
      unvisited[next] = std::min(unvisited[next], visited[current] + weight);
    }
  }
  int64_t min_score = std::numeric_limits<int64_t>::max();
  for (const auto &[reindeer, score] : visited) {
    if (reindeer.position == map.end()) {
      min_score = std::min(min_score, score);
    }
  }
  return min_score;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_16_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_16.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
