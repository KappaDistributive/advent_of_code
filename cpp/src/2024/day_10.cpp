#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
using Direction = utils::geometry::Direction;

class Map {
private:
  std::vector<int> m_map;
  const int m_width, m_height;

public:
  Map(const std::vector<std::string> &map)
      : m_width(static_cast<int>(map[0].size())),
        m_height(static_cast<int>(map.size())) {
    for (const auto &line : map) {
      for (const auto &c : line) {
        assert(c >= '0' && c <= '9');
        this->m_map.push_back(c - '0');
      }
    }
    assert(static_cast<int>(this->m_map.size()) ==
           this->m_width * this->m_height);
  }

  int at(const Point &position) const {
    if (position[0] < 0 || position[0] >= this->m_width || position[1] < 0 ||
        position[1] >= this->m_height) {
      return -1;
    }
    return this->m_map[position[1] * this->m_width + position[0]];
  }

  std::set<Point> peaks(const Point &position) const {
    if (this->at(position) == 9) {
      return std::set<Point>{{position}};
    }
    std::set<Point> peaks;
    for (const auto &direction : {Direction::North, Direction::East,
                                  Direction::South, Direction::West}) {
      auto next = position + direction;
      if (this->at(next) == this->at(position) + 1) {
        peaks.merge(this->peaks(next));
      }
    }
    return peaks;
  }

  int score() const {
    int result{0};
    for (int y = 0; y < m_height; ++y) {
      for (int x = 0; x < m_width; ++x) {
        if (this->at(Point{{x, y}}) == 0) {
          result += this->peaks(Point{{x, y}}).size();
          // std::cout << std::format("Trailhead: ({}, {}), score: {}\n", x, y,
          //                          this->peaks(Point{{x, y}}).size());
        }
      }
    }
    return result;
  }

  std::set<std::vector<Point>> trails(const Point &position) const {
    if (this->at(position) == 9) {
      return std::set<std::vector<Point>>{{position}};
    }
    std::set<std::vector<Point>> trails;
    for (const auto &direction : {Direction::North, Direction::East,
                                  Direction::South, Direction::West}) {
      auto next = position + direction;
      if (this->at(next) == this->at(position) + 1) {
        for (auto trail : this->trails(next)) {
          trail.push_back(position);
          trails.insert(trail);
        }
      }
    }
    return trails;
  }

  int rating() const {
    int result{0};
    for (int y = 0; y < m_height; ++y) {
      for (int x = 0; x < m_width; ++x) {
        if (this->at(Point{{x, y}}) == 0) {
          result += this->trails(Point{{x, y}}).size();
          // std::cout << std::format("Trailhead: ({}, {}), trails: {}\n", x, y,
          //                          this->trails(Point{{x, y}}).size());
        }
      }
    }
    return result;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Map map(input);
  return map.score();
}

auto part_two(const std::vector<std::string> &input) {
  Map map(input);
  return map.rating();
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_10_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_10.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
