#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
using utils::geometry::ALL_DIRECTIONS;
using utils::geometry::Direction;

class Map {
private:
  const std::vector<std::string> &m_map;
  int m_height, m_width;

public:
  Map(const std::vector<std::string> &map)
      : m_map(map), m_height(static_cast<int>(map.size())),
        m_width(static_cast<int>(map[0].size())) {}

  char at(const Point &pos) const {
    if (pos[0] < 0 || pos[0] >= this->m_width || pos[1] < 0 ||
        pos[1] >= this->m_height) {
      return ' ';
    }
    return this->m_map[pos[1]][pos[0]];
  }

  int num_neighbors(const Point &pos) const {
    auto symbol = this->at(pos);
    int count{0};
    for (auto direction : ALL_DIRECTIONS) {
      if (this->at(pos + direction) == symbol) {
        ++count;
      }
    }
    return count;
  }

  int height() const { return this->m_height; }

  int width() const { return this->m_width; }

  std::set<Point> flood_fill(const Point &pos) const {
    std::set<Point> visited;
    std::queue<Point> queue;
    queue.push(pos);
    visited.insert(pos);
    while (!queue.empty()) {
      auto current = queue.front();
      queue.pop();
      for (auto direction : ALL_DIRECTIONS) {
        auto neighbor = current + direction;
        if (this->at(neighbor) == this->at(pos) &&
            visited.find(neighbor) == visited.end()) {
          queue.push(neighbor);
          visited.insert(neighbor);
        }
      }
    }
    return visited;
  }

  std::vector<std::set<std::pair<Point, int>>> regions() const {
    std::vector<std::set<std::pair<Point, int>>> regions;
    std::set<Point> visited;
    for (int y{0}; y < this->height(); ++y) {
      for (int x{0}; x < this->width(); ++x) {
        Point pos{{x, y}};
        if (visited.find(pos) == visited.end()) {
          auto region = this->flood_fill(pos);
          regions.push_back(enhance_region(region));
          visited.insert(region.begin(), region.end());
        }
      }
    }
    return regions;
  }

  std::set<std::pair<Point, int>>
  enhance_region(const std::set<Point> &region) const {
    std::set<std::pair<Point, int>> result;
    for (const auto &point : region) {
      result.insert({point, this->num_neighbors(point)});
    }
    return result;
  }

  int interior_corners(const Point &pos) const {
    int count{0};
    // xX
    // .x
    if (this->at(pos) == this->at(pos + Direction::Left) &&
        this->at(pos) == this->at(pos + Direction::Down) &&
        this->at(pos) != this->at(pos + Direction::Left + Direction::Down)) {
      ++count;
    }
    // Xx
    // x.
    if (this->at(pos) == this->at(pos + Direction::Down) &&
        this->at(pos) == this->at(pos + Direction::Right) &&
        this->at(pos) != this->at(pos + Direction::Down + Direction::Right)) {
      ++count;
    }
    // x.
    // Xx
    if (this->at(pos) == this->at(pos + Direction::Up) &&
        this->at(pos) == this->at(pos + Direction::Right) &&
        this->at(pos) != this->at(pos + Direction::Up + Direction::Right)) {
      ++count;
    }
    // .x
    // xX
    if (this->at(pos) == this->at(pos + Direction::Up) &&
        this->at(pos) == this->at(pos + Direction::Left) &&
        this->at(pos) != this->at(pos + Direction::Up + Direction::Left)) {
      ++count;
    }
    return count;
  }

  int exterior_corners(const Point &pos) const {
    int count{0};
    // -+
    //  |
    if (this->at(pos) != this->at(pos + Direction::Left) &&
        this->at(pos) != this->at(pos + Direction::Down)) {
      ++count;
    }
    // +-
    // |
    if (this->at(pos) != this->at(pos + Direction::Down) &&
        this->at(pos) != this->at(pos + Direction::Right)) {
      ++count;
    }
    //  |
    // -+
    if (this->at(pos) != this->at(pos + Direction::Left) &&
        this->at(pos) != this->at(pos + Direction::Up)) {
      ++count;
    }
    // |
    // +-
    if (this->at(pos) != this->at(pos + Direction::Up) &&
        this->at(pos) != this->at(pos + Direction::Right)) {
      ++count;
    }

    return count;
  }

  int sides(const std::set<std::pair<Point, int>> &region) {
    int count{0};
    for (const auto &[point, _] : region) {
      count += interior_corners(point) + exterior_corners(point);
    }
    return count;
  }
};

int perimiter(const std::set<std::pair<Point, int>> &region) {
  int count{0};
  for (const auto &[point, neighbors] : region) {
    count += 4 - neighbors;
  }
  return count;
}

auto part_one(const std::vector<std::string> &input) {
  Map map(input);
  auto regions = map.regions();
  int result{0};
  for (const auto &region : regions) {
    result += perimiter(region) * region.size();
  }

  return result;
}

auto part_two(const std::vector<std::string> &input) {
  Map map(input);
  auto regions = map.regions();
  int result{0};
  for (const auto &region : regions) {
    result += map.sides(region) * region.size();
  }

  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_12_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_12.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return EXIT_SUCCESS;
}
