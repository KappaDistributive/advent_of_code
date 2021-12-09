#include <cassert>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Cave {
 private:
  size_t m_width, m_height;
  std::vector<int> m_heights;

  std::set<Point> neighbors(Point point, bool include_diagonals = true) const {
    std::set<Point> result;
    auto coordinates = point.coordinates();
    for (int offset_y{-1}; offset_y <= 1; ++offset_y) {
      for (int offset_x{-1}; offset_x <= 1; ++offset_x) {
        if ((offset_x == 0 && offset_y == 0) ||
            (!include_diagonals && offset_x != 0 && offset_y != 0) ||
            (offset_x + coordinates[0] < 0) ||
            (offset_x + coordinates[0] >= this->m_width) ||
            (offset_y + coordinates[1] < 0) ||
            (offset_y + coordinates[1] >= this->m_height)) {
          continue;
        }
        Point offset{std::array<int, 2>{offset_x, offset_y}};
        result.insert(point + offset);
      }
    }
    return result;
  }

 public:
  explicit Cave(const std::vector<std::string>& input) : m_height(0) {
    this->m_width = input[0].size();
    for (auto line : input) {
      assert(line.size() == this->m_width);
      ++this->m_height;
      for (auto character : line) {
        this->m_heights.push_back(static_cast<int>(character - '0'));
      }
    }
  }

  int heights(Point point) const {
    auto coordinates = point.coordinates();
    return this->m_heights[coordinates[1] * this->m_width + coordinates[0]];
  }

  bool is_low_point(Point point) const {
    bool result{true};
    auto coordinates = point.coordinates();
    for (auto neighbor : this->neighbors(point)) {
      if (this->heights(neighbor) <= this->heights(point)) {
        result = false;
      }
    }
    return result;
  }

  int risk_levels() {
    int risk_level{0};
    for (int y{0}; y < this->m_height; ++y) {
      for (int x{0}; x < this->m_width; ++x) {
        Point point{std::array<int, 2>{x, y}};
        if (this->is_low_point(point)) {
          risk_level += this->heights(point) + 1;
        }
      }
    }

    return risk_level;
  }

  auto basin_at(Point point) {
    std::set<Point> result;
    if (this->heights(point) == 9) {
      return result;
    }
    result.insert(point);

    bool expanding{true};
    while (expanding) {
      expanding = false;
      std::set<Point> expansions;
      for (auto location : result) {
        for (auto neighbor : this->neighbors(location, false)) {
          if (this->heights(neighbor) < 9 && result.count(neighbor) == 0) {
            expansions.insert(neighbor);
          }
        }
      }
      expanding = expansions.size() > 0;
      for (auto location : expansions) {
        result.insert(location);
      }
    }

    return result;
  }

  auto basins() {
    std::set<std::set<Point>> result;
    for (int y{0}; y < this->m_height; ++y) {
      for (int x{0}; x < this->m_width; ++x) {
        result.insert(this->basin_at(Point{std::array<int, 2>{x, y}}));
      }
    }
    return result;
  }

  friend std::ostream& operator<<(std::ostream& os, const Cave& cave) {
    for (int y{0}; y < cave.m_height; ++y) {
      for (int x{0}; x < cave.m_width; ++x) {
        Point point{std::array<int, 2>{x, y}};
        if (cave.is_low_point(point)) {
          os << "\033[1m";
        }
        os << cave.heights(point);
        if (cave.is_low_point(point)) {
          os << "\033[0m";
        }
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  Cave cave(input);

  std::cout << cave << std::endl;

  return cave.risk_levels();
}

auto part_two(const std::vector<std::string>& input) {
  Cave cave(input);
  auto basins = cave.basins();

  std::vector<size_t> sizes;
  for (auto basin : basins) {
    sizes.push_back(basin.size());
  }
  std::sort(sizes.begin(), sizes.end(), std::greater<>());

  size_t result{1};
  for (size_t index{0}; index < 3; ++index) {
    result *= sizes[index];
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../2021/data/input_09_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_09.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
