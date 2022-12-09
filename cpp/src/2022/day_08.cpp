#include "../utils/input.hpp"

using point = std::pair<int, int>;

enum class Direction { North, East, South, West };

struct Grid {
  size_t width, height;
  std::vector<int> data;

  Grid(const std::vector<std::string> &input) : width(0) {
    this->width = input[0].size();
    for (const auto &line : input) {
      assert(line.size() == this->width);
      ++height;
      for (const auto &height : line) {
        data.push_back(std::stoi(std::string{height}));
      }
    }
  }

  int operator[](const point &position) const {
    return this
        ->data[std::get<0>(position) + std::get<1>(position) * this->width];
  }

  std::set<point> visible(point position, const Direction &direction) const {
    std::set<point> result{};
    int max_height{-1};

    do {
      if (this->operator[](position) > max_height) {
        max_height = this->operator[](position);
        result.insert(position);
      }
      switch (direction) {
      case Direction::North:
        std::get<1>(position)--;
        break;
      case Direction::East:
        std::get<0>(position)++;
        break;
      case Direction::South:
        std::get<1>(position)++;
        break;
      case Direction::West:
        std::get<0>(position)--;
        break;
      }
    } while (std::get<0>(position) >= 0 &&
             std::get<0>(position) < static_cast<int>(this->width)

             && std::get<1>(position) >= 0 &&
             std::get<1>(position) < static_cast<int>(this->height));

    return result;
  }
};
auto part_one(const std::vector<std::string> &input) {
  Grid grid(input);
  std::set<point> visible;
  for (int y{0}; y < static_cast<int>(grid.height); ++y) {
    for (int x{0}; x < static_cast<int>(grid.width); ++x) {
      point pos{x, y};
      if (y == 0) {
        auto candidates = grid.visible(pos, Direction::South);
        visible.insert(candidates.cbegin(), candidates.cend());
      } else if (y + 1 == static_cast<int>(grid.height)) {
        auto candidates = grid.visible(pos, Direction::North);
        visible.insert(candidates.cbegin(), candidates.cend());
      }
      if (x == 0) {
        auto candidates = grid.visible(pos, Direction::East);
        visible.insert(candidates.cbegin(), candidates.cend());
      } else if (x + 1 == static_cast<int>(grid.width)) {
        auto candidates = grid.visible(pos, Direction::West);
        visible.insert(candidates.cbegin(), candidates.cend());
      }
    }
  }
  return visible.size();
}

// auto part_two(const std::string &input) {
//   return 2;
// }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_08.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  // fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
