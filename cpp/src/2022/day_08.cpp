#include "../utils/input.hpp"

using point = std::pair<int, int>;

enum class Direction { North, East, South, West };

point move(const point &pos, const Direction &direction) {
  auto position = pos;
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

  return position;
}

struct Grid {
  size_t width, height;
  std::vector<int> data;

  Grid(const std::vector<std::string> &input) : width(0), height(0) {
    this->width = input[0].size();
    for (const auto &line : input) {
      assert(line.size() == this->width);
      ++this->height;
      for (const auto &height : line) {
        this->data.push_back(std::stoi(std::string{height}));
      }
    }
    assert(this->data.size() == this->width * this->height);
  }

  int operator[](const point &position) const {
    auto [x, y] = position;
    assert(x >= 0 && x < static_cast<int>(this->width));
    assert(y >= 0 && y < static_cast<int>(this->height));
    return this
        ->data[static_cast<size_t>(x) + static_cast<size_t>(y) * this->width];
  }

  std::set<point> visible(point position, const Direction &direction) const {
    std::set<point> result{};
    int max_height{-1};

    while (std::get<0>(position) >= 0 &&
           std::get<0>(position) < static_cast<int>(this->width) &&
           std::get<1>(position) >= 0 &&
           std::get<1>(position) < static_cast<int>(this->height)) {
      if (this->operator[](position) > max_height) {
        max_height = this->operator[](position);
        result.insert(position);
      }
      position = move(position, direction);
    }

    return result;
  }

  std::set<point> line_of_sight(const point &candidate,
                                const Direction &direction) const {
    std::set<point> result{};
    auto position = move(candidate, direction);
    while (std::get<0>(position) >= 0 &&
           std::get<0>(position) < static_cast<int>(this->width)

           && std::get<1>(position) >= 0 &&
           std::get<1>(position) < static_cast<int>(this->height))

    {
      if (this->operator[](position) < this->operator[](candidate)) {
        result.insert(position);
      } else if (this->operator[](position) >= this->operator[](candidate)) {
        result.insert(position);
        break;
      } else {
        break;
      }
      position = move(position, direction);
    }
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

auto part_two(const std::vector<std::string> &input) {
  Grid grid(input);
  int max_score{0};
  int current_score{0};
  for (int y{0}; y < static_cast<int>(grid.height); ++y) {
    for (int x{0}; x < static_cast<int>(grid.width); ++x) {
      point pos{x, y};
      current_score =
          static_cast<int>(grid.line_of_sight(pos, Direction::North).size()) *
          static_cast<int>(grid.line_of_sight(pos, Direction::East).size()) *
          static_cast<int>(grid.line_of_sight(pos, Direction::South).size()) *
          static_cast<int>(grid.line_of_sight(pos, Direction::West).size());
      if (current_score > max_score) {
        max_score = current_score;
      }
    }
  }
  return max_score;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_08.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
