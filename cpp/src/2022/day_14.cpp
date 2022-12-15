#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

struct Cave {
  std::set<Point> rocks;
  std::set<Point> sand;
  Point upper_left;
  Point lower_right;

  Cave(const std::vector<std::string> &input) {
    for (const auto &line : input) {
      this->add_rock(line);
    }
    this->upper_left = Point{{500, 0}};
    this->lower_right = Point{{500, 0}};
    for (const auto &pos : this->rocks) {
      this->upper_left[0] = std::min(this->upper_left[0], pos[0]);
      this->upper_left[1] = std::min(this->upper_left[1], pos[1]);

      this->lower_right[0] = std::max(this->lower_right[0], pos[0]);
      this->lower_right[1] = std::max(this->lower_right[1], pos[1]);
    }
  }

  void add_rock(const std::string &description) {
    auto splits = utils::split_string(description, ' ');
    std::vector<Point> path;
    for (const auto &split : splits) {
      auto coordinates = utils::split_string(split, ',');
      if (coordinates.size() != 2) {
        continue;
      }
      path.push_back(
          Point{{std::stoi(coordinates[0]), std::stoi(coordinates[1])}});
    }
    assert(path.size() > 1);
    for (size_t index{1}; index < path.size(); ++index) {
      Point source{path[index - 1]};
      Point destination{path[index]};
      Point step{{0, 0}};
      if (source[0] == destination[0] && source[1] < destination[1]) {
        step[1] = 1;
      } else if (source[0] == destination[0] && source[1] > destination[1]) {
        step[1] = -1;
      } else if (source[0] < destination[0] && source[1] == destination[1]) {
        step[0] = 1;
      } else if (source[0] > destination[0] && source[1] == destination[1]) {
        step[0] = -1;
      } else {
        throw std::runtime_error("");
      }

      do {
        this->rocks.insert(source);
        source += step;
      } while (source != destination);
      this->rocks.insert(destination);
    }
  }

  bool is_air(const Point &position) const {
    return this->rocks.count(position) == 0 && this->sand.count(position) == 0;
  }

  bool add_sand() {
    Point position{{500, 0}};
    bool added_sand{false};
    auto max_y = std::get<1>(this->border())[1];
    while (position[1] <= max_y) {
      if (this->is_air(position + Point{{0, 1}})) {
        position += Point{{0, 1}};
      } else if (this->is_air(position + Point{{-1, 1}})) {
        position += Point{{-1, 1}};
      } else if (this->is_air(position + Point{{1, 1}})) {
        position += Point{{1, 1}};
      } else {
        break;
      }
    }

    if (this->is_air(position) && position[1] <= max_y) {
      added_sand = true;
      this->sand.insert(position);
    }

    return added_sand;
  }

  std::pair<Point, Point> border() const {
    return {this->upper_left, this->lower_right};
  }

  friend std::ostream &operator<<(std::ostream &os, const Cave &cave) {
    auto [upper_left, lower_right] = cave.border();
    upper_left += Point{{-2, 0}};
    lower_right += Point{{2, 2}};
    Point pos{{0, 0}};
    // x-axis label
    for (int y{0}; y < 4; ++y) {
      os << "     ";
      for (int x{upper_left[0]}; x < lower_right[0]; ++x) {
        if (x % 5 == 0) {
          std::stringstream ss;
          ss << std::setw(4) << x;
          os << ss.str()[y];
        } else {
          os << ' ';
        }
      }
      os << '\n';
    }

    for (int y{upper_left[1]}; y <= lower_right[1]; ++y) {
      pos[1] = y;
      os << std::setw(4) << y << ' '; // y-axis label
      for (int x{upper_left[0]}; x <= lower_right[0]; ++x) {
        pos[0] = x;
        char mark = '.';
        if (pos[0] == 500 && pos[1] == 0) {
          mark = '+';
        } else if (cave.rocks.count(pos) > 0) {
          mark = '#';
        } else if (cave.sand.count(pos) > 0) {
          mark = 'o';
        }
        os << mark;
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Cave cave(input);
  size_t result{0};
  while (cave.add_sand()) {
    ++result;
    // std::cout << "#Sand: " << result << std::endl;
    // std::cout << cave << std::endl;
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) {
  Cave cave(input);
  auto [upper_left, lower_right] = cave.border();
  cave.add_rock(fmt::format("{},{} -> {},{}", upper_left[0] - 10000,
                            lower_right[1] + 2, lower_right[0] + 10000,
                            lower_right[1] + 2));
  size_t result{0};
  while (cave.add_sand()) {
    ++result;
    std::cout << "#Sand: " << result << std::endl;
    std::cout << cave << std::endl;
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_14_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_14.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
