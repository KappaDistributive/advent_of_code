#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
using Direction = utils::geometry::Direction;

char to_char(Direction direction) noexcept {
  char result = 'x';
  switch (direction) {
  case Direction::Up:
    result = '^';
    break;
  case Direction::Right:
    result = '>';
    break;
  case Direction::Down:
    result = 'v';
    break;
  case Direction::Left:
    result = '<';
    break;
  }
  return result;
}

std::vector<std::string> transform(const std::vector<std::string> &data) {
  std::vector<std::string> result;
  size_t index{0};
  for (; index < data.size(); ++index) {
    auto line = data[index];
    if (line.size() == 0) {
      break;
    }
    std::string new_line;
    new_line.reserve(line.size() * 2);
    for (const auto c : line) {
      switch (c) {
      case '#':
        new_line.push_back('#');
        new_line.push_back('#');
        break;
      case '@':
        new_line.push_back('@');
        new_line.push_back('.');
        break;
      case 'O':
        new_line.push_back('[');
        new_line.push_back(']');
        break;
      case '.':
        new_line.push_back('.');
        new_line.push_back('.');
        break;
      default:
        throw std::runtime_error(std::format("Illegal symbol: {}", c));
      }
    }
    result.push_back(new_line);
  }
  result.push_back("");
  for (; index < data.size(); ++index) {
    result.push_back(data[index]);
  }

  return result;
}

class Grid {
private:
  std::vector<std::string> m_grid;
  std::vector<Direction> m_instructions;
  size_t m_instruction_index;
  int m_verbosity{0};

public:
  Grid(const std::vector<std::string> &input) : m_instruction_index{0} {
    int index{0};
    // parse grid
    for (; index < static_cast<int>(input.size()); ++index) {
      auto line = input[index];
      if (line.size() == 0) {
        ++index;
        break;
      }
      this->m_grid.push_back(line);
    }
    // parse instructions
    for (; index < static_cast<int>(input.size()); ++index) {
      for (const auto direction : input[index]) {
        switch (direction) {
        case '^':
          this->m_instructions.push_back(Direction::Up);
          break;
        case '>':
          this->m_instructions.push_back(Direction::Right);
          break;
        case 'v':
          this->m_instructions.push_back(Direction::Down);
          break;
        case '<':
          this->m_instructions.push_back(Direction::Left);
          break;
        default:
          throw std::runtime_error(
              std::format("Illegal direction: {}", direction));
        }
      }
    }
  }

  int verbosity() const { return this->m_verbosity; }

  int gps(Point point) const {
    const auto coordinates = point.coordinates();
    return coordinates[0] + 100 * coordinates[1];
  }

  int64_t score() const {
    int64_t result{0};
    for (int y{0}; y < static_cast<int>(this->m_grid.size()); ++y) {
      for (int x{0}; x < static_cast<int>(this->m_grid[0].size()); ++x) {
        const Point point{{x, y}};
        if (this->at(point) == 'O' || this->at(point) == '[') {
          result += this->gps(Point{{x, y}});
        }
      }
    }
    return result;
  }

  Point robot() const {
    for (int y{0}; y < static_cast<int>(this->m_grid.size()); ++y) {
      for (int x{0}; x < static_cast<int>(this->m_grid[0].size()); ++x) {
        if (this->m_grid[y][x] == '@') {
          return Point{{x, y}};
        }
      }
    }
    throw std::runtime_error("Unable to find robot!");
  }

  bool step() {
    if (this->m_instruction_index >= this->m_instructions.size()) {
      return false;
    }
    auto instruction = this->m_instructions[this->m_instruction_index++];
    if (this->verbosity() > 0) {
      std::cout << "Move: " << to_char(instruction) << std::endl;
      ;
    }
    auto robot = this->robot();
    auto pos = robot + instruction;
    if (this->at(pos) == '#') {
      return true;
    } else if (this->at(pos) == '.') {
      this->m_grid[robot.coordinates()[1]][robot.coordinates()[0]] = '.';
      robot += instruction;
      this->m_grid[robot.coordinates()[1]][robot.coordinates()[0]] = '@';
      return true;
    } else if (this->at(pos) == 'O') {
      while (this->at(pos) == 'O') {
        pos += instruction;
      }
      if (this->at(pos) == '#') {
        return true;
      }
      this->m_grid[pos.coordinates()[1]][pos.coordinates()[0]] = 'O';
      this->m_grid[robot.coordinates()[1]][robot.coordinates()[0]] = '.';
      robot += instruction;
      this->m_grid[robot.coordinates()[1]][robot.coordinates()[0]] = '@';
      return true;
    }

    std::vector<std::vector<Point>> moving_layers{{{robot}}};
    bool done{false};
    while (!done) {
      auto last_layers = moving_layers[moving_layers.size() - 1];
      std::vector<Point> moving_points;
      for (auto &point : last_layers) {
        auto new_point = point + instruction;
        if (this->at(new_point) == '#') {
          return true;
        } else if (this->at(new_point) == '[') {
          if (std::find(moving_points.cbegin(), moving_points.cend(),
                        new_point) == moving_points.cend()) {
            moving_points.push_back(new_point);
          }
          if (instruction == Direction::Up || instruction == Direction::Down) {
            if (std::find(moving_points.cbegin(), moving_points.cend(),
                          new_point + Point{{1, 0}}) == moving_points.cend()) {
              moving_points.push_back(new_point + Point{{1, 0}});
            }
          }
        } else if (this->at(new_point) == ']') {
          if (std::find(moving_points.cbegin(), moving_points.cend(),
                        new_point) == moving_points.cend()) {
            moving_points.push_back(new_point);
          }
          if (instruction == Direction::Up || instruction == Direction::Down) {
            if (std::find(moving_points.cbegin(), moving_points.cend(),
                          new_point + Point{{-1, 0}}) == moving_points.cend()) {
              moving_points.push_back(new_point + Point{{-1, 0}});
            }
          }
        }
      }
      done = moving_points.size() == 0;
      if (!done) {
        moving_layers.push_back(moving_points);
      }
    }
    std::reverse(moving_layers.begin(), moving_layers.end());
    for (const auto &layer : moving_layers) {
      for (const auto &point : layer) {
        auto new_point = point + instruction;
        this->m_grid[new_point.coordinates()[1]][new_point.coordinates()[0]] =
            this->m_grid[point.coordinates()[1]][point.coordinates()[0]];
        this->m_grid[point.coordinates()[1]][point.coordinates()[0]] = '.';
      }
    }
    return true;
  }

  char at(const Point &pos) const {
    auto coordinates = pos.coordinates();
    return this->m_grid[coordinates[1]][coordinates[0]];
  }

  friend std::ostream &operator<<(std::ostream &os, const Grid &grid) {
    for (int y{0}; y < static_cast<int>(grid.m_grid.size()); ++y) {
      for (int x{0}; x < static_cast<int>(grid.m_grid[0].size()); ++x) {
        Point pos{{x, y}};
        os << grid.m_grid[y][x];
      }
      os << '\n';
    }
    os << '\n';
    if (grid.verbosity() > 0) {
      for (size_t index{0}; index < grid.m_instructions.size(); ++index) {
        if (index == grid.m_instruction_index) {
          os << "\033[36m";
        }
        os << to_char(grid.m_instructions[index]);
        if (index == grid.m_instruction_index) {
          os << "\033[39m";
        }
      }
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &data) {
  Grid grid(data);
  do {
    // std::cout << grid << std::endl;
  } while (grid.step());
  return grid.score();
}

auto part_two(const std::vector<std::string> &data) {
  Grid grid(transform(data));
  do {
    // std::cout << grid << std::endl;
  } while (grid.step());
  return grid.score();
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_15_mock2.txt"};
  std::filesystem::path input_path{"../../data/2024/input_15.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
