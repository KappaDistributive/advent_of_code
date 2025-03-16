#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
using Direction = utils::geometry::Direction;

class Grid {
private:
  Point m_robot;
  std::set<Point> m_walls;
  std::set<Point> m_boxes;
  std::vector<Direction> m_instructions;
  size_t m_instruction_index;

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
      for (int x{0}; x < static_cast<int>(line.size()); ++x) {
        if (line[x] == '#') {
          this->m_walls.insert(Point{{x, index}});
        } else if (line[x] == 'O') {
          this->m_boxes.insert(Point{{x, index}});
        } else if (line[x] == '@') {
          this->m_robot = Point{{x, index}};
        }
      }
    }
    // parse instructions
    assert(index + 1 == static_cast<int>(input.size()));
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
        this->m_instructions.push_back(Direction::Down);
        break;
      default:
        throw std::runtime_error(
            std::format("Illegal direction: {}", direction));
      }
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Grid &grid) {
    int max_x{0}, max_y{0};
    for (const auto &point : grid.m_walls) {
      auto coords = point.coordinates();
      max_x = std::max(max_x, coords[0]);
      max_y = std::max(max_y, coords[1]);
    }
    for (int y{0}; y <= max_y; ++y) {
      for (int x{0}; x <= max_x; ++x) {
        Point pos{{x, y}};
        if (grid.m_robot == pos) {
          os << "\033[36m";
          os << '@';
          os << "\033[39m";
        } else if (grid.m_walls.count(pos)) {
          os << '#';
        } else if (grid.m_boxes.count(pos)) {
          os << 'O';
        } else {
          os << ' ';
        }
      }
      os << '\n';
    }
    os << '\n';
    for (size_t index{0}; index < grid.m_instructions.size(); ++index) {
      if (index == grid.m_instruction_index) {
        os << "\033[36m";
      }
      switch (grid.m_instructions[index]) {
      case Direction::Up:
        os << '^';
        break;
      case Direction::Right:
        os << '>';
        break;
      case Direction::Down:
        os << 'v';
        break;
      case Direction::Left:
        os << '<';
        break;
      }
      if (index == grid.m_instruction_index) {
        os << "\033[39m";
      }
    }
    return os;
  }
};

void parse(const std::vector<std::string> &data) {
  size_t index{0};
  // parse grid
  std::cout << "Grid" << std::endl;
  for (; index < data.size(); ++index) {
    auto line = data[index];
    std::cout << line << std::endl;
    if (line.size() == 0) {
      break;
    }
  }
  // parse instructions
  std::cout << "Instructions" << std::endl;
  for (; index < data.size(); ++index) {
    auto line = data[index];
    std::cout << line << std::endl;
  }
}

auto part_one() { return 1; }

auto part_two() { return 2; }

int main() {
  std::filesystem::path input_path{"../../data/2024/input_15_mock.txt"};
  // std::filesystem::path input_path{"../../data/2024/input_15.txt"};
  utils::Reader reader(input_path);
  Grid grid(reader.get_lines());
  std::cout << grid << std::endl;

  std::cout << std::format("The answer to part one is: {}", part_one())
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
