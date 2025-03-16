#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;
using Direction = utils::geometry::Direction;

class Grid {
private:
  Point m_robot;
  std::vector<Point> m_walls;
  std::vector<Point> m_boxes;
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
          this->m_walls.push_back(Point{{x, index}});
        } else if (line[x] == 'O') {
          this->m_boxes.push_back(Point{{x, index}});
        } else if (line[x] == '@') {
          this->m_robot = Point{{x, index}};
        }
      }
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

  int gps(Point point) const {
    const auto coordinates = point.coordinates();
    return coordinates[0] + 100 * coordinates[1];
  }

  int64_t score() const {
    int64_t result{0};
    for (auto const &box : this->m_boxes) {
      result += this->gps(box);
    }
    return result;
  }

  bool step() {
    if (this->m_instruction_index >= this->m_instructions.size()) {
      return false;
    }
    auto instruction = this->m_instructions[this->m_instruction_index];

    auto new_pos = this->m_robot + instruction;
    if (std::find(this->m_walls.cbegin(), this->m_walls.cend(), new_pos) !=
        this->m_walls.cend()) {
      // robot attempts to move into wall
    } else if (std::find(this->m_boxes.cbegin(), this->m_boxes.cend(),
                         new_pos) != this->m_boxes.cend()) {
      // robot is moving into box
      auto trail = new_pos;
      while (std::find(this->m_boxes.cbegin(), this->m_boxes.cend(), trail) !=
             this->m_boxes.cend()) {
        trail += instruction;
      }
      if (std::find(this->m_walls.cbegin(), this->m_walls.cend(), trail) ==
          this->m_walls.cend()) {
        this->m_robot += instruction;
        do {
          trail -= instruction;
          auto it =
              std::find(this->m_boxes.begin(), this->m_boxes.end(), trail);
          *it += instruction;
        } while (trail != new_pos);
      }
    } else {
      this->m_robot = new_pos;
    }
    ++this->m_instruction_index;
    return true;
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
        } else if (std::find(grid.m_walls.cbegin(), grid.m_walls.cend(), pos) !=
                   grid.m_walls.cend()) {
          os << '#';
        } else if (std::find(grid.m_boxes.cbegin(), grid.m_boxes.cend(), pos) !=
                   grid.m_boxes.cend()) {
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

auto part_one(Grid grid) {
  do {
  } while (grid.step());
  return grid.score();
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_15_mock2.txt"};
  std::filesystem::path input_path{"../../data/2024/input_15.txt"};
  utils::Reader reader(input_path);
  Grid grid(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(grid))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
