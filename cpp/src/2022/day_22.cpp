#include "../utils/input.hpp"

enum class Direction { Up, Right, Down, Left };

static const std::array<Direction, 4> ALL_DIRECTIONS{
    {Direction::Up, Direction::Right, Direction::Down, Direction::Left}};

static const std::map<std::pair<Direction, char>, Direction> ROTATION{
    {{Direction::Up, 'R'}, Direction::Right},
    {{Direction::Up, 'L'}, Direction::Left},
    {{Direction::Right, 'R'}, Direction::Down},
    {{Direction::Right, 'L'}, Direction::Up},
    {{Direction::Down, 'R'}, Direction::Left},
    {{Direction::Down, 'L'}, Direction::Right},
    {{Direction::Left, 'R'}, Direction::Up},
    {{Direction::Left, 'L'}, Direction::Down},
};

struct Jungle {
  std::array<int, 300 * 300> map; // 0 -> void, 1 -> path, 2-> force field
  std::vector<std::variant<int, char>> instructions;
  std::vector<std::tuple<size_t, size_t, Direction>> path;
  size_t max_x, max_y;

  Jungle(const std::vector<std::string> &input) : max_x(0), max_y(0) {
    this->map.fill(0);
    for (size_t y{0}; y < 300; ++y) {
      if (input[y].size() == 0) {
        break;
      }
      for (size_t x{0}; x < 300; ++x) {
        if (y >= input.size() || x >= input[y].size()) {
          this->map[y * 300 + x] = 0;
        } else if (input[y][x] == '.') {
          this->map[y * 300 + x] = 1;
        } else if (input[y][x] == '#') {
          this->map[y * 300 + x] = 2;
        }
      }
    }
    for (size_t y{0}; y < 300; ++y) {
      for (size_t x{0}; x < 300; ++x) {
        if (this->map[y * 300 + x] == 1 && this->path.size() == 0) {
          this->path.push_back(std::make_tuple(x, y, Direction::Right));
        }
        if (this->map[y * 300 + x] != 0) {
          this->max_x = std::max(this->max_x, x);
          this->max_y = std::max(this->max_y, y);
        }
      }
    }
    int buffer{0};
    for (auto c : input.back()) {
      if ('0' <= c && c <= '9') {
        buffer *= 10;
        buffer += static_cast<int>(c - '0');
      } else {
        if (buffer != 0) {
          this->instructions.push_back(buffer);
          buffer = 0;
        }
        this->instructions.push_back(c);
      }
    }
    if (buffer != 0) {
      this->instructions.push_back(buffer);
    }
  }

  std::tuple<size_t, size_t, Direction> next() const {
    auto [x, y, direction] = this->path.back();
    if (direction == Direction::Up) {
      auto potential_y = y == 0 ? 299 : y - 1;
      while (this->map[potential_y * 300 + x] == 0) {
        potential_y = potential_y == 0 ? 299 : potential_y - 1;
      }
      if (this->map[potential_y * 300 + x] == 1) {
        y = potential_y;
      }
    } else if (direction == Direction::Right) {
      auto potential_x = (x + 1) % 300;
      while (this->map[y * 300 + potential_x] == 0) {
        potential_x = (potential_x + 1) % 300;
      }
      if (this->map[y * 300 + potential_x] == 1) {
        x = potential_x;
      }
    } else if (direction == Direction::Down) {
      auto potential_y = (y + 1) % 300;
      while (this->map[potential_y * 300 + x] == 0) {
        potential_y = (potential_y + 1) % 300;
      }
      if (this->map[potential_y * 300 + x] == 1) {
        y = potential_y;
      }
    } else if (direction == Direction::Left) {
      auto potential_x = x == 0 ? 299 : x - 1;
      while (this->map[y * 300 + potential_x] == 0) {
        potential_x = potential_x == 0 ? 299 : potential_x - 1;
      }
      if (this->map[y * 300 + potential_x] == 1) {
        x = potential_x;
      }
    }

    return {x, y, direction};
  }

  void step(std::variant<int, char> move) {
    if (std::holds_alternative<char>(move)) {
      auto [x, y, direction] = this->path.back();
      this->path.push_back(std::make_tuple(
          x, y, ROTATION.at({direction, std::get<char>(move)})));
    } else {
      auto distance = std::get<int>(move);
      while (distance-- > 0) {
        auto next_step = this->next();
        if (next_step != this->path.back()) {
          this->path.push_back(next_step);
        }
      }
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Jungle &jungle) {
    for (size_t y{0}; y < jungle.max_y; ++y) {
      for (size_t x{0}; x < jungle.max_x; ++x) {
        auto it{jungle.path.crend()};
        for (auto direction : ALL_DIRECTIONS) {
          it =
              std::min(it, std::find(jungle.path.crbegin(), jungle.path.crend(),
                                     std::make_tuple(x, y, direction)));
        }
        if (it != jungle.path.crend()) {
          if (it == jungle.path.crbegin()) {
            os << "\033[31m";
          }
          auto direction = std::get<2>(*it);
          switch (direction) {
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
          if (it == jungle.path.crbegin()) {
            os << "\033[39m";
          }

          continue;
        }
        switch (jungle.map[y * 300 + x]) {
        case 1:
          os << '.';
          break;
        case 2:
          os << '#';
          break;
        default:
          os << ' ';
          break;
        }
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Jungle jungle(input);
  // std::cout << jungle << std::endl;
  for (auto i : jungle.instructions) {
    jungle.step(i);
    // if (std::holds_alternative<char>(i)) {
    //   jungle.step(i);
    //   std::cout << std::get<char>(i) << std::endl;
    //   std::cout << jungle << std::endl;
    // } else {
    //   std::cout << std::get<int>(i) << std::endl;
    //   for (int sub_step{0}; sub_step < std::get<int>(i); ++sub_step) {
    //     jungle.step(1);
    //     std::cout << jungle << std::endl;
    //   }
    // }
  }
  auto [x, y, direction] = jungle.path.back();
  size_t result{0};
  result += 1000 * (y + 1);
  result += 4 * (x + 1);
  switch (direction) {
  case Direction::Up:
    result += 3;
    break;
  case Direction::Right:
    result += 0;
    break;
  case Direction::Down:
    result += 1;
    break;
  case Direction::Left:
    result += 2;
    break;
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_22_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_22.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
