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
  std::vector<std::tuple<int, int, Direction>> path;
  int max_x, max_y;
  bool part_two{false};

  Jungle(const std::vector<std::string> &input) : max_x(0), max_y(0) {
    this->map.fill(0);
    for (int y{0}; y < 300; ++y) {
      if (input[y].size() == 0) {
        break;
      }
      for (int x{0}; x < 300; ++x) {
        if (y >= static_cast<int>(input.size()) ||
            x >= static_cast<int>(input[y].size())) {
          this->map[y * 300 + x] = 0;
        } else if (input[y][x] == '.') {
          this->map[y * 300 + x] = 1;
        } else if (input[y][x] == '#') {
          this->map[y * 300 + x] = 2;
        }
      }
    }
    for (int y{0}; y < 300; ++y) {
      for (int x{0}; x < 300; ++x) {
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

  size_t border(size_t x, size_t y) const {
    size_t result{0};
    if (((x == 50 || x == 99) && y >= 50 && y <= 99) ||
        ((y == 50 || y == 99) && x >= 50 && x <= 99)) {
      result = 1;
    } else if (((x == 50 || x == 99) && y >= 0 && y <= 49) ||
               ((y == 0 || y == 49) && x >= 50 && x <= 99)) {
      result = 2;
    } else if (((x == 0 || x == 49) && y >= 100 && y <= 149) ||
               ((y == 100 || y == 149) && x >= 0 && x <= 49)) {
      result = 3;
    } else if (((x == 100 || x == 149) && y >= 0 && y <= 49) ||
               ((y == 0 || y == 49) && x >= 100 && x <= 149)) {
      result = 4;
    } else if (((x == 50 || x == 99) && y >= 100 && y <= 149) ||
               ((y == 100 || y == 149) && x >= 50 && x <= 99)) {
      result = 5;
    } else if (((x == 0 || x == 49) && y >= 150 && y <= 199) ||
               ((y == 150 || y == 199) && x >= 0 && x <= 49)) {
      result = 6;
    }

    return result;
  }

  std::tuple<int, int, Direction> next() const {
    if (!this->part_two) {
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
    } else {
      auto [x, y, direction] = this->path.back();
      /*       +----+----+
       *       | 2  | 4  |
       *       |    |    |
       *       +----+----+
       *       | 1  |
       *       |    |
       *  +----+----+
       *  | 3  | 5  |
       *  |    |    |
       *  +----+----+
       *  | 6  |
       *  |    |
       *  +----+
       */
      int potential_x{x}, potential_y{y};
      Direction potential_direction{direction};
      auto num_border = this->border(x, y);
      bool did_move{false};
      if (num_border == 1) {
        if (direction == Direction::Right && x == 99) {
          // 1 [>] 4
          did_move = true;
          potential_direction = Direction::Up;
          potential_x = 100 + y - 50;
          potential_y = 49;
        } else if (direction == Direction::Left && x == 50) {
          // 1 [<] 3
          did_move = true;
          potential_direction = Direction::Down;
          potential_x = y - 50;
          potential_y = 100;
        }
      } else if (num_border == 2) {
        if (direction == Direction::Left && x == 50) {
          // 2 [<] 3
          did_move = true;
          potential_direction = Direction::Right;
          potential_x = 0;
          potential_y = 149 - y;
        } else if (direction == Direction::Up && y == 0) {
          // 2 [^] 6
          did_move = true;
          potential_direction = Direction::Right;
          potential_x = 0;
          potential_y = 150 + (x - 50);
        }
      } else if (num_border == 3) {
        if (direction == Direction::Up && y == 100) {
          // 3 [^] 1
          did_move = true;
          potential_direction = Direction::Right;
          potential_x = 50;
          potential_y = 50 + x;
        } else if (direction == Direction::Left && x == 0) {
          // 3 [<] 2
          did_move = true;
          potential_direction = Direction::Right;
          potential_x = 50;
          potential_y = 49 - (y - 100);
        }
      } else if (num_border == 4) {
        if (direction == Direction::Up && y == 0) {
          // 4 [^] 6
          did_move = true;
          potential_direction = Direction::Up;
          potential_x = x - 100;
          potential_y = 199;
        } else if (direction == Direction::Right && x == 149) {
          // 4 [>] 5
          did_move = true;
          potential_direction = Direction::Left;
          potential_x = 99;
          potential_y = 149 - y;
        } else if (direction == Direction::Down && y == 49) {
          // 4 [v] 1
          did_move = true;
          potential_direction = Direction::Left;
          potential_x = 99;
          potential_y = 50 + x - 100;
        }
      } else if (num_border == 5) {
        if (direction == Direction::Right && x == 99) {
          // 5 [>] 4
          did_move = true;
          potential_direction = Direction::Left;
          potential_x = 149;
          potential_y = 49 - (y - 100);
        } else if (direction == Direction::Down && y == 149) {
          // 5 [v] 6
          did_move = true;
          potential_direction = Direction::Left;
          potential_x = 49;
          potential_y = 150 + x - 50;
        }
      } else if (num_border == 6) {
        if (direction == Direction::Right && x == 49) {
          // 6 [>] 5
          did_move = true;
          potential_direction = Direction::Up;
          potential_x = 50 + y - 150;
          potential_y = 149;
        } else if (direction == Direction::Down && y == 199) {
          // 6 [v] 4
          did_move = true;
          potential_direction = Direction::Down;
          potential_x = 100 + x;
          potential_y = 0;
        } else if (direction == Direction::Left && x == 0) {
          // 6 [<] 2
          did_move = true;
          potential_direction = Direction::Down;
          potential_x = 50 + y - 150;
          potential_y = 0;
        }
      }
      if (!did_move) {
        assert(num_border == 0);
        switch (direction) {
        case Direction::Up:
          --potential_y;
          break;
        case Direction::Right:
          ++potential_x;
          break;
        case Direction::Down:
          ++potential_y;
          break;
        case Direction::Left:
          --potential_x;
          break;
        }
      }
      assert(this->map[potential_y * 300 + potential_x] == 1 ||
             this->map[potential_y * 300 + potential_x] == 2);
      if (this->map[potential_y * 300 + potential_x] == 1) {
        x = potential_x;
        y = potential_y;
        direction = potential_direction;
      }
      return {x, y, direction};
    }
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
    for (int y{0}; y <= jungle.max_y; ++y) {
      for (int x{0}; x <= jungle.max_x; ++x) {
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

size_t get_result(size_t x, size_t y, Direction direction) {
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

auto part_one(const std::vector<std::string> &input) {
  Jungle jungle(input);
  for (auto i : jungle.instructions) {
    jungle.step(i);
  }
  auto [x, y, direction] = jungle.path.back();
  return get_result(x, y, direction);
}

auto part_two(const std::vector<std::string> &input) {
  Jungle jungle(input);
  jungle.part_two = true;
  for (auto i : jungle.instructions) {
    jungle.step(i);
    // std::cout << jungle << std::endl;
  }

  auto [x, y, direction] = jungle.path.back();

  return get_result(x, y, direction);
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_22_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_22.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
