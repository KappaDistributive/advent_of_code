#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

enum class Direction { Up, Right, Down, Left };

static const std::array<Direction, 4> ALL_DIRECTIONS{
    {Direction::Up, Direction::Right, Direction::Down, Direction::Left}};

char decode(Direction direction) {
  switch (direction) {
  case Direction::Up:
    return '^';
    break;
  case Direction::Right:
    return '>';
    break;
  case Direction::Down:
    return 'v';
    break;
  case Direction::Left:
    return '<';
    break;
  }
}

Direction direction(char symbol) {
  switch (symbol) {
  case '^':
    return Direction::Up;
  case '>':
    return Direction::Right;
  case 'v':
    return Direction::Down;
  case '<':
    return Direction::Left;
  default:
    break;
  }
  throw std::runtime_error("");
}

Point decode_step(Direction direction) {
  Point step{{0, 0}};
  switch (direction) {
  case Direction::Up:
    step = Point{{0, -1}};
    break;
  case Direction::Right:
    step = Point{{1, 0}};
    break;
  case Direction::Down:
    step = Point{{0, 1}};
    break;
  case Direction::Left:
    step = Point{{-1, 0}};
    break;
  }
  return step;
}

struct Basin {
  std::vector<char> map;
  size_t width, height;
  Point start_position;
  std::vector<std::pair<Direction, Point>> blizzards;

  Basin(const std::vector<std::string> &input) {
    this->height = input.size();
    this->width = input[0].size();
    Point current_pos{{0, 0}};
    for (const auto &line : input) {
      current_pos[0] = 0;
      for (const auto &symbol : line) {
        if (current_pos[1] == 0 && symbol == '.') {
          this->start_position = current_pos;
        }
        if (symbol == '.' || symbol == '#') {
          this->map.push_back(symbol);
        } else {
          this->blizzards.push_back({direction(symbol), current_pos});
          this->map.push_back('.');
        }
        ++current_pos[0];
      }
      ++current_pos[1];
    }
  }

  void move_blizzards() {
    for (size_t index{0}; index < this->blizzards.size(); ++index) {
      auto step = decode_step(std::get<0>(this->blizzards[index]));
      std::get<1>(this->blizzards[index]) += step;
      if (std::get<1>(this->blizzards[index])[0] == 0) {
        std::get<1>(this->blizzards[index])[0] =
            static_cast<int>(this->width) - 2;
      } else if (std::get<1>(this->blizzards[index])[0] ==
                 static_cast<int>(this->width) - 1) {
        std::get<1>(this->blizzards[index])[0] = 1;
      }
      if (std::get<1>(this->blizzards[index])[1] == 0) {
        std::get<1>(this->blizzards[index])[1] =
            static_cast<int>(this->height) - 2;
      } else if (std::get<1>(this->blizzards[index])[1] ==
                 static_cast<int>(this->height) - 1) {
        std::get<1>(this->blizzards[index])[1] = 1;
      }
    }
  }

  bool can_move(const Point &position,
                std::optional<Direction> direction) const {

    Point target = position;
    if (direction.has_value()) {
      target += decode_step(direction.value());
    }
    std::vector<Point> blizzard_positions;
    for (const auto &blizzard : blizzards) {
      blizzard_positions.push_back(std::get<1>(blizzard));
    }
    return this->map[target[1] * this->width + target[0]] == '.' &&
           std::find(blizzard_positions.cbegin(), blizzard_positions.cend(),
                     target) == blizzard_positions.cend();
  }

  friend std::ostream &operator<<(std::ostream &os, const Basin &basin) {
    Point pos{{0, 0}};
    for (size_t y{0}; y < basin.height; ++y) {
      pos[1] = static_cast<int>(y);
      for (size_t x{0}; x < basin.width; ++x) {
        pos[0] = static_cast<int>(x);
        std::vector<std::pair<Direction, Point>> blizzards;
        for (auto blizzard : basin.blizzards) {
          if (std::get<1>(blizzard) == pos) {
            blizzards.push_back(blizzard);
          }
        }
        if (blizzards.size() == 0) {
          os << basin.map[y * basin.width + x];
        } else if (blizzards.size() == 1) {
          os << decode(std::get<0>(blizzards[0]));
        } else {
          if (blizzards.size() < 10) {
            os << blizzards.size();
          } else {
            os << '*';
          }
        }
      }
      os << '\n';
    }

    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Basin basin(input);
  std::set<Point> positions{{basin.start_position}};
  size_t result{0};
  while (true) {
    std::set<Point> new_positions;
    basin.move_blizzards();
    for (auto pos : positions) {
      if (pos[1] == static_cast<int>(basin.height) - 1) {
        return result;
      }
      if (basin.can_move(pos, std::nullopt)) {
        new_positions.insert(pos);
      }
      for (auto direction : ALL_DIRECTIONS) {
        if (basin.can_move(pos, direction)) {
          new_positions.insert(pos + decode_step(direction));
        }
      }
    }
    ++result;
    positions = new_positions;
  }
  return size_t{0};
}

auto part_two(const std::vector<std::string> &input) {
  Basin basin(input);
  std::set<std::pair<Point, int>> positions{{{basin.start_position, 0}}};
  size_t result{0};
  while (true) {
    std::set<std::pair<Point, int>> new_positions;
    basin.move_blizzards();
    for (auto [pos, phase] : positions) {
      auto new_phase = phase;
      if (phase == 0 && pos[1] == static_cast<int>(basin.height) - 1) {
        new_phase = 1;
      } else if (phase == 1 && pos[1] == 0) {
        new_phase = 2;
      } else if (phase == 2 && pos[1] == static_cast<int>(basin.height) - 1) {
        return result;
      }
      if (basin.can_move(pos, std::nullopt)) {
        new_positions.insert({pos, new_phase});
      }
      for (auto direction : ALL_DIRECTIONS) {
        if (basin.can_move(pos, direction)) {
          new_positions.insert({pos + decode_step(direction), new_phase});
        }
      }
    }
    ++result;
    positions = new_positions;
  }
  return size_t{0};
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_24_mock2.txt"};
  std::filesystem::path input_path{"../../data/2022/input_24.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
