#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

struct Rope {
  Point head{{0, 0}};
  Point tail{{0, 0}};
};

enum class Direction { Up, Right, Down, Left };

Direction parse(char direction) {
  switch (direction) {
  case 'U':
    return Direction::Up;
    break;
  case 'R':
    return Direction::Right;
    break;
  case 'D':
    return Direction::Down;
    break;
  case 'L':
    return Direction::Left;
    break;
  default:
    throw std::runtime_error(fmt::format("Illegal direction: {}\n", direction));
  }
}

std::ostream &operator<<(std::ostream &os, Direction direction) {
  switch (direction) {
  case Direction::Up:
    os << 'U';
    break;
  case Direction::Right:
    os << 'R';
    break;
  case Direction::Down:
    os << 'D';
    break;
  case Direction::Left:
    os << 'L';
    break;
  }
  return os;
}

struct Move {
  Direction direction;
  size_t distance;

  Move(const std::string &input) {
    auto splits = utils::split_string(input, ' ');
    assert(splits.size() == 2);
    assert(splits[0].size() == 1);
    this->direction = parse(splits[0][0]);
    this->distance = std::stoull(splits[1]);
  }

  friend std::ostream &operator<<(std::ostream &os, const Move &move) {
    os << move.direction << ' ' << move.distance;
    return os;
  }
};

struct Grid {
  Rope rope;
  std::vector<Point> head_trace{Point{{0, 0}}};
  std::vector<Point> tail_trace{Point{{0, 0}}};
  size_t x_stretch{0};
  size_t y_stretch{0};

  void move(Direction direction) {
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
    this->rope.head += step;

    Point diff = this->rope.head - this->rope.tail;
    step = Point{{0, 0}};
    if (diff == Point{{0, 2}}) {
      step = Point{{0, 1}};
    } else if (diff == Point{{-1, 2}}) {
      step = Point{{-1, 1}};
    } else if (diff == Point{{-2, 1}}) {
      step = Point{{-1, 1}};
    } else if (diff == Point{{-2, 0}}) {
      step = Point{{-1, 0}};
    } else if (diff == Point{{-2, -1}}) {
      step = Point{{-1, -1}};
    } else if (diff == Point{{-1, -2}}) {
      step = Point{{-1, -1}};
    } else if (diff == Point{{0, -2}}) {
      step = Point{{0, -1}};
    } else if (diff == Point{{1, -2}}) {
      step = Point{{1, -1}};
    } else if (diff == Point{{2, -1}}) {
      step = Point{{1, -1}};
    } else if (diff == Point{{2, 0}}) {
      step = Point{{1, 0}};
    } else if (diff == Point{{2, 1}}) {
      step = Point{{1, 1}};
    } else if (diff == Point{{1, 2}}) {
      step = Point{{1, 1}};
    }
    rope.tail += step;

    this->head_trace.push_back(rope.head);
    this->tail_trace.push_back(rope.tail);
  }

  friend std::ostream &operator<<(std::ostream &os, const Grid &grid) {
    Point upper_left{{0, 0}};
    Point lower_right{{0, 0}};

    for (const auto &pos : grid.tail_trace) {
      upper_left[0] = std::min(upper_left[0], pos[0]);
      upper_left[1] = std::min(upper_left[1], pos[1]);
      lower_right[0] = std::max(lower_right[0], pos[0]);
      lower_right[1] = std::max(lower_right[1], pos[1]);
    }
    for (const auto &pos : grid.head_trace) {
      upper_left[0] = std::min(upper_left[0], pos[0]);
      upper_left[1] = std::min(upper_left[1], pos[1]);
      lower_right[0] = std::max(lower_right[0], pos[0]);
      lower_right[1] = std::max(lower_right[1], pos[1]);
    }

    Point pos{{0, 0}};
    char mark = '.';
    for (int y{std::min(upper_left[1], -static_cast<int>(grid.y_stretch))};
         y <= std::max(lower_right[1], static_cast<int>(grid.y_stretch)); ++y) {
      for (int x{std::min(upper_left[0], -static_cast<int>(grid.x_stretch))};
           x <= std::max(lower_right[0], static_cast<int>(grid.x_stretch));
           ++x) {
        mark = '.';
        pos = Point{{x, y}};
        if (pos == grid.rope.head) {
          mark = 'H';
        } else if (pos == grid.rope.tail) {
          mark = 'T';
        } else if (pos == Point{{0, 0}}) {
          mark = 's';
        } else if (std::find(grid.tail_trace.cbegin(), grid.tail_trace.cend(),
                             pos) != grid.tail_trace.cend()) {
          mark = '#';
        }
        os << mark;
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Grid grid;
  std::cout << grid << '\n' << std::endl;
  for (const auto &line : input) {
    Move move(line);
    for (size_t step{0}; step < move.distance; ++step) {
      grid.move(move.direction);
    }
    // std::cout << "### " << move << std::endl;
    // std::cout << grid << '\n' << std::endl;
  }

  std::cout << grid << std::endl;
  std::set<Point> trace;
  for (const auto &pos : grid.tail_trace) {
    trace.insert(pos);
  }
  return trace.size();
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_09_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_09.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
