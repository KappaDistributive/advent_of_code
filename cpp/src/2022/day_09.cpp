#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

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

template <size_t d> struct Grid {
  std::array<Point, d> rope;
  std::vector<Point> tail_trace;

  Grid() {
    this->rope.fill(Point{{0, 0}});
    this->tail_trace.push_back(Point{{0, 0}});
  }

  Point step(const Point &leader, const Point &follower) {
    Point diff = leader - follower;
    Point step{{0, 0}};
    if (diff[0] == 0 || diff[1] == 0) {
      if (diff[0] > 1) {
        step[0] = 1;
      } else if (diff[0] < -1) {
        step[0] = -1;
      }
      if (diff[1] > 1) {
        step[1] = 1;
      } else if (diff[1] < -1) {
        step[1] = -1;
      }
    } else {
      step[0] = std::min(std::abs(diff[0]), 1);
      if (diff[0] < 0) {
        step[0] *= -1;
      }
      step[1] = std::min(std::abs(diff[1]), 1);
      if (diff[1] < 0) {
        step[1] *= -1;
      }
      if (leader == follower + step) {
        step = Point{{0, 0}};
      }
    }
    return step;
  }

  void move(Direction direction) {
    Point first_step{{0, 0}};
    switch (direction) {
    case Direction::Up:
      first_step = Point{{0, -1}};
      break;
    case Direction::Right:
      first_step = Point{{1, 0}};
      break;
    case Direction::Down:
      first_step = Point{{0, 1}};
      break;
    case Direction::Left:
      first_step = Point{{-1, 0}};
      break;
    }
    this->rope[0] += first_step;
    for (size_t index{1}; index < d; ++index) {
      auto local_step = this->step(this->rope[index - 1], this->rope[index]);
      // std::cout
      //   << "Index: " << index
      //   << " Local Step: " << local_step
      //   << " Direction: " << direction
      //   << " Leader: " <<  rope[index-1]
      //   << " Follower: "  << rope[index]
      //   << " Diff: " << rope[index-1] - rope[index];
      this->rope[index] += local_step;

      // std::cout
      //   << " New Follower: " << rope[index]
      //   << " Manhatten Distance: " <<
      //   rope[index-1].manhatten_distance(rope[index]) << std::endl;
      assert(rope[index - 1].manhatten_distance(rope[index]) <= 2);
    }

    this->tail_trace.push_back(this->rope.back());
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
    for (const auto &pos : grid.rope) {
      upper_left[0] = std::min(upper_left[0], pos[0]);
      upper_left[1] = std::min(upper_left[1], pos[1]);
      lower_right[0] = std::max(lower_right[0], pos[0]);
      lower_right[1] = std::max(lower_right[1], pos[1]);
    }

    Point pos{{0, 0}};
    char mark = '.';
    for (int y{upper_left[1]}; y <= lower_right[1]; ++y) {
      for (int x{upper_left[0]}; x <= lower_right[0]; ++x) {
        mark = '.';
        pos = Point{{x, y}};
        if (pos == grid.rope[0]) {
          mark = 'H';
        } else if (std::find(grid.rope.begin(), grid.rope.end(), pos) !=
                   grid.rope.end()) {
          mark = '0' + static_cast<char>(
                           (std::find(grid.rope.begin(), grid.rope.end(), pos) -
                            grid.rope.begin()) %
                           10);
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

template <size_t d> auto run(const std::vector<std::string> &input) {
  Grid<d> grid;
  std::cout << grid << '\n' << std::endl;
  for (const auto &line : input) {
    Move move(line);
    // std::cout << "### " << move << std::endl;
    for (size_t step{0}; step < move.distance; ++step) {
      grid.move(move.direction);
      // std::cout << grid << '\n' << std::endl;
    }
  }

  std::cout << grid << std::endl;
  std::set<Point> trace;
  for (const auto &pos : grid.tail_trace) {
    trace.insert(pos);
  }
  return trace.size();
}

auto part_one(const std::vector<std::string> &input) { return run<2>(input); }

auto part_two(const std::vector<std::string> &input) { return run<10>(input); }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_09_mock2.txt"};
  std::filesystem::path input_path{"../../data/2022/input_09.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
