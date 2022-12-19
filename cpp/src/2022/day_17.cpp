#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

struct Rock {
  Point lower_left;
  std::array<bool, 4 * 4> shape;

  Rock(int id, Point lower_left) : lower_left(lower_left) {
    if (id == 0) {
      // clang-format off
      this->shape = {{
          0, 0, 0, 0,
          0, 0, 0, 0,
          0, 0, 0, 0,
          1, 1, 1, 1,
      }};
      // clang-format on

    } else if (id == 1) {
      // clang-format off
      this->shape = {{
          0, 0, 0, 0,
          0, 1, 0, 0,
          1, 1, 1, 0,
          0, 1, 0, 0,
      }};
      // clang-format on
    } else if (id == 2) {
      // clang-format off
      this->shape = {{
          0, 0, 0, 0,
          0, 0, 1, 0,
          0, 0, 1, 0,
          1, 1, 1, 0,
      }};
      // clang-format on
    } else if (id == 3) {
      // clang-format off
      this->shape = {{
          1, 0, 0, 0,
          1, 0, 0, 0,
          1, 0, 0, 0,
          1, 0, 0, 0,
      }};
      // clang-format on
    } else if (id == 4) {
      // clang-format off
      this->shape = {{
          0, 0, 0, 0,
          0, 0, 0, 0,
          1, 1, 0, 0,
          1, 1, 0, 0,
      }};
      // clang-format on
    }
  }

  std::set<Point> units() const {
    std::set<Point> result;
    for (int y{0}; y < 4; ++y) {
      for (int x{0}; x < 4; ++x) {
        if (this->shape[(3 - y) * 4 + x]) {
          result.insert(this->lower_left + Point{{x, y}});
        }
      }
    }
    return result;
  }
};

struct Chamber {
  std::vector<Rock> settled_rocks;
  std::set<Point> occupied;
  Rock current_rock;
  const std::string movements;
  int max_y;
  size_t tick;

  explicit Chamber(std::string movements)
      : current_rock(Rock(0, Point{{2, 3}})), movements(movements), max_y(0),
        tick(0) {}

  bool is_valid(const Rock &rock) const {
    for (const auto &unit : rock.units()) {
      if (unit[1] < 0 || unit[0] < 0 || unit[0] > 6 ||
          this->occupied.count(unit) > 0) {
        return false;
      }
    }
    return true;
  }

  bool step() {
    bool new_rock{false};
    if (this->tick % 2 == 0) {
      // jet of gas pushes rock
      Point push{{1, 0}};
      if (this->movements[(this->tick / 2) % this->movements.size()] == '<') {
        push[0] = -1;
      }
      this->current_rock.lower_left += push;
      if (!this->is_valid(this->current_rock)) {
        this->current_rock.lower_left -= push;
      }
    } else {
      // rock falls 1 unit
      Point push{{0, -1}};
      this->current_rock.lower_left += push;
      if (!this->is_valid(this->current_rock)) {
        this->current_rock.lower_left -= push;
        this->settled_rocks.push_back(this->current_rock);
        for (const auto &unit : this->current_rock.units()) {
          this->occupied.insert(unit);
          this->max_y = std::max(this->max_y, unit[1]);
        }
        this->current_rock =
            Rock(this->settled_rocks.size() % 5, Point{{2, this->max_y + 4}});
        new_rock = true;
      }
    }
    ++this->tick;
    return new_rock;
  }

  std::string str() const {
    std::string result{};
    Point pos{{0, 0}};
    for (int y{this->max_y}; y >= -1; --y) {
      pos[1] = y;
      for (int x{-1}; x < 8; ++x) {
        pos[0] = x;
        if (x < 0 || x > 6) {
          result.push_back(y == -1 ? '+' : '|');
        } else if (y == -1) {
          result.push_back('-');
        } else if (this->current_rock.units().count(pos) > 0) {
          result.push_back('@');
        } else {
          result.push_back(this->occupied.count(Point{{x, y}}) > 0 ? '#' : '.');
        }
      }
      result.push_back('\n');
    }
    return result;
  }
};

auto part_one(const std::string &input) {
  Chamber chamber{input};
  // std::cout << chamber.str() << std::endl;
  while (chamber.settled_rocks.size() < 2022) {
    chamber.step();
  }
  int result{0};
  for (const auto &rock : chamber.settled_rocks) {
    for (const auto &unit : rock.units()) {
      result = std::max(result, unit[1]);
    }
  }
  // std::cout << chamber.str() << std::endl;
  return result + 1;
}

auto part_two(const std::string &input) {
  Chamber chamber{input};
  std::vector<int> height_gains{};
  int max_y{0};
  while (chamber.settled_rocks.size() < 10000) {
    if (chamber.step()) {
      height_gains.push_back(chamber.max_y - max_y);
      max_y = chamber.max_y;
    }
  }
  const size_t total_length{1000000000000};

  // find cycle
  size_t best_offset{0};
  size_t best_period{std::numeric_limits<size_t>::max()};
  for (size_t offset{0}; offset < 1000; ++offset) {
    for (size_t period{1}; period < height_gains.size() / 4; ++period) {
      bool match{true};
      for (size_t t{1};
           match && offset + t * period < height_gains.size() - period; ++t) {
        for (size_t i{0}; i < period; ++i) {
          if (height_gains[offset + i] !=
              height_gains[offset + t * period + i]) {
            match = false;
            break;
          }
        }
      }
      if (match && period < best_period) {
        best_offset = offset;
        best_period = period;
      }
    }
  }

  size_t result{0};
  for (size_t index{0}; index < best_offset; ++index) {
    result += height_gains[index];
  }

  size_t period_sum{0};
  for (size_t index{0}; index < best_period; ++index) {
    period_sum += height_gains[best_offset + index];
  }

  result += ((total_length - best_offset) / best_period) * period_sum;
  for (size_t index{0}; index < (total_length - best_offset) % best_period;
       ++index) {
    result += height_gains[best_offset + index];
  }

  return result + 1;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_17_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_17.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines()[0];

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
