#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

enum class Direction {
  North,
  NorthEast,
  East,
  SouthEast,
  South,
  SouthWest,
  West,
  NorthWest
};

static const std::vector<Direction> ALL_DIRECTIONS{
    {Direction::North, Direction::NorthEast, Direction::East,
     Direction::SouthEast, Direction::South, Direction::SouthWest,
     Direction::West, Direction::NorthWest}};

Point add(const Point &point, const Direction &direction) {
  switch (direction) {
  case Direction::North:
    return Point{{point[0], point[1] - 1}};
    break;
  case Direction::NorthEast:
    return Point{{point[0] + 1, point[1] - 1}};
    break;
  case Direction::East:
    return Point{{point[0] + 1, point[1]}};
    break;
  case Direction::SouthEast:
    return Point{{point[0] + 1, point[1] + 1}};
    break;
  case Direction::South:
    return Point{{point[0], point[1] + 1}};
    break;
  case Direction::SouthWest:
    return Point{{point[0] - 1, point[1] + 1}};
    break;
  case Direction::West:
    return Point{{point[0] - 1, point[1]}};
    break;
  case Direction::NorthWest:
    return Point{{point[0] - 1, point[1] - 1}};
    break;
  }
}

struct Grove {
  std::set<Point> elves;
  size_t round;

  Grove(const std::vector<std::string> &input) : round(0) {
    Point pos{{0, 0}};
    for (const auto &line : input) {
      pos[0] = 0;
      for (const auto &symbol : line) {
        if (symbol == '#') {
          this->elves.insert(pos);
        }
        ++pos[0];
      }
      ++pos[1];
    }
  }

  std::optional<Direction> first_valid_direction(const Point &elve,
                                                 size_t rule) const {
    if (rule == 0) {
      if (!this->elves.count(add(elve, Direction::North)) &&
          !this->elves.count(add(elve, Direction::NorthEast)) &&
          !this->elves.count(add(elve, Direction::NorthWest))) {
        return Direction::North;
      }
    } else if (rule == 1) {
      if (!this->elves.count(add(elve, Direction::South)) &&
          !this->elves.count(add(elve, Direction::SouthEast)) &&
          !this->elves.count(add(elve, Direction::SouthWest))) {
        return Direction::South;
      }
    } else if (rule == 2) {
      if (!this->elves.count(add(elve, Direction::West)) &&
          !this->elves.count(add(elve, Direction::NorthWest)) &&
          !this->elves.count(add(elve, Direction::SouthWest))) {
        return Direction::West;
      }
    } else if (rule == 3) {
      if (!this->elves.count(add(elve, Direction::East)) &&
          !this->elves.count(add(elve, Direction::NorthEast)) &&
          !this->elves.count(add(elve, Direction::SouthEast))) {
        return Direction::East;
      }
    }
    return std::nullopt;
  }

  std::optional<Direction> proposed_move(const Point &elve) const {
    // do nothing?
    bool do_nothing{true};
    for (const auto &direction : ALL_DIRECTIONS) {
      if (this->elves.count(add(elve, direction))) {
        do_nothing = false;
        break;
      }
    }
    if (do_nothing) {
      return std::nullopt;
    }

    for (size_t index{0}; index < 4; ++index) {
      auto result = this->first_valid_direction(elve, (round + index) % 4);
      if (result.has_value()) {
        return result.value();
      }
    }
    return std::nullopt;
  }

  bool step() {
    bool did_move{false};
    std::map<Point, Direction> proposed_moves;
    std::map<Point, size_t> visit_counts;
    for (auto &elve : this->elves) {
      auto move = this->proposed_move(elve);
      if (move.has_value()) {
        proposed_moves.insert({elve, move.value()});
      }
      auto point = move.has_value() ? add(elve, move.value()) : elve;
      if (visit_counts.count(point) == 0) {
        visit_counts.insert({point, 1});
      } else {
        ++visit_counts.at(point);
      }
    }

    std::set<Point> new_elves;
    for (auto elve : this->elves) {
      if (proposed_moves.count(elve)) {
        auto move = proposed_moves.at(elve);
        auto target = add(elve, move);
        if (!visit_counts.count(target) || visit_counts.at(target) == 1) {
          new_elves.insert(target);
          did_move = true;
        } else {
          new_elves.insert(elve);
        }
      } else {
        new_elves.insert(elve);
      }
    }
    this->elves = new_elves;
    ++this->round;

    return did_move;
  }

  std::pair<Point, Point> border() const {
    Point upper_left{{0, 0}}, lower_right{{0, 0}};
    if (this->elves.size() > 0) {
      upper_left = *this->elves.begin();
      lower_right = *this->elves.begin();
    }
    for (const auto &elve : this->elves) {
      upper_left[0] = std::min(upper_left[0], elve[0]);
      upper_left[1] = std::min(upper_left[1], elve[1]);
      lower_right[0] = std::max(lower_right[0], elve[0]);
      lower_right[1] = std::max(lower_right[1], elve[1]);
    }

    return {upper_left, lower_right};
  }

  size_t score() const {
    auto [upper_left, lower_right] = this->border();
    size_t result{0};
    for (int y{upper_left[1]}; y <= lower_right[1]; ++y) {
      for (int x{upper_left[0]}; x <= lower_right[0]; ++x) {
        if (!this->elves.count(Point{{x, y}})) {
          ++result;
        }
      }
    }

    return result;
  }

  friend std::ostream &operator<<(std::ostream &os, const Grove &grove) {
    auto [upper_left, lower_right] = grove.border();
    for (int y{upper_left[1] - 2}; y <= lower_right[1] + 2; ++y) {
      for (int x{upper_left[0] - 2}; x <= lower_right[0] + 2; ++x) {
        os << (grove.elves.count(Point{{x, y}}) ? '#' : '.');
      }
      os << '\n';
    }

    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Grove grove(input);
  // std::cout << grove << std::endl;
  for (size_t round{1}; round <= 10; ++round) {
    // std::cout << "Round #" << round << std::endl;
    grove.step();
    // std::cout << grove << std::endl;
  }
  return grove.score();
}

auto part_two(const std::vector<std::string> &input) {
  Grove grove(input);
  size_t result{1};

  while (grove.step())
    ++result;
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_23_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_23.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
