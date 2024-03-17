#include "../utils/geometry.hpp"
#include "../utils/input.hpp"
#include <regex>

using Point = utils::geometry::Point<int, 2>;

struct Ground {
  std::set<Point> clay;
  Point upper_left, lower_right;
  std::set<Point> flowing;
  std::set<Point> settled;

  Ground(const std::vector<std::string> &input) {
    std::regex description_regex{"^([xy])=(\\d+), ([xy])=(\\d+)\\.\\.(\\d+)$"};
    std::smatch matches;
    for (const auto &line : input) {
      std::regex_match(line, matches, description_regex);
      if (matches[1].str() == "x") {
        assert(matches[3].str() == "y");
        int x{std::stoi(matches[2].str())};
        for (int y{std::stoi(matches[4].str())};
             y <= std::stoi(matches[5].str()); ++y) {
          this->clay.insert(Point{{x, y}});
        }

      } else {
        assert(matches[1].str() == "y");
        assert(matches[3].str() == "x");
        int y{std::stoi(matches[2].str())};
        for (int x{std::stoi(matches[4].str())};
             x <= std::stoi(matches[5].str()); ++x) {
          this->clay.insert(Point{{x, y}});
        }
      }
    }
    this->upper_left = Point{{500, 0}};
    this->lower_right = Point{{500, 0}};
    if (this->clay.size() > 0) {
      this->upper_left = *this->clay.begin();
      this->lower_right = *this->clay.begin();
    }
    for (const auto &point : this->clay) {
      this->upper_left[0] = std::min(this->upper_left[0], point[0]);
      this->upper_left[1] = std::min(this->upper_left[1], point[1]);
      this->lower_right[0] = std::max(this->lower_right[0], point[0]);
      this->lower_right[1] = std::max(this->lower_right[1], point[1]);
    }
  }

  bool fill(Point point = Point{{500, 0}}, Point direction = Point{{0, 1}}) {
    this->flowing.insert(point);
    Point below{point + Point{{0, 1}}};

    // std::cout << "step\n" << *this << std::endl;

    if (!this->clay.count(below) && !this->flowing.count(below) &&
        1 <= below[1] && below[1] <= this->lower_right[1]) {
      this->fill(below);
    }

    if (!this->clay.count(below) && !this->settled.count(below)) {
      return false;
    }

    Point left{point + Point{{-1, 0}}};
    Point right{point + Point{{1, 0}}};

    bool left_filled =
        this->clay.count(left) ||
        (!this->flowing.count(left) && this->fill(left, Point{{-1, 0}}));
    bool right_filled =
        this->clay.count(right) ||
        (!this->flowing.count(right) && this->fill(right, Point{{1, 0}}));

    if (direction == Point{{0, 1}} && left_filled && right_filled) {
      this->settled.insert(point);
      while (this->flowing.count(left)) {
        this->settled.insert(left);
        --left[0];
      }
      while (this->flowing.count(right)) {
        this->settled.insert(right);
        ++right[0];
      }
    }

    if (direction == Point{{-1, 0}}) {
      return (left_filled || this->clay.count(left));
    }
    if (direction == Point{{1, 0}}) {
      return (right_filled || this->clay.count(right));
    }
    return false;
  }

  friend std::ostream &operator<<(std::ostream &os, const Ground &ground) {
    Point point{{0, 0}};
    for (int y{0}; y <= ground.lower_right[1] + 1; ++y) {
      point[1] = y;
      for (int x{ground.upper_left[0] - 1}; x <= ground.lower_right[0] + 1;
           ++x) {
        point[0] = x;
        if (point[0] == 500 && point[1] == 0) {
          os << '+';
        } else if (ground.clay.count(point)) {
          os << '#';
        } else if (ground.settled.count(point)) {
          os << '~';
        } else if (ground.flowing.count(point)) {
          os << '|';
        } else {
          os << '.';
        }
      }
      os << '\n';
    }

    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Ground ground(input);
  ground.fill();
  size_t result{0};
  for (auto point : ground.flowing) {
    if (point[1] >= ground.upper_left[1] && point[1] <= ground.lower_right[1]) {
      ++result;
    }
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) {
  Ground ground(input);
  ground.fill();
  return ground.settled.size();
}

int main() {
  // std::filesystem::path input_path{"../../data/2018/input_17_mock.txt"};
  std::filesystem::path input_path{"../../data/2018/input_17.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
