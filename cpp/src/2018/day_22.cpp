#include "../utils/input.hpp"
#include "../utils/geometry.hpp"

using Point = utils::geometry::Point<size_t, 2>;

class Cave {
 private:
  Point target;

 public:

  Cave(const std::vector<std::string>& input) {

  }

  size_t geological_index(const Point& position) {
    if ((position[0] == 0 && position[1] == 0) || position == this->target) {
      return 0;
    }
    if (position[1] == 0) {
      return position[0] * 16807;
    }
    if (position[0] == 0) {
      return position[1] * 48271;
    }

    Point lhs{{position[0] - 1, position[1]}}, rhs{{position[0], position[1] - 1}};

    return geological_index(lhs) * geological_index(rhs);

  }

};

auto part_one(const std::vector<std::string> &input) {
  for (auto& line : input) {
    fmt::print("{}\n", line);
  }
  return 1;
}

auto part_two(const std::vector<std::string> &input) {
  return 2;
}

int main() {
  std::filesystem::path input_path{"../../data/2018/input_22_mock.txt"};
  // std::filesystem::path input_path{"../../data/2018/input_22.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
