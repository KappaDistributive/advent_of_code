#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<size_t, 2>;

class Cave {
private:
  size_t m_depth;
  Point m_target;

public:
  Cave(const std::vector<std::string> &input) {
    std::string temp;
    this->m_depth = std::stoull(input[0].substr(7, std::string::npos));
    auto splits = utils::split_string(input[1], ',');
    assert(splits.size() == 2);
    this->m_target = Point{{std::stoull(splits[0].substr(8, std::string::npos)),
                            std::stoull(splits[1])}};
  }

  size_t geological_index(const Point &position) {
    if ((position[0] == 0 && position[1] == 0) || position == this->m_target) {
      return 0;
    }
    if (position[1] == 0) {
      return position[0] * 16807;
    }
    if (position[0] == 0) {
      return position[1] * 48271;
    }

    Point lhs{{position[0] - 1, position[1]}},
        rhs{{position[0], position[1] - 1}};

    return geological_index(lhs) * geological_index(rhs);
  }

  friend std::ostream &operator<<(std::ostream &os, const Cave &cave) {
    os << cave.m_depth << '\t' << cave.m_target;
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Cave cave{input};
  std::cout << cave << std::endl;
  return 1;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  std::filesystem::path input_path{"../../data/2018/input_22_mock.txt"};
  // std::filesystem::path input_path{"../../data/2018/input_22.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
