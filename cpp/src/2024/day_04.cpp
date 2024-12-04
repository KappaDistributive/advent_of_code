#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Puzzle {
private:
  std::vector<std::string> m_input;
  size_t m_height, m_width;

public:
  explicit Puzzle(const std::vector<std::string> &input)
      : m_input(input), m_height(input.size()) {
    assert(this->m_input.size() > 0);
    this->m_width = this->m_input[0].size();
    for (size_t i = 1; i < this->m_height; i++) {
      assert(this->m_input[i].size() == this->m_width);
    }
  }

  char at(int x, int y) const {
    if (x < 0 || x >= static_cast<int>(this->height()) || y < 0 ||
        y >= static_cast<int>(this->width())) {
      return ' ';
    }
    return this->m_input.at(y).at(x % this->m_width);
  }

  size_t height() const { return this->m_height; }

  size_t width() const { return this->m_width; }
};

auto part_one(const std::vector<std::string> &input) {
  size_t result{0};
  class Puzzle puzzle(input);
  std::string target{"XMAS"};
  std::vector<Point> directions{Point{{1, 0}},  Point{{-1, 0}},  Point{{0, -1}},
                                Point{{0, 1}},  Point{{-1, -1}}, Point{{-1, 1}},
                                Point{{1, -1}}, Point{{1, 1}}};
  for (int y = 0; y < static_cast<int>(puzzle.height()); y++) {
    for (int x = 0; x < static_cast<int>(puzzle.width()); x++) {
      for (const auto &direction : directions) {
        size_t progress{0};
        Point pos{{x, y}};
        for (size_t step{0}; step < target.size(); ++step) {
          if (puzzle.at(pos[0], pos[1]) == target[step]) {
            ++progress;
          } else {
            break;
          }
          pos += direction;
        }
        if (progress == target.size()) {
          ++result;
        }
      }
    }
  }
  return result;
}

auto part_two() { return 1; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_04_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_04.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
