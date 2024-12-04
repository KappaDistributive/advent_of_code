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

auto part_two(const std::vector<std::string> &input) {
  size_t result{0};
  class Puzzle puzzle(input);

  // clang-format off
  const std::vector<std::vector<std::pair<char, Point>>> targets{
    // 1
    {{'M', Point{{-1, -1}}}, /*                    */ {'S', Point{{+1, -1}}},
    /*                   */ {'A', Point{{+0, +0}}}, /*                  */
    {'M', Point{{-1, +1}}}, /*                    */ {'S', Point{{+1, +1}}}},
    // 2
    {{'S', Point{{-1, -1}}}, /*                    */ {'S', Point{{+1, -1}}},
    /*                   */ {'A', Point{{+0, +0}}}, /*                  */
    {'M', Point{{-1, +1}}}, /*                    */ {'M', Point{{+1, +1}}}},
    // 3
    {{'S', Point{{-1, -1}}}, /*                    */ {'M', Point{{+1, -1}}},
    /*                   */ {'A', Point{{+0, +0}}}, /*                  */
    {'S', Point{{-1, +1}}}, /*                    */ {'M', Point{{+1, +1}}}},
    // 4
    {{'M', Point{{-1, -1}}}, /*                    */ {'M', Point{{+1, -1}}},
    /*                   */ {'A', Point{{+0, +0}}}, /*                  */
    {'S', Point{{-1, +1}}}, /*                    */ {'S', Point{{+1, +1}}}},
  };
  // clang-format on

  for (int y = 0; y < static_cast<int>(puzzle.height()); y++) {
    for (int x = 0; x < static_cast<int>(puzzle.width()); x++) {
      const Point pos{{x, y}};
      for (const auto &target : targets) {
        bool hit{true};
        for (const auto &[letter, direction] : target) {
          Point current_pos = pos + direction;
          if (puzzle.at(current_pos[0], current_pos[1]) != letter) {
            hit = false;
            break;
          }
        }
        if (hit) {
          ++result;
        }
      }
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_04_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_04.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
