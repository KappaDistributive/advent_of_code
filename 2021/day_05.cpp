#include <algorithm>
#include <regex>  // NOLINT

#include "../utils/input.hpp"

static const size_t width{1000}, height{1000};

auto prepare_input(const std::vector<std::string>& input, bool no_diagonals) {
  const std::regex coord_regex{"^(\\d+),(\\d+) -> (\\d+),(\\d+)$"};
  std::smatch matches;
  std::array<int, height * width> map{0};
  for (auto line : input) {
    if (std::regex_match(line, matches, coord_regex)) {
      const int x_0 = std::stoul(matches[1].str());
      const int y_0 = std::stoul(matches[2].str());
      const int x_1 = std::stoul(matches[3].str());
      const int y_1 = std::stoul(matches[4].str());

      if (no_diagonals && x_0 != x_1 && y_0 != y_1) {
        continue;
      }

      if (y_0 <= y_1) {
        for (auto y{y_0}; y <= y_1; ++y) {
          if (x_0 <= x_1) {
            for (auto x{x_0}; x <= x_1; ++x) {
              map[y * width + x] += 1;
            }
          } else {
            for (auto x{x_0}; x >= x_1; --x) {
              map[y * width + x] += 1;
            }
          }
        }
      } else {
        for (auto y{y_0}; y >= y_1; --y) {
          if (x_0 <= x_1) {
            for (auto x{x_0}; x <= x_1; ++x) {
              map[y * width + x] += 1;
            }
          } else {
            for (auto x{x_0}; x >= x_1; --x) {
              map[y * width + x] += 1;
            }
          }
        }
      }
    }
  }

  return map;
}

auto part_one(const std::vector<std::string>& input) {
  auto map = prepare_input(input, true);
  size_t result{0};
  for (auto marker : map) {
    result += marker > 1;
  }
  return result;
}

// auto part_two(const std::vector<std::string>& input) {
//   return 1;
// }

int main() {
  // std::filesystem::path input_path{"../2021/data/input_05_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_05.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;

  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
