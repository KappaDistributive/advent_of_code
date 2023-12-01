#include "../utils/input.hpp"
#include <iterator>
#include <vector>

auto part_one(const std::vector<std::string>& input) {
  size_t total = 0;
  for (auto line: input) {
    size_t value = 0;
    for (auto c : line) {
      if (c >= '0' && c <= '9') {
        value = 10 * (size_t)(c - '0');
        break;
      }
    }
    for (auto it{std::rbegin(line)}; it != std::rend(line); ++it) {
      auto c = *it;
      if (c >= '0' && c <= '9') {
        value += (size_t)(c - '0');
        break;
      }
    }
    fmt::print("{}\n", value);
    total += value;
  }
  return total;
}

auto part_two(void) {
  return 2;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_01_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_01.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two());

  return 0;
}
