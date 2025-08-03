#include "../utils/input.hpp"
#include <cassert>

auto hash(const std::string &s) {
  int result{0};
  for (const auto c : s) {
    result += int(c);
    result *= 17;
    result %= 256;
    // std::cout << std::format("Hashing: {} -> {}", s, c, result) << std::endl;
  }
  return result;
}

auto part_one(const std::vector<std::string> &input) {
  int result{0};
  for (const auto &line : input) {
    auto splits = utils::split_string(line, ',');
    for (const auto &split : splits) {
      result += hash(split);
    }
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_15_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_15.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
