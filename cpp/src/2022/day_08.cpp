#include "../utils/input.hpp"

auto part_one(const std::string &input) {
  return 1;
}

auto part_two(const std::string &input) {
  return 2;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_07_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_07.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines()[0];

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
