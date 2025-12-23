#include "../utils/input.hpp"

auto part_one(const std::vector<std::string> &data) {
  int result{0};
  for (const auto &line : data) {
    int max{0};
    for (size_t i{0}; i < line.size(); ++i) {
      for (size_t j{i + 1}; j < line.size(); ++j) {
        max = std::max(max, (line[i] - '0') * 10 + (line[j] - '0'));
      }
    }
    result += max;
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_03_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_03.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
