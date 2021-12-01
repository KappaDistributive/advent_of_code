#include <iostream>
#include <string>
#include <vector>

#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<int> result;

  for (auto line : input) {
    result.push_back(std::stoi(line));
  }

  return result;
}

auto part_one(const std::vector<std::string>& input) {
  auto depths = prepare_input(input);
  size_t result{0};

  for (size_t index{1}; index < depths.size(); ++index) {
    result += depths[index] > depths[index - 1];
  }

  return result;
}

auto part_two(const std::vector<std::string>& input) {
  auto depths = prepare_input(input);
  size_t result{0};

  for (size_t index{3}; index < depths.size(); ++index) {
    auto previous = depths[index - 3] + depths[index - 2] + depths[index - 1];
    auto current = depths[index - 2] + depths[index - 1] + depths[index];
    result += current > previous;
  }

  return result;
}

int main() {
  // std::filesystem::path input_path{"../2021/data/input_01_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_01.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
