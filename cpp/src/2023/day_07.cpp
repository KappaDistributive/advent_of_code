#include "../utils/input.hpp"

auto part_one() {
  return 11;
}

auto part_two() {
  return 0;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_07_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_07.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one())  
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
