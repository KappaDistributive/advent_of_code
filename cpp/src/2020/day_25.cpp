#include <iostream>
#include <string>
#include <vector>

#include "../utils/input.hpp"

int64_t part_one(const std::vector<std::string>& input) {
  const int64_t subject_number{7};
  const int64_t card_public_key{std::stoi(input[0])};
  const int64_t door_public_key{std::stoi(input[1])};
  const int64_t divider{20201227};

  int64_t a{1}, b{1};
  while (true) {
    a = (a * subject_number) % divider;
    b = (b * card_public_key) % divider;
    if (a == door_public_key) {
      return b;
    }
  }
  return -1;
}

// int64_t part_two(const std::vector<std::string>& input) {
//   return 17;
// }

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2020/input_25.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  // std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
