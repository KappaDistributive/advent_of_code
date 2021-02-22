#include <stdexcept>

#include "../utils/input.hpp"

std::pair<bool, int> part_one(std::vector<std::string> input) {
  for (size_t index_left{0}; index_left + 1 < input.size(); index_left++) {
    for (size_t index_right{index_left+1}; index_right < input.size(); index_right++) {
      if (std::stoi(input[index_left]) + std::stoi(input[index_right]) == 2020) {
        return std::pair<bool, int>{true, std::stoi(input[index_left]) * std::stoi(input[index_right])};
      }
    }
  }
  return std::pair<bool, int>{false, 0};
}

std::pair<bool, int> part_two(std::vector<std::string> input) {
  for (size_t index_left{0}; index_left + 2 < input.size(); index_left++) {
    for (size_t index_middle{index_left+1}; index_middle + 1 < input.size(); index_middle++) {
      for (size_t index_right{index_middle+1}; index_right < input.size(); index_right++) {
        if (std::stoi(input[index_left]) + std::stoi(input[index_middle]) + std::stoi(input[index_right]) == 2020) {
          return std::pair<bool, int>{true, std::stoi(input[index_left]) * std::stoi(input[index_middle]) * std::stoi(input[index_right])};
        }
      }
    }
  }
  return std::pair<bool, int>{false, 0};
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_01.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::pair<bool, int> answer_one = part_one(input);
  if (std::get<0>(answer_one))
    std::cout << "The answer to part one is: " << std::get<1>(answer_one) << std::endl;
  else
    std::cout << "Failed to find an answer to part one." << std::endl;

  std::pair<bool, int> answer_two = part_two(input);
  if (std::get<0>(answer_two))
    std::cout << "The answer to part two is: " << std::get<1>(answer_two) << std::endl;
  else
    std::cout << "Failed to find an answer to part two." << std::endl;

  return 0;
}

