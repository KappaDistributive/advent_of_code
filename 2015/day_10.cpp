#include <algorithm>
#include <map>
#include <regex>  // NOLINT
#include <set>

#include "../utils/input.hpp"


std::vector<std::string> split_blocks(const std::string& input) {
  std::vector<std::string> blocks;
  std::string block;

  if (input.size() > 0) {
    block = input[0];

    for (size_t index{1}; index < input.size(); index++) {
      if (input[index] == block[0]) {
        block += input[index];
      } else {
        blocks.push_back(block);
        block = input[index];
      }
    }
    blocks.push_back(block);
  }

  return blocks;
}


int part_one(const std::string& input) {
  std::string current_input{input};
  for (size_t index{0}; index < 40; index++) {
    auto blocks = split_blocks(current_input);
    current_input = "";
    for (auto block : blocks) {
      current_input += std::to_string(block.size()) + block[0];
    }
  }

  return current_input.size();
}


int part_two(const std::string& input) {
  std::string current_input{input};
  for (size_t index{0}; index < 50; index++) {
    auto blocks = split_blocks(current_input);
    current_input = "";
    for (auto block : blocks) {
      current_input += std::to_string(block.size()) + block[0];
    }
  }

  return current_input.size();
}


int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_10.txt"));
  auto input = reader.get_lines()[0];

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
