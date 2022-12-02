#include "../utils/input.hpp"

#include <algorithm>

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<std::vector<int>> result;
  std::vector<int> inventory;

  for (auto line : input) {
    if (line.size() == 0) {
      if (inventory.size() > 0) {
        result.push_back(inventory);
        inventory = std::vector<int>{};
      }
    } else {
      inventory.push_back(std::stoi(line));
    }
  }

  if (inventory.size() > 0) {
    result.push_back(inventory);
  }

  return result;
}

auto part_one(const std::vector<std::vector<int>>& input) {
  int answer{0};
  for (const auto& inventory: input) {
    int sum = std::accumulate(inventory.begin(), inventory.end(), 0);
    if (sum > answer) {
      answer = sum;
    }
  }

  return answer;
}

// auto part_two(const std::vector<std::vector<int>>& input) {
//   for (auto line : input) {
//     fmt::print("{}\n", line);
//   }
// 
//   return 2;
// }

int main() {
  // std::filesystem::path input_path{"../../data/2021/input_01_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_01.txt"};
  utils::Reader reader(input_path);
  auto input = prepare_input(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(input));
  // fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
