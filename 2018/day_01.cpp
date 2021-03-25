#include <limits>
#include <set>

#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input) {
  std::vector<int> changes;
  for (auto line : input) {
    changes.push_back(std::stoi(line));
  }
  return changes;
}

int part_one(const std::vector<std::string>& input) {
  auto changes = prepare_input(input);
  int frequency{0};
  for (auto change : changes) {
    frequency += change;
  }
  return frequency;
}

int part_two(const std::vector<std::string>& input) {
  auto changes = prepare_input(input);
  std::set<int> frequencies{0};
  int frequency{0};
  while (1) {
    for (auto change : changes) {
      frequency += change;
      if (frequencies.count(frequency) > 0) {
        return frequency;
      }
      frequencies.insert(frequency);
    }
  }
  return std::numeric_limits<int>::min();
}

int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_01.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

