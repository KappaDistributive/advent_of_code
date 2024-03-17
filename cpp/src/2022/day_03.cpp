#include <cassert>
#include <span>
#include <string_view>

#include "../utils/input.hpp"

int priority(const char &item) {
  if ('a' <= item && item <= 'z') {
    return 1 + item - 'a';
  } else {
    assert('A' <= item && item <= 'Z');
    return 27 + item - 'A';
  }
}

auto part_one(const std::vector<std::string> &input) {
  int answer{0};
  size_t capacity;
  for (auto rucksacks : input) {
    capacity = rucksacks.size();
    assert(capacity > 0 && capacity % 2 == 0);
    std::string_view lhs{&*std::begin(rucksacks), capacity / 2};
    std::string_view rhs{&*(std::begin(rucksacks) + capacity / 2),
                         capacity / 2};

    for (const auto &item : lhs) {
      if (rhs.find(item) != std::string::npos) {
        answer += priority(item);
        break;
      }
    }
  }
  return answer;
}

auto part_two(const std::vector<std::string> &input) {
  int answer{0};
  for (size_t index{0}; index + 2 < input.size(); index += 3) {
    for (const auto &item : input[index]) {
      if (input[index + 1].find(item) != std::string::npos &&
          input[index + 2].find(item) != std::string::npos) {
        answer += priority(item);
        break;
      }
    }
  }
  return answer;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_03_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_03.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
