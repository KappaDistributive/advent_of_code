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

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
