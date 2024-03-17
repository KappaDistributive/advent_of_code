#include "../utils/input.hpp"

auto part_one(const std::vector<std::string> &input) {
  size_t total = 0;
  for (auto line : input) {
    size_t value = 0;
    for (auto c : line) {
      if (c >= '0' && c <= '9') {
        value = 10 * (size_t)(c - '0');
        break;
      }
    }
    for (auto it{std::rbegin(line)}; it != std::rend(line); ++it) {
      auto c = *it;
      if (c >= '0' && c <= '9') {
        value += (size_t)(c - '0');
        break;
      }
    }
    // std::cout << std::format("{}", value) << std::endl;
    total += value;
  }
  return total;
}

auto part_two(const std::vector<std::string> &input) {
  size_t result{0};
  for (auto line : input) {
    size_t value{0};
    for (size_t index{0}; index < line.length(); ++index) {
      if ('0' <= line[index] && line[index] <= '9') {
        value += (size_t)(line[index] - '0');
        break;
      } else if (line.substr(index, 3) == "one") {
        value += 1;
        break;
      } else if (line.substr(index, 3) == "two") {
        value += 2;
        break;
      } else if (line.substr(index, 5) == "three") {
        value += 3;
        break;
      } else if (line.substr(index, 4) == "four") {
        value += 4;
        break;
      } else if (line.substr(index, 4) == "five") {
        value += 5;
        break;
      } else if (line.substr(index, 3) == "six") {
        value += 6;
        break;
      } else if (line.substr(index, 5) == "seven") {
        value += 7;
        break;
      } else if (line.substr(index, 5) == "eight") {
        value += 8;
        break;
      } else if (line.substr(index, 4) == "nine") {
        value += 9;
        break;
      }
    }
    value *= 10;
    for (size_t index{0}; index <= line.length(); ++index) {
      auto position = line.length() - index;
      if (position < line.length() && '0' <= line[position] &&
          line[position] <= '9') {
        value += (size_t)(line[position] - '0');
        break;
      } else if (position >= 3 && line.substr(position - 3, 3) == "one") {
        value += 1;
        break;
      } else if (position >= 3 && line.substr(position - 3, 3) == "two") {
        value += 2;
        break;
      } else if (position >= 5 && line.substr(position - 5, 5) == "three") {
        value += 3;
        break;
      } else if (position >= 4 && line.substr(position - 4, 4) == "four") {
        value += 4;
        break;
      } else if (position >= 4 && line.substr(position - 4, 4) == "five") {
        value += 5;
        break;
      } else if (position >= 3 && line.substr(position - 3, 3) == "six") {
        value += 6;
        break;
      } else if (position >= 5 && line.substr(position - 5, 5) == "seven") {
        value += 7;
        break;
      } else if (position >= 5 && line.substr(position - 5, 5) == "eight") {
        value += 8;
        break;
      } else if (position >= 4 && line.substr(position - 4, 4) == "nine") {
        value += 9;
        break;
      }
    }
    // std::cout << std::format("{} -> {}", line, value) << std::endl;
    result += value;
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_01_mock2.txt"};
  std::filesystem::path input_path{"../../data/2023/input_01.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
