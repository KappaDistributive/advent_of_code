#include "../utils/input.hpp"

std::pair<int, size_t> get_number(const std::string &line, size_t pos) {
  int result{0};
  for (size_t i{0}; i < 4; ++i) {
    if (line[pos] >= '0' && line[pos] <= '9') {
      result = result * 10 + (line[pos] - '0');
    } else {
      break;
    }
    ++pos;
  }
  return {result, pos};
}

auto part_one(const std::vector<std::string> &input) {
  size_t result{0};
  for (const auto &line : input) {
    size_t pos{0};

    while (true) {
      pos = line.find("mul(", pos);
      if (pos == std::string::npos || pos + 7 >= line.size()) {
        break;
      }
      pos += 4;
      auto left = get_number(line, pos);
      if (left.second == pos || line[left.second] != ',') {
        ++pos;
        continue;
      }
      pos = left.second + 1;
      auto right = get_number(line, pos);
      if (right.second == pos || line[right.second] != ')') {
        ++pos;
        continue;
      }
      pos = right.second + 1;
      result += left.first * right.first;
    }
  }

  return result;
}

auto part_two(const std::vector<std::string> &input) {
  size_t result{0};
  std::string combined_input{""};
  for (auto line : input) {
    combined_input += " " + line;
  }
  auto splits = utils::split_string(combined_input, "do()");
  for (const auto &split : splits) {
    result += part_one({split.substr(0, split.find("don't()"))});
  }

  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_03_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_03.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;
  return 0;
}
