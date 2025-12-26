#include "../utils/input.hpp"

std::vector<std::vector<int64_t>>
parse_input(const std::vector<std::string> &data) {
  std::vector<std::vector<int64_t>> parsed_data;
  for (const auto &line : data) {
    std::cout << std::format("Parsing line: {}", line) << std::endl;
    std::vector<int64_t> row;
    int64_t curr{0};
    int i{0};
    while (line[i] == ' ') {
      ++i;
    }
    for (; i < static_cast<int>(line.size()); ++i) {
      bool pushed{false};
      char ch = line[i];
      if (ch == ' ') {
        row.push_back(curr);
        pushed = true;
        curr = 0;
        while (line[i] == ' ') {
          ++i;
        }
        --i;
      } else {
        if (ch == '+' || ch == '*') {
          curr = ch == '+' ? -1 : -2;
          continue;
        } else {
          curr = curr * 10 + (ch - '0');
        }
      }
      if (!pushed && i + 1 == static_cast<int>(line.size())) {
        row.push_back(curr);
      }
    }
    parsed_data.push_back(row);
  }
  return parsed_data;
}

auto part_one(const std::vector<std::vector<int64_t>> &data) {
  int64_t result{0};
  for (size_t col{0}; col < data[0].size(); ++col) {
    bool addition = data[data.size() - 1][col] == -1;
    size_t tmp = addition ? 0 : 1;
    for (size_t row{0}; row + 1 < data.size(); ++row) {
      if (addition) {
        tmp += data[row][col];
      } else {
        tmp *= data[row][col];
      }
    }
    result += tmp;
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_06_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_06.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());

  return EXIT_SUCCESS;
}
