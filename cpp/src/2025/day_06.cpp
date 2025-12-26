#include "../utils/input.hpp"

std::vector<std::vector<int64_t>>
parse_input(const std::vector<std::string> &data) {
  std::vector<std::vector<int64_t>> parsed_data;
  for (const auto &line : data) {
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

auto part_two(std::vector<std::string> &data) {
  int64_t result{0};
  std::vector<int64_t> numbers;
  for (int col{static_cast<int>(data[0].size()) - 1}; col >= 0; --col) {
    int64_t tmp{0};
    for (int row{0}; row < static_cast<int>(data.size()); ++row) {
      char ch = data[row][col];
      if (ch == ' ') {
        if (tmp != 0) {
          numbers.push_back(tmp);
        }
        tmp = 0;
      } else if (ch == '+') {
        numbers.push_back(tmp);
        tmp = std::transform_reduce(numbers.rbegin(), numbers.rend(), 0LL,
                                    std::plus<>(), [](int64_t n) { return n; });
        numbers.clear();
        result += tmp;
        tmp = 0;
        continue;
      } else if (ch == '*') {
        numbers.push_back(tmp);
        tmp = std::transform_reduce(numbers.rbegin(), numbers.rend(), 1LL,
                                    std::multiplies<>(),
                                    [](int64_t n) { return n == 0 ? 1 : n; });
        numbers.clear();
        result += tmp;
        tmp = 0;
        continue;
      } else {
        tmp = tmp * 10 + (ch - '0');
      }
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_06_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_06.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}",
                           part_one(parse_input(data)))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
