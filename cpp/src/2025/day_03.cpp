#include "../utils/input.hpp"

std::vector<std::vector<int>>
parse_input(const std::vector<std::string> &data) {
  std::vector<std::vector<int>> result;
  for (const auto &line : data) {
    std::vector<int> row;
    for (const auto &ch : line) {
      row.push_back(ch - '0');
    }
    result.push_back(row);
  }
  return result;
}

int max_idx(const std::vector<int> &vec, size_t start, size_t left) {
  int idx = start;
  for (size_t i{start + 1}; i + left < vec.size(); ++i) {
    if (vec[i] > vec[idx]) {
      idx = i;
    }
  }
  return idx;
}

auto part_one(const std::vector<std::vector<int>> &data) {
  int result{0};
  for (const auto &row : data) {
    int left = max_idx(row, 0, 1);
    int right = max_idx(row, left + 1, 0);
    result += 10 * row[left] + row[right];
  }

  return result;
}

auto part_two(const std::vector<std::vector<int>> &data) {
  int64_t result{0};
  for (const auto &row : data) {
    int64_t index{0};
    int64_t number{0};
    for (size_t i{0}; i < 12; ++i) {
      index = max_idx(row, index, 11 - i);
      number = 10 * number + row[index];
      index += 1;
    }
    result += number;
  }

  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_03_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_03.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
