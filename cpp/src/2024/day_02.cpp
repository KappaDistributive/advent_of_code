#include "../utils/input.hpp"

std::vector<std::vector<int>> parse(const std::vector<std::string> &input) {
  std::vector<std::vector<int>> data;
  for (const auto &line : input) {
    std::vector<int> row;
    auto segments = utils::split_string(line, ' ');
    for (const auto &segment : segments) {
      row.push_back(std::stoi(segment));
    }
    data.push_back(row);
  }
  return data;
}

bool is_safe(const std::vector<int> &row) {
  int direction = row[0] < row[1] ? 1 : -1;
  for (size_t index{0}; index < row.size() - 1; ++index) {
    int temp = direction * (row[index + 1] - row[index]);
    if (temp < 1 || temp > 3) {
      return false;
    }
  }
  return true;
}

bool is_safe_soft(const std::vector<int> &row) {
  if (is_safe(row)) {
    return true;
  }
  for (size_t index{0}; index < row.size(); ++index) {
    // skip index element in row
    std::vector<int> temp_row;
    for (size_t i{0}; i < row.size(); ++i) {
      if (i != index) {
        temp_row.push_back(row[i]);
      }
    }
    if (is_safe(temp_row)) {
      return true;
    }
  }
  return false;
}

auto part_one(const std::vector<std::vector<int>> &data) {
  int result{0};
  for (const auto &row : data) {
    if (is_safe(row)) {
      ++result;
    }
  }
  return result;
}

auto part_two(const std::vector<std::vector<int>> &data) {
  int result{0};
  for (const auto &row : data) {
    if (is_safe_soft(row)) {
      ++result;
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_01_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_02.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();
  auto data = parse(input);

  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return 0;
}
