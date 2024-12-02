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

auto part_one(const std::vector<std::vector<int>> &data) {
  int result{0};
  for (const auto &row : data) {
    int direction = row[0] < row[1] ? 1 : -1;
    bool safe{true};
    for (size_t index{0}; index < row.size() - 1; ++index) {
      int temp = direction * (row[index + 1] - row[index]);
      if (temp < 1 || temp > 3) {
        safe = false;
        break;
      }
    }
    if (safe) {
      ++result;
    }
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_01_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_02.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();
  auto data = parse(input);

  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
