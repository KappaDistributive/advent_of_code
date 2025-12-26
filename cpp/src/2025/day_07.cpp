#include "../utils/input.hpp"

auto part_one(const std::vector<std::string> &data) {
  int result{0};
  std::set<int> cols;
  cols.insert(std::find(data[0].rbegin(), data[0].rend(), 'S') -
              data[0].rbegin());
  int row{0};
  while (++row < static_cast<int>(data.size())) {
    std::set<int> new_cols;
    for (const auto &col : cols) {
      if (data[row][col] == '^') {
        ++result;
        new_cols.insert(col - 1);
        new_cols.insert(col + 1);
      } else {
        new_cols.insert(col);
      }
    }
    cols = new_cols;
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_07_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_07.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
