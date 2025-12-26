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

auto part_two(const std::vector<std::string> &data) {
  std::set<int> cols;
  std::map<std::pair<int, int>, int64_t> paths;
  for (int y{0}; y < static_cast<int>(data.size()); ++y) {
    for (int x{0}; x < static_cast<int>(data[0].size()); ++x) {
      paths.insert({{x, y}, 0});
    }
  }
  int col = std::find(data[0].rbegin(), data[0].rend(), 'S') - data[0].rbegin();
  cols.insert(col);
  int row{0};
  std::pair<int, int> pos{col, row};
  ++paths.at(pos);
  while (++row < static_cast<int>(data.size())) {
    std::set<int> new_cols;
    for (const auto &col : cols) {
      if (data[row][col] == '^') {
        paths.at({col - 1, row}) += paths.at({col, row - 1});
        new_cols.insert(col - 1);

        paths.at({col + 1, row}) += paths.at({col, row - 1});
        new_cols.insert(col + 1);
      } else {
        paths.at({col, row}) += paths.at({col, row - 1});
        new_cols.insert(col);
      }
    }
    cols = new_cols;
  }
  int64_t result{0};
  for (col = 0; col < static_cast<int>(data[0].size()); ++col) {
    std::pair<int, int> pos{col, static_cast<int>(data.size() - 1)};
    result += paths[pos];
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_07_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_07.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
