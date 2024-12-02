#include "../utils/input.hpp"

auto parse(const std::vector<std::string> &input) {
  std::vector<int> left, right;
  for (const auto &line : input) {
    auto segments = utils::split_string(line, ' ');
    left.push_back(std::stoi(segments[0]));
    right.push_back(std::stoi(segments[segments.size() - 1]));
  }
  return std::make_pair(left, right);
}

auto part_one(std::vector<int> left, std::vector<int> right) {
  std::sort(left.begin(), left.end());
  std::sort(right.begin(), right.end());
  int result{0};

  assert(left.size() == right.size());
  for (size_t index{0}; index < left.size(); index++) {
    result += abs(left[index] - right[index]);
  }
  return result;
}

auto part_two(std::vector<int> left, std::vector<int> right) {
  int result{0};
  for (auto l : left) {
    int count{0};
    for (auto r : right) {
      if (l == r) {
        ++count;
      }
    }
    result += l * count;
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_01_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_01.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto [left, right] = parse(input);

  std::cout << std::format("The answer to part one is: {}",
                           part_one(left, right))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}",
                           part_two(left, right))
            << std::endl;

  return 0;
}
