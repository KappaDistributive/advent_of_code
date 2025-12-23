#include "../utils/input.hpp"

std::vector<std::pair<int64_t, int64_t>>
parse_input(const std::vector<std::string> &data) {
  assert(data.size() == 1);
  std::vector<std::pair<int64_t, int64_t>> parsed;
  auto splits = utils::split_string(data[0], ',');
  for (const auto &s : splits) {
    auto coords = utils::split_string(s, '-');
    assert(coords.size() == 2);
    parsed.emplace_back(std::stoll(coords[0]), std::stoll(coords[1]));
  }
  return parsed;
}

bool repeats(int64_t n) {
  auto s = std::to_string(n);
  if (s.size() % 2 != 0) {
    return false;
  }
  for (size_t index{0}; index < s.size() / 2; ++index) {
    if (s[index] != s[s.size() / 2 + index]) {
      return false;
    }
  }
  return true;
}

auto part_one(const std::vector<std::pair<int64_t, int64_t>> &data) {
  int64_t result = 0;
  for (const auto &[start, end] : data) {
    for (int64_t n = start; n <= end; ++n) {
      if (repeats(n)) {
        result += n;
      }
    }
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_02_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_02.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
