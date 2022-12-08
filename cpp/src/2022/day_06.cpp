#include "../utils/input.hpp"


auto part_one(const std::string &input) {
  std::string seen;
  for (size_t index{0}; index < input.size(); ++index) {
    if (seen.size() == 4) {
      return index;
    } else if (seen.find(input[index]) == std::string::npos) {
      seen += input[index];
    } else {
      auto offset = seen.find(input[index]);
      seen = seen.substr(offset + 1, std::string::npos);
      seen += input[index];
    }
  }
    throw std::runtime_error("");
}

auto part_two(const std::string &input) {
  return 2;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_06_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_06.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines()[0];

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
