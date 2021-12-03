#include "../utils/input.hpp"

int decode(const std::vector<std::string>& input, bool gamma) {
  const auto num_bits = input[0].size();
  std::vector<size_t> ones(num_bits, 0);

  for (auto code : input) {
    for (size_t power{0}; power < num_bits; ++power) {
      ones[power] += code[num_bits - power - 1] == '1';
    }
  }

  int result{0};
  for (size_t power{0}; power < num_bits; ++power) {
    if (gamma) {
      if (2 * ones[power] > input.size()) {
        result += utils::pow(2, static_cast<int>(power));
      }
    } else {
      if (2 * ones[power] < input.size()) {
        result += utils::pow(2, static_cast<int>(power));
      }
    }
  }

  return result;
}

auto part_one(const std::vector<std::string>& input) {
  auto gamma = decode(input, true);
  auto epsilon = decode(input, false);

  return gamma * epsilon;
}

auto part_two(const std::vector<std::string>& input) { return 1; }

int main() {
  // std::filesystem::path input_path{"../2021/data/input_03_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_03.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
