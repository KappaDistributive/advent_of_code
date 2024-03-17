#include "../utils/input.hpp"

int64_t decode(const std::string &snafu) {
  int64_t result{0};
  int64_t place{1};
  int64_t factor{0};
  for (int64_t index{0}; index < static_cast<int64_t>(snafu.size()); ++index) {
    switch (snafu[snafu.size() - index - 1]) {
    case '0':
      factor = 0;
      break;
    case '1':
      factor = 1;
      break;
    case '2':
      factor = 2;
      break;
    case '-':
      factor = -1;
      break;
    case '=':
      factor = -2;
      break;
    default:
      throw std::runtime_error("");
      break;
    }
    result += factor * place;
    place *= 5;
  }
  return result;
}

std::string snafu(int64_t value) {
  std::string result{};
  while (value) {
    int64_t x = value % 5;
    if (x <= 2) {
      result.push_back('0' + x);
      value -= x;
    } else if (x == 3) {
      result.push_back('=');
      value += 2;
    } else {
      assert(x == 4);
      result.push_back('-');
      value += 1;
    }
    value /= 5;
  }
  std::reverse(result.begin(), result.end());

  return result;
}

auto part_one(const std::vector<std::string> &input) {
  int64_t sum{0};
  for (auto l : input) {
    // std::cout << l << '\t' << decode(l) << '\t' << snafu(decode(l)) <<
    // std::endl;
    sum += decode(l);
  }
  return snafu(sum);
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_25_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_25.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two()) << std::endl;

  return 0;
}
