#include "../utils/input.hpp"

auto eval(const std::map<std::string, std::string> &input,
          const std::string &name = "root") {
  auto value = input.at(name);
  int64_t result{0};
  try {
    return static_cast<int64_t>(std::stoi(value));
  } catch (...) {
    auto splits = utils::split_string(value, ' ');
    assert(splits.size() == 3);
    switch (splits[1][0]) {
    case '+':
      result = eval(input, splits[0]) + eval(input, splits[2]);
      break;
    case '-':
      result = eval(input, splits[0]) - eval(input, splits[2]);
      break;
    case '*':
      result = eval(input, splits[0]) * eval(input, splits[2]);
      break;
    case '/':
      result = eval(input, splits[0]) / eval(input, splits[2]);
      break;
    default:
      throw std::runtime_error("");
    }
  }
  return result;
}

std::map<std::string, std::string>
parse_input(const std::vector<std::string> &input) {
  std::map<std::string, std::string> result;
  for (const auto &line : input) {
    auto splits = utils::split_string(line, ':');
    assert(splits.size() == 2);
    result.insert(std::make_pair(splits[0], splits[1].substr(1)));
  }
  return result;
}

auto part_one(const std::vector<std::string> &input) {
  return eval(parse_input(input));
}

auto part_two(const std::vector<std::string> &input) { return 0; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_21_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_21.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
