#include "../utils/input.hpp"

auto eval(const std::map<std::string, std::string> &input,
          const std::string &name = "root") {
  auto value = input.at(name);
  int64_t result{0};
  try {
    return static_cast<int64_t>(std::stoll(value));
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

auto part_two(const std::vector<std::string> &input) {
  auto instructions = parse_input(input);
  auto splits = utils::split_string(instructions.at("root"), ' ');
  assert(splits.size() == 3);
  instructions.at("root") = splits[0] + " - " + splits[2];
  int64_t guess{1};
  int64_t guess_max{10000000000000};
  int64_t guess_min{1};

  while (guess_max - guess_min > 100) {
    guess = (guess_max + guess_min) / 2;
    instructions.at("humn") = std::to_string(guess);
    auto result = eval(instructions);
    if (result == 0) {
      break;
    } else if (result < 0) {
      guess_max = guess;
    } else {
      guess_min = guess;
    }
  }
  for (guess = guess_min; guess <= guess_max; ++guess) {
    instructions.at("humn") = std::to_string(guess);
    auto result = eval(instructions);
    if (result == 0) {
      return guess;
    }
  }
  return int64_t{0};
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_21_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_21.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
