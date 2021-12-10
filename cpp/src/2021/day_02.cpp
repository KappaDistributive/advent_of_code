#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<std::pair<std::string, int>> instructions;

  for (auto line : input) {
    auto splits = utils::split_string(line, ' ');
    instructions.push_back(std::make_pair(splits[0], std::stoi(splits[1])));
  }

  return instructions;
}

auto part_one(const std::vector<std::string>& input) {
  auto instructions = prepare_input(input);
  int horizontal{0}, depth{0};

  for (auto instruction : instructions) {
    auto [command, value] = instruction;
    if (command == "up") {
      depth -= value;
    } else if (command == "down") {
      depth += value;
    } else if (command == "forward") {
      horizontal += value;
    } else {
      throw std::runtime_error("Unknown command: " + command);
    }
  }
  return horizontal * depth;
}

auto part_two(const std::vector<std::string>& input) {
  auto instructions = prepare_input(input);
  int horizontal{0}, depth{0}, aim{0};

  for (auto instruction : instructions) {
    auto [command, value] = instruction;
    if (command == "up") {
      aim -= value;
    } else if (command == "down") {
      aim += value;
    } else if (command == "forward") {
      horizontal += value;
      depth += aim * value;
    } else {
      throw std::runtime_error("Unknown command: " + command);
    }
  }

  return horizontal * depth;
}

int main() {
  // std::filesystem::path input_path{"../../data/2021/input_02_mock.txt"};
  std::filesystem::path input_path{"../../data/2021/input_02.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
