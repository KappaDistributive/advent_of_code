#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input) {
  std::vector<int> modules;
  for (auto line: input) {
    modules.push_back(std::stoi(line));
  }
  return modules;
}

int part_one(const std::vector<std::string>& input) {
  auto modules = prepare_input(input);
  int mass{0};
  for (auto module: modules) {
    mass += (module / 3) - 2;
  }
  return mass;
}

int part_two(const std::vector<std::string>& input) {

  auto modules = prepare_input(input);
  int mass{0};
  for (auto module: modules) {
    int fuel{(module / 3) - 2};
    while (fuel > 0) {
      mass += fuel;
      fuel = (fuel / 3) - 2;
    }
  }
  return mass;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_01.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

