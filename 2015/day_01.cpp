#include <stdexcept>

#include "../utils/input.hpp"

int part_one(std::string input) {
  int floor{0};

  for (auto symbol : input) {
    switch (symbol) {
      case '(':
        floor++;
        break;
      case ')':
        floor--;
        break;
      default:
        throw std::runtime_error("Unrecognized symbol");
        break;
    }
  }

  return floor;
}

int part_two(std::string input) {
  int floor{0};
  int position{0};

  for (auto symbol : input) {
    position++;
    switch (symbol) {
      case '(':
        floor++;
        break;
      case ')':
        floor--;
        break;
      default:
        throw std::runtime_error("Unrecognized symbol");
        break;
    }

    if (floor == -1) break;
  }

  return position;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_01.txt"));
  std::string input = reader.get_lines()[0];

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
