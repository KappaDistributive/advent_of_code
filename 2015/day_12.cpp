#include "../utils/input.hpp"

int part_one(const std::string& input) {
  int result{0};
  std::regex numbers_regex{"(-?\\d+)"};
  auto beginning =
      std::sregex_iterator(input.begin(), input.end(), numbers_regex);
  auto ending = std::sregex_iterator();

  for (std::sregex_iterator i = beginning; i != ending; i++) {
    std::smatch match = *i;
    result += std::stoi(match.str());
  }

  return result;
}

// int part_two(const std::string& input) {
//   int result{0};
//   // TODO: solve part_two in C++
//
//   return result;
// }

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_12.txt"));
  auto input = reader.get_lines()[0];

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  // std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

