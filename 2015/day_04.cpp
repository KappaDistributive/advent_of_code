#include "../includes/md5.hpp"
#include "../utils/input.hpp"

int find_offset(const std::string& input, size_t zeros) {
  int offset{-1};
  std::string candidate;
  bool done = false;

  while (!done) {
    done = true;
    candidate = md5(input + std::to_string(++offset));
    for (size_t index{0}; index < zeros; index++) {
      if (candidate[index] != '0') {
        done = false;
        break;
      }
    }
  }
  return offset;
}

int part_one(const std::string& input) { return find_offset(input, 5); }

int part_two(const std::string& input) { return find_offset(input, 6); }

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_04.txt"));
  std::string input = reader.get_lines()[0];

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

