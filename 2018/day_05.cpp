#include <cassert>

#include "../utils/input.hpp"

bool react(std::string& polymer) {
    for (size_t index{0}; index + 1 < polymer.size(); index++) {
        if (
            ('a' <= polymer[index] && polymer[index] <= 'z' && polymer[index] + 'A' - 'a' == polymer[index+1]) ||
            ('A' <= polymer[index] && polymer[index] <= 'Z' && polymer[index+1] + 'A' - 'a' == polymer[index])
        ) {
            polymer = polymer.substr(0, index) + polymer.substr(index + 2, std::string::npos);
            return true;
        }
    }
    return false;
}


size_t part_one(const std::string& input) {
    auto polymer = input;
    do {
    } while (react(polymer));

    return polymer.size();
}

size_t part_two(const std::string& input) {
    size_t score{std::numeric_limits<size_t>::max()};
    for (char c{'a'}; c <= 'z'; c++) {
        auto polymer = input;
        utils::replace_all_substrings(polymer, std::string(1, c), "");
        utils::replace_all_substrings(polymer, std::string(1, c + 'A' - 'a'), "");

        do {
        } while (react(polymer));

        if (polymer.size() < score) {
            score = polymer.size();
        }
    }
    return score;
}



int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_05.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

