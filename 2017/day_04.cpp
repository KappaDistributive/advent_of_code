#include <algorithm>
#include<set>

#include "../utils/input.hpp"

int part_one(const std::vector<std::string>& input) {
  int result{0};
  for (auto line: input) {
    auto splits = utils::split_string(line, ' ');
    std::set<std::string> splits_set;
    for (auto split: splits) {
      splits_set.insert(split);
    }
    result += splits_set.size() == splits.size();
  }
  return result;
}

int part_two(const std::vector<std::string>& input) {
  int result{0};
  for (auto line: input) {
    auto splits = utils::split_string(line, ' ');
    std::vector<std::string> splits_vector;
    std::set<std::string> splits_set;
    for (auto split: splits) {
      std::vector<char> characters;
      for (auto character: split) {
        characters.push_back(character);
      }
      std::sort(characters.begin(), characters.end());
      std::string word;
      for (auto character: characters) {
        word += character;
      }
      splits_set.insert(word);
    }
    result += splits_set.size() == splits.size();
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_04.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

