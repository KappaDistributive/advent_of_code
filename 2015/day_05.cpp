#include <regex>
#include <stdexcept>

#include "../utils/input.hpp"

class Checker {
private:

public:
  Checker() = default;

  virtual bool is_valid(std::string candidate) = 0;
};

class NiceString : Checker {
private:
  bool has_three_vowels(std::string& candidate) {
    int vowel_counter{0};

    for (auto character: candidate) {
      vowel_counter += (int)(
        character == 'a' ||
        character == 'e' || 
        character == 'i' ||
        character == 'o' ||
        character == 'u'
      );
    }

    return vowel_counter >= 3;
  }

  bool has_pair(std::string& candidate) {
    for (size_t index{1}; index < candidate.length(); index++) {
      if (candidate[index-1] == candidate[index])
        return true;
    }
    return false;
  }

  bool has_no_forbidden_pair(std::string& candidate) {
    std::regex re{"^.*(ab|cd|pq|xy).*$"};

    return !std::regex_match(candidate, re);
  }

public:
  NiceString() = default;

  bool is_valid (std::string input) override {
    return (
      has_three_vowels(input) &&
      has_pair(input) &&
      has_no_forbidden_pair(input)
    );
  }
};

int part_one(const std::vector<std::string>& input) {
  int nice_counter{0};
  NiceString checker;
  for (auto candidate: input) {
    if (checker.is_valid(candidate)) {
      nice_counter++;
    }
  }

  return nice_counter;
}

int part_two(const std::vector<std::string>& input) {
  return 0;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_05.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}