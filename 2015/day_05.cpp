#include <regex>

#include "../utils/input.hpp"

class Checker {
private:

public:
  Checker() = default;

  virtual bool is_valid(const std::string& candidate) = 0;
};

class NiceString : Checker {
private:
  bool has_three_vowels(const std::string& candidate) {
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

  bool has_pair(const std::string& candidate) {
    for (size_t index{1}; index < candidate.length(); index++) {
      if (candidate[index-1] == candidate[index])
        return true;
    }
    return false;
  }

  bool has_no_forbidden_pair(const std::string& candidate) {
    std::regex re{"^.*(ab|cd|pq|xy).*$"};

    return !std::regex_match(candidate, re);
  }

public:
  NiceString() = default;

  bool is_valid (const std::string& input) override {
    return (
      has_three_vowels(input) &&
      has_pair(input) &&
      has_no_forbidden_pair(input)
    );
  }
};

class SuperNiceString : Checker {
private:
  bool has_pair(const std::string& candidate) {
    for (size_t left{0}; left + 3 < candidate.length(); left++) {
      for (size_t right{left+2}; right + 1 < candidate.length(); right++) {
        if(candidate[left] == candidate[right] && candidate[left+1] == candidate[right+1])
          return true;
      }
    }
    return false;
  }

  bool has_clamp(const std::string& candidate) {
    for (size_t index{0}; index + 2 < candidate.length(); index ++) {
      if (candidate[index] == candidate[index+2])
        return true;
    }
    return false;
  }

public:
  SuperNiceString() = default;

  bool is_valid (const std::string& candidate) override {
    return has_clamp(candidate) && has_pair(candidate);
  }
};

int part_one(const std::vector<std::string>& input) {
  int nice_counter{0};
  NiceString checker;
  for (auto candidate: input)
    nice_counter += (int)checker.is_valid(candidate);

  return nice_counter;
}

int part_two(const std::vector<std::string>& input) {
  int nice_counter{0};
  SuperNiceString checker;
  for (auto candidate: input)
    nice_counter += (int)checker.is_valid(candidate);

  return nice_counter;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_05.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}