#include "../utils/input.hpp"

enum Part { p_one, p_two };

template <Part p>
char mode(const std::vector<std::string>& input, size_t position);

template <>
char mode<p_one>(const std::vector<std::string>& input, size_t position) {
  std::array<size_t, 1 + 'z' - 'a'> frequencies =
      std::array<size_t, 1 + 'z' - 'a'>{};
  for (auto word : input) {
    assert(word[position] >= 'a' && word[position] <= 'z');
    frequencies[word[position] - 'a']++;
  }
  return 'a' + std::distance(
                   frequencies.begin(),
                   std::max_element(frequencies.begin(), frequencies.end()));
}

template <>
char mode<p_two>(const std::vector<std::string>& input, size_t position) {
  std::array<size_t, 1 + 'z' - 'a'> frequencies =
      std::array<size_t, 1 + 'z' - 'a'>{};
  for (auto word : input) {
    assert(word[position] >= 'a' && word[position] <= 'z');
    frequencies[word[position] - 'a']++;
  }
  for (size_t index{0}; index < frequencies.size(); index++) {
    if (frequencies[index] == 0) {
      frequencies[index] = input.size() + 1;
    }
  }
  return 'a' + std::distance(
                   frequencies.begin(),
                   std::min_element(frequencies.begin(), frequencies.end()));
}

std::string part_one(const std::vector<std::string>& input) {
  std::string result;
  for (size_t index{0}; index < input[0].size(); index++) {
    result.push_back(mode<p_one>(input, index));
  }
  return result;
}

std::string part_two(const std::vector<std::string>& input) {
  std::string result;
  for (size_t index{0}; index < input[0].size(); index++) {
    result.push_back(mode<p_two>(input, index));
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_06.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}

