#include <cassert>
#include <map>

#include "../utils/input.hpp"

template<size_t multiplicity>
bool contains_multiple(const std::string& candidate) {
  std::map<char, size_t> counts;
  for (auto character : candidate) {
    counts.try_emplace(character, 0);
    counts.at(character)++;
  }

  for (auto [key, value] : counts) {
    if (value == multiplicity) {
      return true;
    }
  }

  return false;
}

bool are_close(const std::string& lhs, const std::string& rhs) {
  if (lhs.size() != rhs.size()) {
    return false;
  }
  if (lhs == rhs) {
    return false;
  }

  size_t differences{0};
  for (size_t index{0}; index < lhs.size(); index++) {
    differences += lhs[index] != rhs[index];
  }

  return differences == 1;
}

size_t part_one(const std::vector<std::string>& input) {
  size_t twin_count{0}, tripled_count{0};
  for (auto line : input) {
    twin_count += contains_multiple<2>(line);
    tripled_count += contains_multiple<3>(line);
  }

  return twin_count * tripled_count;
}

std::string part_two(const std::vector<std::string>& input) {
  std::string lhs, rhs;
  bool searching{true};
  for (size_t left_index{0};
       left_index + 1 < input.size() && searching;
       left_index++) {
    for (size_t right_index{left_index+1};
         right_index < input.size() && searching;
         right_index++) {
      if (are_close(input[left_index], input[right_index])) {
        lhs = input[left_index];
        rhs = input[right_index];
        assert(lhs.size() == rhs.size());
        searching = false;
      }
    }
  }
  std::string result;
  for (size_t index{0}; index < lhs.size(); index++) {
    if (lhs[index] == rhs[index]) {
      result += lhs[index];
    }
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_02.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
