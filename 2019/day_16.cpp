#include <algorithm>
#include <cassert>
#include <iomanip>

#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::string& input) {
  std::vector<int> result;
  std::transform(
      input.begin(), input.end(),
      std::back_inserter(result),
      [] (char character) -> int {
        return static_cast<int>(character - '0');
      });

  return result;
}


int factor(size_t index, size_t offset) {
  assert(index >= 1);
  auto lookup = (static_cast<int>(offset + 1) / static_cast<int>(index)) % 4;
  int result{0};
  switch (lookup) {
    case 0: result =  0; break;
    case 1: result =  1; break;
    case 2: result =  0; break;
    case 3: result = -1; break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
  }
  return result;
}

int
next_digit(size_t index, const std::vector<int>& digits) {
  int64_t candidate{0};
  for (size_t offset{0}; offset < digits.size(); ++offset) {
    candidate += digits[offset] * factor(index + 1, offset);
  }

  return static_cast<int>(std::abs(candidate) % 10);
}


auto
part_one(const std::string& input) {
  auto digits = prepare_input(input);

  for (size_t phase{0}; phase < 100; ++phase) {
    for (size_t index{0}; index < digits.size(); ++index) {
      digits[index] = next_digit(index, digits);
    }
  }

  int result{0};
  for (size_t index{0}; index < 8; ++index) {
    result = result * 10 + digits[index];
  }

  return result;
}


auto
part_two(const std::string& input) {
  return 2;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_16.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

