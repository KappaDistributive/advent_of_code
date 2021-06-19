#include "../utils/input.hpp"

const int first_code{20151125};
const int multiplier{252533};
const int divisor{33554393};

std::tuple<int, int> coordinate_generator() {
  static int row_index{0}, column_index{0};

  if (row_index == 0) {
    row_index = 1;
    column_index = 1;
  } else if (row_index == 1) {
    row_index = column_index + 1;
    column_index = 1;
  } else {
    row_index -= 1;
    column_index += 1;
  }

  return {row_index, column_index};
}

std::tuple<int, int> prepare_input(const std::string& input) {
  std::regex re{"^[^\\d]*(\\d+)[^\\d]*(\\d+)[^\\d]*$"};
  std::smatch matches;
  std::regex_match(input, matches, re);
  assert(matches.size() == 3);

  return {std::stoi(matches[1]), std::stoi(matches[2])};
}

int part_one(const std::string& input) {
  auto target = prepare_input(input);
  auto coordinate = coordinate_generator();
  int64_t code = first_code;

  while (coordinate != target) {
    code = (code * multiplier) % divisor;
    coordinate = coordinate_generator();
  }

  return code;
}

// int part_two(const std::string& input) {
//     return 2;
// }

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_25.txt"));
  const auto input = reader.get_lines()[0];

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

