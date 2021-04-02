#include <regex>  // NOLINT

#include "../utils/input.hpp"

const size_t DIVISOR = 2147483647;
const size_t MUL_A = 16807;
const size_t MUL_B = 48271;

std::pair<size_t, size_t> prepare_input(const std::vector<std::string>& input) {
  assert(input.size() == 2);
  std::regex re{"^[^\\d]*(\\d+)[^\\d]*$"};
  std::smatch matches;

  std::regex_match(input[0], matches, re);
  size_t a = std::stoul(matches[1].str());
  std::regex_match(input[1], matches, re);
  size_t b = std::stoul(matches[1].str());

  return {a, b};
}

void step(size_t* value, size_t multiplier) {
  *value = (*value * multiplier) % DIVISOR;
}

bool match(const size_t& val_a, const size_t& val_b) {
  return (val_a & 0b1111111111111111) == (val_b & 0b1111111111111111);
}

size_t part_one(const std::vector<std::string>& input) {
  size_t matches{0};
  auto prepared_input = prepare_input(input);
  size_t val_a = std::get<0>(prepared_input);
  size_t val_b = std::get<1>(prepared_input);

  for (size_t round{0}; round < 40000000; round++) {
    step(&val_a, MUL_A);
    step(&val_b, MUL_B);
    matches += match(val_a, val_b);
  }

  return matches;
}

size_t part_two(const std::vector<std::string>& input) {
  return 0;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_15.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

