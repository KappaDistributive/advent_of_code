#include "../utils/combinatorics.hpp"
#include "../utils/input.hpp"

std::vector<std::vector<int64_t>> parse(const std::vector<std::string> &input) {
  std::vector<std::vector<int64_t>> result;
  for (const auto &line : input) {
    std::vector<int64_t> temp;
    auto parts = utils::split_string(line, ' ');
    for (auto part : parts) {
      if (part.size() == 0) {
        continue;
      }
      if (part[part.size() - 1] == ':') {
        part = part.substr(0, part.size() - 1);
      }
      temp.push_back(std::stol(part));
    }
    result.push_back(temp);
  }
  return result;
}

// Enum class for the different types of instructions
enum class Operator { ADD, MUL, CONCAT };

std::ostream &operator<<(std::ostream &os, const Operator &op) {
  switch (op) {
  case Operator::ADD:
    os << "+";
    break;
  case Operator::MUL:
    os << "*";
    break;
  case Operator::CONCAT:
    os << "*";
    break;
  default:
    assert(false);
  }
  return os;
}

bool is_match(const std::vector<std::int64_t> &numbers,
              const std::vector<Operator> &operations) {
  assert(operations.size() + 2 == numbers.size());
  int64_t result{numbers[1]};
  for (size_t index{0}; index < operations.size(); ++index) {
    switch (operations[index]) {
    case Operator::ADD:
      result += numbers[index + 2];
      break;
    case Operator::MUL:
      result *= numbers[index + 2];
      break;
    case Operator::CONCAT:
      result = std::stoll(std::to_string(result) +
                          std::to_string(numbers[index + 2]));
      break;
    default:
      assert(false);
    }
  }
  return numbers[0] == result;
}

auto part_one(const std::vector<std::vector<int64_t>> &input) {
  int64_t result{0};
  std::vector<Operator> allowed_ops{{Operator::ADD, Operator::MUL}};
  for (const auto &numbers : input) {
    auto ops =
        utils::combinatorics::all_combinations(numbers.size() - 2, allowed_ops);
    for (const auto &op : ops) {
      if (is_match(numbers, op)) {
        result += numbers[0];
        break;
      }
    }
  }
  return result;
}

auto part_two(const std::vector<std::vector<int64_t>> &input) {
  int64_t result{0};
  std::vector<Operator> allowed_ops{
      {Operator::ADD, Operator::MUL, Operator::CONCAT}};
  for (const auto &numbers : input) {
    auto ops =
        utils::combinatorics::all_combinations(numbers.size() - 2, allowed_ops);
    for (const auto &op : ops) {
      if (is_match(numbers, op)) {
        result += numbers[0];
        break;
      }
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_07_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_07.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
