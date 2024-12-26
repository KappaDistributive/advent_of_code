#include "../utils/input.hpp"
#include <bitset>

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
  assert(operation.size() + 2 == numbers.size());
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

// get all combinations
// 0        0 0
// 1        0 0
// ...
// max_size 0 0
// 0        1 0
// 1        1 0
// 2        1 0
// ...
std::vector<std::vector<int>> get_all_combinations(size_t size, int max_size) {
  std::vector<std::vector<int>> result;
  std::vector<int> sequence(size, 0);
  result.push_back(sequence);
  while (!std::all_of(sequence.begin(), sequence.end(),
                      [max_size](int i) { return i == max_size; })) {
    for (size_t index{0}; index < sequence.size(); ++index) {
      if (sequence[index] < max_size) {
        sequence[index] += 1;
        break;
      } else {
        sequence[index] = 0;
      }
    }
    result.push_back(sequence);
  }
  return result;
}

std::vector<std::vector<Operator>> get_operations(size_t size,
                                                  bool concat = false) {
  auto combincations = get_all_combinations(size, concat ? 2 : 1);
  std::vector<std::vector<Operator>> result;
  for (const auto &comb : combincations) {
    std::vector<Operator> temp;
    for (const auto &c : comb) {
      switch (c) {
      case 0:
        temp.push_back(Operator::ADD);
        break;
      case 1:
        temp.push_back(Operator::MUL);
        break;
      case 2:
        temp.push_back(Operator::CONCAT);
        break;
      default:
        assert(false);
      }
    }
    result.push_back(temp);
  }
  return result;
}

auto part_one(const std::vector<std::vector<int64_t>> &input) {
  int64_t result{0};
  for (const auto &numbers : input) {
    auto ops = get_operations(numbers.size() - 2);
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
  for (const auto &numbers : input) {
    auto ops = get_operations(numbers.size() - 2, true);
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
