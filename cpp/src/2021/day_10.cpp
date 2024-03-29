#include <cassert>
#include <stack>

#include "../utils/input.hpp"

char partner(char paranthesis) {
  switch (paranthesis) {
    case '(':
      return ')';
    case '[':
      return ']';
    case '{':
      return '}';
    case '<':
      return '>';
    case ')':
      return '(';
    case ']':
      return '[';
    case '}':
      return '{';
    case '>':
      return '<';
    default:
      std::stringstream ss;
      ss << "Encountered invalid parenthesis: " << paranthesis;
      throw std::runtime_error(ss.str());
  }
}

size_t penalty(char symbol) {
  switch (symbol) {
    case ')':
      return 3;
    case ']':
      return 57;
    case '}':
      return 1197;
    case '>':
      return 25137;
    default:
      std::stringstream ss;
      ss << "Encountered invalid symbol: " << symbol;
      throw std::runtime_error(ss.str());
  }
}

size_t score(char symbol) {
  switch (symbol) {
    case ')':
      return 1;
    case ']':
      return 2;
    case '}':
      return 3;
    case '>':
      return 4;
    default:
      std::stringstream ss;
      ss << "Encountered invalid symbol: " << symbol;
      throw std::runtime_error(ss.str());
  }
}

auto check(const std::string& line) {
  std::stack<char> stack;
  for (auto symbol : line) {
    switch (symbol) {
      case '(':
        stack.push('(');
        break;
      case '[':
        stack.push('[');
        break;
      case '{':
        stack.push('{');
        break;
      case '<':
        stack.push('<');
        break;
      default:
        assert(symbol == ')' || symbol == ']' || symbol == '}' ||
               symbol == '>');
        if (!stack.empty()) {
          auto top = stack.top();
          if (partner(top) == symbol) {
            stack.pop();
          } else {  // invalid sequence
            return std::make_tuple(top, symbol, stack);
          }
        } else {  // invalid sequence
          return std::make_tuple('1', symbol, stack);
        }
    }
  }
  // stack.empty() ? `valid sequence` : `incomplete sequence`
  return std::make_tuple('0', '0', stack);
}

auto part_one(const std::vector<std::string>& input) {
  size_t result{0};
  for (auto line : input) {
    auto [want, got, stack] = check(line);
    if (want != '1' && want != '0') {
      result += penalty(got);
    }
  }
  return result;
}

auto part_two(const std::vector<std::string>& input) {
  std::vector<size_t> results{};
  size_t result;
  for (auto line : input) {
    result = 0;
    auto [want, got, stack] = check(line);
    if (want == '0') {
      while (!stack.empty()) {
        result *= 5;
        result += score(partner(stack.top()));
        stack.pop();
      }
      results.push_back(result);
    }
  }

  std::sort(results.begin(), results.end());
  assert(results.size() % 2 == 1);

  return results[results.size() / 2];
}

int main() {
  // std::filesystem::path input_path{"../../data/2021/input_10_mock.txt"};
  std::filesystem::path input_path{"../../data/2021/input_10.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
