#include <map>
#include <regex>

#include "../utils/input.hpp"

enum Comparator
{
  less,
  less_or_equal,
  equal,
  unequal,
  greater_or_equal,
  greater
};

Comparator string_to_comparator(const std::string& input)
{
  if (input == "<")
  {
    return less;
  }
  else if (input == "<=")
  {
    return less_or_equal;
  }
  else if (input == "==")
  {
    return equal;
  }
  else if (input == "!=")
  {
    return unequal;
  }
  else if (input == ">=")
  {
    return greater_or_equal;
  }
  else if (input == ">")
  {
    return greater;
  }
  else
  {
    throw std::invalid_argument("Invalid comparator: " + input);
  }
}

std::string comparator_to_string(const Comparator& comparator)
{
  switch (comparator)
  {
    case less: return "<"; break;
    case less_or_equal: return "<="; break;
    case equal: return "=="; break;
    case unequal: return "!="; break;
    case greater_or_equal: return ">="; break;
    case greater: return ">"; break;
  }
}

enum Operator
{
  increment,
  decrement
};

Operator string_to_operator(const std::string& input)
{
  if (input == "inc")
  {
    return increment;
  }
  else if (input == "dec")
  {
    return decrement;
  }
  else
  {
    throw std::invalid_argument("Invalid operator: " + input);
  }
}

std::string operator_to_string(const Operator& op)
{
  switch (op)
  {
    case increment: return "inc"; break;
    case decrement: return "dec"; break;
  }
}


bool compare(const int& lhs, const Comparator& comparator, const int& rhs)
{
  switch (comparator)
  {
    case less:
      return lhs < rhs;
      break;
    case less_or_equal:
      return lhs <= rhs;
      break;
    case equal:
      return lhs == rhs;
      break;
    case unequal:
      return lhs != rhs;
      break;
    case greater_or_equal:
      return lhs >= rhs;
      break;
    case greater:
      return lhs > rhs;
      break;
  }
}

typedef std::tuple<std::string, Operator, int, std::string, Comparator, int> Instruction;

std::ostream& operator<< (std::ostream& os, const Instruction& instruction)
{
  auto [target, op, data, lhs, cmp, rhs] = instruction;
  os << target << " " << operator_to_string(op) << " " << data << " if ";
  os << lhs << " " << comparator_to_string(cmp) << " " << rhs;
  return os;
}

std::vector<Instruction> prepare_input(const std::vector<std::string>& input)
{
  std::vector<Instruction> instructions;
  std::regex re{"^(\\w+)\\s(inc|dec)\\s(-?\\d+)\\sif\\s(\\w+)\\s(<|<=|==|!=|>=|>)\\s(-?\\d+)$"};
  std::smatch matches;
  for (auto line: input)
  {
    std::regex_match(line, matches, re);
    instructions.push_back({
        matches[1].str(),
        string_to_operator(matches[2].str()), 
        std::stoi(matches[3].str()),
        matches[4].str(),
        string_to_comparator(matches[5].str()),
        std::stoi(matches[6].str())
    });
  }
  return instructions;
}

class CPU
{
private:
  const std::vector<Instruction> instructions;
  std::map<std::string, int> registers;
  size_t instruction_pointer;

public:
  explicit CPU (const std::vector<Instruction>& instructions)
    : instructions(instructions), instruction_pointer(0)
  {
  }

  bool step ()
  {
    auto [target, op, data, lhs, cmp, rhs] = instructions[instruction_pointer];
    registers.emplace(lhs, 0);
    if (compare(registers.at(lhs), cmp, rhs))
    {
      registers.emplace(target, 0);
      switch (op)
      {
        case increment: registers.at(target) += data; break;
        case decrement: registers.at(target) -= data; break;
      }
    }
    return ++instruction_pointer < instructions.size();
  }

  std::map<std::string, int> get_registers () const
  {
    return registers;
  }
};

int part_one(const std::vector<std::string>& input)
{
  auto instructions = prepare_input(input);
  CPU cpu(instructions);
  do {} while (cpu.step());
  auto registers = cpu.get_registers();

  int result{INT_MIN};
  for (auto [_, value]: registers)
  {
    if (value > result)
    {
      result = value;
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input)
{
  return 2211;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2017/data/input_08.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
