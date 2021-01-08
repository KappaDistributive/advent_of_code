#include "../utils/input.hpp"

#include <map>
#include <optional>

enum Operation
{
  copy,
  increment,
  decrement,
  jump_not_zero,
};

using Instruction = std::tuple<Operation, std::optional<char>, std::optional<int>, std::optional<char>, std::optional<int>>;

std::ostream& operator<< (std::ostream& os, Instruction instruction)
{
  switch (std::get<0>(instruction))
  {
    case copy:
      os << "cpy";
      break;
    case increment:
      os << "inc";
      break;
    case decrement:
      os << "dec";
      break;
    case jump_not_zero:
      os << "jnz";
      break;
  }
  if (std::get<1>(instruction).has_value())
  {
    os << " " << std::get<1>(instruction).value();
  }
  else if (std::get<2>(instruction).has_value())
  {
    os << " " << std::get<2>(instruction).value();
  }

  if (std::get<3>(instruction).has_value())
  {
    os << " " << std::get<3>(instruction).value();
  }
  else if (std::get<4>(instruction).has_value())
  {
    os << " " << std::get<4>(instruction).value();
  }
  return os;
}

class CPU
{
private:
  std::map<char, int> registers;
  const std::vector<Instruction> instructions;
  int instruction_pointer;

public:
  CPU (const std::vector<Instruction>& instructions)
    : registers({{'a', 0},{'b', 0},{'c', 0},{'d', 0}}), instruction_pointer(0), instructions(instructions)
  {
  }

  bool step()
  {
    auto instruction = instructions[instruction_pointer];
    switch (std::get<0>(instruction))
    {
      int value;
      case copy:
        if (std::get<1>(instruction).has_value())
        {
          value = registers.at(std::get<1>(instruction).value());
        }
        else
        {
          assert (std::get<2>(instruction).has_value());
          value = std::get<2>(instruction).value();
        }
        registers.insert_or_assign(std::get<3>(instruction).value(), value);
        instruction_pointer++;
        break;
      case increment:
        registers[std::get<1>(instruction).value()]++;
        instruction_pointer++;
        break;
      case decrement:
        registers[std::get<1>(instruction).value()]--;
        instruction_pointer++;
        break;
      case jump_not_zero:
        if (std::get<1>(instruction).has_value())
        {
          value = registers.at(std::get<1>(instruction).value());
        }
        else
        {
          assert (std::get<2>(instruction).has_value());
          value = std::get<2>(instruction).value();
        }
        if (value != 0)
        {
          instruction_pointer += std::get<4>(instruction).value();
        }
        else
        {
          instruction_pointer++;
        }
        break;
    }
    return 0 <= instruction_pointer && instruction_pointer < instructions.size();
  }

  int read_register(const char& name) const
  {
    return registers.at(name);
  }
};

std::vector<Instruction> prepare_input (const std::vector<std::string>& input)
{
  std::vector<Instruction> instructions;
  std::vector<std::string> splits;
  for (auto line: input)
  {
    splits = utils::split_string(line, ' ');

    if (splits[0] == "cpy")
    {
      assert (splits.size() == 3);
      if (splits[1][0] >= 'a' && splits[1][0] <= 'z')
      {
        instructions.push_back({copy, splits[1][0], {}, splits[2][0], {}});
      }
      else
      {
        instructions.push_back({copy, {}, std::stoi(splits[1]), splits[2][0], {}});
      }
    }
    else if (splits[0] == "inc")
    {
      assert (splits.size() == 2);
      instructions.push_back({increment, splits[1][0], {}, {}, {}});
    }
    else if (splits[0] == "dec")
    {
      assert (splits.size() == 2);
      instructions.push_back({decrement, splits[1][0], {}, {}, {}});
    }
    else if (splits[0] == "jnz")
    {
      assert (splits.size() == 3);
      if (splits[1][0] >= 'a' && splits[1][0] <= 'z')
      {
        instructions.push_back({jump_not_zero, splits[1][0], {}, {}, std::stoi(splits[2])});
      }
      else
      {
        instructions.push_back({jump_not_zero, {}, std::stoi(splits[1]), {}, std::stoi(splits[2])});
      }
    }
    else
    {
      throw std::invalid_argument("Invalid instruction: " + line);
    }
  }

  return instructions;
}

int part_one(const std::vector<std::string>& input)
{
  auto instructions = prepare_input(input);
  CPU cpu(instructions);
  while (cpu.step())
  {
  }
  return cpu.read_register('a');
}

int part_two(const std::vector<std::string>& input)
{
  return 2222;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_12.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
