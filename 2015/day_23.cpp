#include <optional>
#include <map>
#include <regex>

#include "../utils/input.hpp"

enum InstructionType
{
  half,
  triple,
  increment, 
  jump,
  jump_if_even,
  jump_if_one,
};

class Instruction
{
private:
  InstructionType type;
  std::optional<char> memory;
  std::optional<int> data;

public:
  explicit Instruction (const std::string& instruction)
  {
    std::regex re{"^(\\w+)\\s(\\w)?,?\\s?([\\+-]\\d+)?$"};
    std::smatch matches;
    std::regex_match(instruction, matches, re);
    assert (matches.size() == 4);

    if (matches[1].str() == "hlf")
    {
      type = half;
      assert (matches[2].str().size() == 1);
      memory = matches[2].str()[0];
      data = std::nullopt;
    }
    else if (matches[1].str() == "tpl")
    {
      type = triple;
      assert (matches[2].str().size() == 1);
      memory = matches[2].str()[0];
      data = std::nullopt;
    }
    else if (matches[1].str() == "inc")
    {
      type = increment;
      assert (matches[2].str().size() == 1);
      memory = matches[2].str()[0];
      data = std::nullopt;
    }
    else if (matches[1].str() == "jmp")
    {
      type = jump;
      memory = std::nullopt;
      data = std::stoi(matches[3].str());
    }
    else if (matches[1].str() == "jie")
    {
      type = jump_if_even;
      assert (matches[2].str().size() == 1);
      memory = matches[2].str()[0];
      data = std::stoi(matches[3].str());
    }
    else if (matches[1].str() == "jio")
    {
      type = jump_if_one;
      assert (matches[2].str().size() == 1);
      memory = matches[2].str()[0];
      data = std::stoi(matches[3].str());
    }
    else
    {
      throw std::invalid_argument("Invalid instruction: " + instruction);
    }
  }

  InstructionType get_type () const
  {
    return this->type;
  }

  std::optional<char> get_memory () const
  {
    return this->memory;
  }

  std::optional<int> get_data() const
  {
    return this->data;
  }

  friend std::ostream& operator<< (std::ostream& os, const Instruction& instruction)
  {
    switch (instruction.get_type())
    {
      case half:
        os << "hlf " << instruction.get_memory().value();
        break;
      case triple:
        os << "tpl " << instruction.get_memory().value();
        break;
      case increment:
        os << "inc " << instruction.get_memory().value();
        break;
      case jump:
        os << "jump " << instruction.get_data().value();
        break;
      case jump_if_even:
        os << "jie " << instruction.get_memory().value() << ", " << instruction.get_data().value();
        break;
      case jump_if_one:
        os << "jio " << instruction.get_memory().value() << ", " << instruction.get_data().value();
        break;
    }
    return os;
  }
};


class CPU
{
private:
  std::vector<Instruction> instructions;
  size_t instruction_pointer;
  std::map<char, int> memory;

public:
  explicit CPU (const std::vector<std::string>& input)
    : instruction_pointer(0)
  {
    memory = {
      {'a', 0},
      {'b', 0},
    };
    for (auto instruction: input)
    {
      instructions.push_back(Instruction(instruction));
    }
  }

  void run()
  {
    bool running{true};

    while (running)
    {
      running = this->step();
    }
  }

  bool step() 
  {
    return this->step(instructions[instruction_pointer]);
  }

  bool step(const Instruction& instruction)
  {
    switch (instruction.get_type())
    {
      case half:
        assert (instruction.get_memory().has_value());
        this->memory.at(instruction.get_memory().value()) /= 2;
        this->instruction_pointer++;
        break;
      case triple:
        assert (instruction.get_memory().has_value());
        memory.at(instruction.get_memory().value()) *= 3;
        this->instruction_pointer++;
        break;
      case increment:
        assert (instruction.get_memory().has_value());
        this->memory.at(instruction.get_memory().value())++;
        this->instruction_pointer++;
        break;
      case jump:
        assert (instruction.get_data().has_value());
        this->instruction_pointer += instruction.get_data().value();
        break;
      case jump_if_even:
        assert (instruction.get_memory().has_value());
        assert (instruction.get_data().has_value());
        if (memory.at(instruction.get_memory().value()) % 2 == 0)
        {
          this->instruction_pointer += instruction.get_data().value();
        }
        else
        {
          this->instruction_pointer++;
        }
        break;
      case jump_if_one:
        assert (instruction.get_memory().has_value());
        assert (instruction.get_data().has_value());
        if (memory.at(instruction.get_memory().value()) == 1)
        {
          this->instruction_pointer += instruction.get_data().value();
        }
        else
        {
          this->instruction_pointer++;
        }
        break;
    }
    return this->instruction_pointer < this->instructions.size();
  }

  void set_memory(char address, int value)
  {
    this->memory.insert_or_assign(address, value);
  }

  std::map<char, int> get_memory() const
  {
    return this->memory;
  }

  friend std::ostream& operator<< (std::ostream& os, const CPU& cpu)
  {
    os << cpu.instruction_pointer << " -> ";
    if (cpu.instruction_pointer < cpu.instructions.size())
    {
      os << cpu.instructions[cpu.instruction_pointer];
    }
    else
    {
      os << "{invalid instruction}";
    }
    os << std::endl;
    for (auto [memory, value]: cpu.memory)
    {
      os << memory << ": " << value << "\n";
    }
    return os;
  }
};

int part_one(const std::vector<std::string>& input)
{
  CPU cpu(input);
  cpu.run();
  return cpu.get_memory().at('b');
}

int part_two(const std::vector<std::string>& input)
{
  CPU cpu(input);
  cpu.set_memory('a', 1);
  cpu.run();
  return cpu.get_memory().at('b');
}

int main(int argc, char** argv)
{
  utils::Reader reader(std::filesystem::path("../2015/data/input_23.txt"));
  const auto input = reader.get_lines();
 
  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
