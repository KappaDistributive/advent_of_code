#include <cassert>
#include <regex>

#include "../utils/input.hpp"

enum Operation { nop, acc, jmp };

class Instruction 
{
private:
  Operation operation;
  int data;


public:
  explicit Instruction(const std::string& instruction)
  {
    std::regex re{"^(\\w+)\\s((?:\\+|\\-)\\d+)$"};
    std::smatch matches;

    std::regex_match(instruction, matches, re);
    assert(matches.size() == 3);

    if (matches[1].str() == "nop")
    {
      operation = nop;
    }
    else if (matches[1].str() == "acc")
    {
      operation = acc;
    }
    else if (matches[1].str() == "jmp")
    {
      operation = jmp;
    }
    else
    {
      throw std::invalid_argument("Invalid instruction: " + instruction);
    }

    data = std::stoi(matches[2].str());
  }

  Operation get_operation() const {
    return operation;
  }

  void set_operation(Operation operation) {
    this->operation = operation;
  }

  int get_data() const {
    return data;
  }
};

std::ostream& operator<< (std::ostream& os, const Instruction& instruction)
{
  switch (instruction.get_operation())
  {
    case acc: os << "acc "; break;
    case jmp: os << "jmp "; break;
    case nop: os << "nop "; break;
  }
  os << instruction.get_data();

  return os;
}

class CPU
{
private:
  int accumulator;
  size_t program_counter;
  std::vector<Instruction> instructions;

public:
  explicit CPU(const std::vector<std::string>& program) : accumulator(0), program_counter(0)
  {
    for (auto instruction: program)
    {
      instructions.push_back(Instruction(instruction));
    }
  }

  bool step()
  {
    auto instruction = instructions[program_counter];
    switch (instruction.get_operation())
    {
      case acc: accumulator += instruction.get_data(); program_counter++; break;
      case nop: program_counter++; break;
      case jmp: program_counter += instruction.get_data(); break;
    }

    return program_counter < instructions.size();
  }

  size_t get_program_counter() const {
    return program_counter;
  }

  void reset() {
    accumulator = 0;
    program_counter = 0;
  }

  int get_accumulator() const {
    return accumulator;
  }

  bool flip_instruction(size_t instruction_pointer)
  {
    if (instruction_pointer >= instructions.size())
    {
      return false;
    }
    switch (instructions[instruction_pointer].get_operation())
    {
      case acc: return false; break;
      case nop: instructions[instruction_pointer].set_operation(jmp); break;
      case jmp: instructions[instruction_pointer].set_operation(nop); break;
    }

    return true;
  }

};

int part_one(const std::vector<std::string>& input)
{
  int result{0};
  CPU cpu(input);
  std::vector<size_t> visited_instructions;

  while (std::find(visited_instructions.begin(), visited_instructions.end(), cpu.get_program_counter()) == visited_instructions.end())
  {
    visited_instructions.push_back(cpu.get_program_counter());
    result = cpu.get_accumulator();
    cpu.step();
  }

  return result;
}

int part_two(const std::vector<std::string>& input)
{
  int result{0};
  size_t flip_pointer{0};
  CPU cpu = CPU(input);
  bool looping = true;
  bool flipped_instruction = false;

  while (looping)
  {
    flipped_instruction = false;

    // the following is a huge waste of time -- I really should reflip the previously flipped instruction.
    cpu = CPU(input);

    while (!flipped_instruction)
    {
      flipped_instruction = cpu.flip_instruction(flip_pointer++);
    }

    std::vector<size_t> visited_instructions = std::vector<size_t>();
    while (std::find(visited_instructions.begin(), visited_instructions.end(), cpu.get_program_counter()) == visited_instructions.end())
    {
      visited_instructions.push_back(cpu.get_program_counter());
      result = cpu.get_accumulator();
      if (!cpu.step())
      {
        looping = false;
        result = cpu.get_accumulator();
        break;
      }
    }
  }

  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_08.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
