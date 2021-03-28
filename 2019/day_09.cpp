#include <algorithm>
#include <cassert>
#include <map>

#include "../utils/input.hpp"

std::vector<int64_t> prepare_input(const std::vector<std::string>& input) {
  std::vector<int64_t> intcodes;
  std::transform(
    input.begin(),
    input.end(),
    std::back_inserter(intcodes),
      [](std::string code) -> int64_t {
        return std::strtoll(code.c_str(), NULL, 10);
      });
  return intcodes;
}

struct Instruction {
  int64_t opcode;
  std::vector<int64_t> parameters;
};

class CPU {
 private:
  std::map<size_t, int64_t> memory;
  size_t instruction_pointer;
  size_t relative_base;
  int64_t output;
  bool verbose;

 public:
  explicit CPU(const std::vector<int64_t>& intcodes,
               const bool& verbose = true)
      : instruction_pointer(0),
        relative_base(0),
        output(0),
        verbose(verbose) {
    for (size_t index{0}; index < intcodes.size(); index++) {
      this->get_memory(index) = static_cast<int64_t>(intcodes[index]);
    }
  }

  int64_t& get_parameter(Instruction instruction, const size_t& index) {
    int64_t mode{
    static_cast<int64_t>(instruction.opcode /
    utils::pow(
      static_cast<size_t>(10),
      static_cast<size_t>(2 + index))) % 10};
    switch (mode) {
    case 0:   // position mode
      return this->get_memory(instruction.parameters[index]);
      break;
    case 1:  // immediate mode
      return instruction.parameters[index];
      break;
    case 2:  // relative mode
      return this->get_memory(relative_base +
        instruction.parameters[index]);
      break;
    default:
      throw std::runtime_error(
        "Invalid parameter mode " + std::to_string(mode));
      break;
    }
  }

bool execute(const Instruction& instruction) {
  bool halting{false};
  std::string input{""};
  bool update_instruction_pointer{true};

  switch (instruction.opcode % 100) {
    case 1:
      assert(instruction.parameters.size() == 3);
      this->get_parameter(instruction, 2) =
        get_parameter(instruction, 0) +
        get_parameter(instruction, 1);
      break;
    case 2:
      assert(instruction.parameters.size() == 3);
      this->get_parameter(instruction, 2) =
        get_parameter(instruction, 0) *
        get_parameter(instruction, 1);
      break;
    case 3:
      assert(instruction.parameters.size() == 1);
      std::cout << "Input required:" << std::endl;
      std::cin >> input;
      this->get_parameter(instruction, 0) =
        std::strtoll(input.c_str(), NULL, 10);
      break;
    case 4:
      assert(instruction.parameters.size() == 1);
      output = get_parameter(instruction, 0);
      if (verbose) {
        std::cout << "Output: " << output << std::endl;
      }
      break;
    case 5:
      assert(instruction.parameters.size() == 2);
      if (get_parameter(instruction, 0) != 0) {
        this->instruction_pointer = get_parameter(instruction, 1);
        update_instruction_pointer = false;
      }
      break;
    case 6:
      assert(instruction.parameters.size() == 2);
      if (get_parameter(instruction, 0) == 0) {
        this->instruction_pointer = get_parameter(instruction, 1);
        update_instruction_pointer = false;
      }
      break;
    case 7:
      assert(instruction.parameters.size() == 3);
      this->get_parameter(instruction, 2) = static_cast<int64_t>(
        get_parameter(instruction, 0) <
        get_parameter(instruction, 1));
      break;
    case 8:
      assert(instruction.parameters.size() == 3);
      this->get_parameter(instruction, 2) = static_cast<int64_t>(
        get_parameter(instruction, 0) ==
        get_parameter(instruction, 1));
      break;
    case 9:
      assert(instruction.parameters.size() == 1);
      relative_base += get_parameter(instruction, 0);
      break;
    case 99:
      halting = true;
      break;
    default:
      throw std::runtime_error(
        "Encountered invalid opcode: " +
        std::to_string(instruction.opcode));
      break;
  }

  if (update_instruction_pointer) {
    this->instruction_pointer += 1 + instruction.parameters.size();
  }

  return halting;
}

  bool execute() {
    int64_t opcode = this->get_memory(this->instruction_pointer);
    Instruction instruction;
    instruction.opcode = opcode;
    std::vector<int64_t> parameters;
    switch (opcode % 100) {
      case 1:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 2:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 3:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      case 4:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      case 5:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2) };
        break;
      case 6:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2) };
        break;
      case 7:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 8:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 9:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      default:
        parameters = {};
        break;
    }
    instruction.parameters = parameters;
    return execute(instruction);
  }

  int64_t run() {
    while (!execute()) {}
    return this->get_memory(0);
  }

  int64_t& get_memory(const size_t& index) {
    if (memory.count(index) == 0) {
      this->memory.insert({index, 0});
    }
    return this->memory.at(index);
  }

  int64_t get_output() const {
    return output;
  }
};

int64_t part_one(const std::vector<std::string>& input) {
  auto intcodes = prepare_input(input);
  CPU cpu(intcodes);
  cpu.run();
  return cpu.get_output();
}

int64_t part_two(const std::vector<std::string>& input) {
  auto intcodes = prepare_input(input);
  CPU cpu(intcodes);
  cpu.run();
  return cpu.get_output();
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_09.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

