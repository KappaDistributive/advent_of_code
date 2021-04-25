#include <array>
#include <cassert>

#include "../utils/input.hpp"

// 4 registers
//   starting with value 0
// 16 opcodes
// instruction consists of 4 values
//   opcode
//   first input   [A]
//   second input  [B]
//   output        [C]  <- always a register

enum class Mode : int {
  val,
  reg
};

enum class Opcode : int {
  addr,
  addi,
  mulr,
  muli,
  banr,
  bani,
  borr,
  bori,
  setr,
  seti,
  gtir,
  gtri,
  gtrr,
  eqir,
  eqri,
  eqrr
};

std::pair<Mode, Mode> get_modes(Opcode opcode) {
  switch (opcode) {
    case Opcode::addr: return {Mode::reg, Mode::reg}; break;
    case Opcode::addi: return {Mode::reg, Mode::val}; break;
    case Opcode::mulr: return {Mode::reg, Mode::reg}; break;
    case Opcode::muli: return {Mode::reg, Mode::val}; break;
    case Opcode::banr: return {Mode::reg, Mode::reg}; break;
    case Opcode::bani: return {Mode::reg, Mode::val}; break;
    case Opcode::borr: return {Mode::reg, Mode::reg}; break;
    case Opcode::bori: return {Mode::reg, Mode::val}; break;
    case Opcode::setr: return {Mode::reg, Mode::reg}; break;
    case Opcode::seti: return {Mode::reg, Mode::val}; break;
    case Opcode::gtir: return {Mode::val, Mode::reg}; break;
    case Opcode::gtri: return {Mode::reg, Mode::val}; break;
    case Opcode::gtrr: return {Mode::reg, Mode::reg}; break;
    case Opcode::eqir: return {Mode::val, Mode::reg}; break;
    case Opcode::eqri: return {Mode::reg, Mode::val}; break;
    case Opcode::eqrr: return {Mode::reg, Mode::reg}; break;
    default: throw std::runtime_error("This should never happen"); break;
  }
}

struct Example {
  std::array<int, 4> registers_before;
  std::array<int, 4> instruction;
  std::array<int, 4> registers_after;

  friend std::ostream&
  operator<<(std::ostream& os, const Example& example) {
    os << "Before: [";
    for (auto it = example.registers_before.begin();
         it != example.registers_before.end();
         it++) {
      os << *it;
      if (std::next(it) != example.registers_before.end()) {
        os << ", ";
      } else {
        os << "]\n";
      }
    }

    for (auto it = example.instruction.begin();
         it != example.instruction.end();
         it++) {
      os << *it;
      if (std::next(it) != example.instruction.end()) {
        os << " ";
      } else {
        os << "\n";
      }
    }

    os << "After: [";
    for (auto it = example.registers_after.begin();
         it != example.registers_after.end();
         it++) {
      os << *it;
      if (std::next(it) != example.registers_after.end()) {
        os << ", ";
      } else {
        os << "]\n";
      }
    }

    return os;
  }
};


std::array<int, 4> extract_registers(std::string input) {
  utils::replace_all_substrings(&input, ", ", ",");
  utils::replace_all_substrings(&input, "]", "");
  auto input_vec = utils::split_string(input, ',');
  assert(input_vec.size() == 4);

  std::array<int, 4> result;
  for (size_t index{0}; index < 4; index++) {
    result[index] = std::stoi(input_vec[index]);
  }

  return result;
}


std::vector<Example>
extract_examples(const std::vector<std::string>& input) {
  std::vector<Example> examples;
  for (auto it = input.begin(); ; it++) {
    if (it->substr(0, 7) != "Before:") {
      break;
    }
    std::string before_str = utils::split_string(*it, '[')[1];
    it++;
    std::string instruction_str = *it;
    utils::replace_all_substrings(&instruction_str, " ", ",");
    it++;
    std::string after_str = utils::split_string(*it, '[')[1];
    it++;
    examples.push_back(Example{
      extract_registers(before_str),
      extract_registers(instruction_str),
      extract_registers(after_str),
    });
  }

  return examples;
}

int part_one(const std::vector<std::string>& input) {
  auto examples = extract_examples(input);
  for (auto example : examples) {
    std::cout << example << std::endl;
  }
  return 0;
}


int part_two(const std::vector<std::string>& input) {
  return -1;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_16.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
