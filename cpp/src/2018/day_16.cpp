#include <algorithm>
#include <array>
#include <cassert>
#include <map>
#include <set>
#include <sstream>

#include "../utils/input.hpp"

#ifdef DEBUG
#define DBGVAR(os, var)                                                    \
  (os) << "DBG: " << __FILE__ << "(" << __LINE__ << ") " << #var << " = [" \
       << (var) << "]" << std::endl
#else
#define DBGVAR(os, var) \
  do {                  \
  } while (0)
#endif

enum class Mode : uint { val, reg };

enum class Opcode : uint {
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

std::ostream& operator<<(std::ostream& os, const Opcode& opcode) {
  switch (opcode) {
    case Opcode::addr:
      os << "addr";
      break;
    case Opcode::addi:
      os << "addi";
      break;
    case Opcode::mulr:
      os << "mulr";
      break;
    case Opcode::muli:
      os << "muli";
      break;
    case Opcode::banr:
      os << "banr";
      break;
    case Opcode::bani:
      os << "bani";
      break;
    case Opcode::borr:
      os << "borr";
      break;
    case Opcode::bori:
      os << "bori";
      break;
    case Opcode::setr:
      os << "setr";
      break;
    case Opcode::seti:
      os << "seti";
      break;
    case Opcode::gtir:
      os << "gtir";
      break;
    case Opcode::gtri:
      os << "gtri";
      break;
    case Opcode::gtrr:
      os << "gtrr";
      break;
    case Opcode::eqir:
      os << "eqir";
      break;
    case Opcode::eqri:
      os << "eqri";
      break;
    case Opcode::eqrr:
      os << "eqrr";
      break;
    default:
      throw std::runtime_error("This should never happen.");
      break;
  }

  return os;
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& values) {
  os << "[";
  for (auto it{values.begin()}; it != values.end(); ++it) {
    os << *it;
    if (std::next(it) != values.end()) {
      os << ", ";
    } else {
      os << "]";
    }
  }

  return os;
}

static const std::array<Opcode, 16> ALL_OPCODES = {
    Opcode::addr, Opcode::addi, Opcode::mulr, Opcode::muli,
    Opcode::banr, Opcode::bani, Opcode::borr, Opcode::bori,
    Opcode::setr, Opcode::seti, Opcode::eqir, Opcode::eqri,
    Opcode::eqrr, Opcode::gtir, Opcode::gtri, Opcode::gtrr};

std::pair<Mode, Mode> get_modes(Opcode opcode) {
  switch (opcode) {
    case Opcode::addr:
      return {Mode::reg, Mode::reg};
      break;
    case Opcode::addi:
      return {Mode::reg, Mode::val};
      break;
    case Opcode::mulr:
      return {Mode::reg, Mode::reg};
      break;
    case Opcode::muli:
      return {Mode::reg, Mode::val};
      break;
    case Opcode::banr:
      return {Mode::reg, Mode::reg};
      break;
    case Opcode::bani:
      return {Mode::reg, Mode::val};
      break;
    case Opcode::borr:
      return {Mode::reg, Mode::reg};
      break;
    case Opcode::bori:
      return {Mode::reg, Mode::val};
      break;
    case Opcode::setr:
      return {Mode::reg, Mode::reg};
      break;
    case Opcode::seti:
      return {Mode::val, Mode::val};
      break;
    case Opcode::gtir:
      return {Mode::val, Mode::reg};
      break;
    case Opcode::gtri:
      return {Mode::reg, Mode::val};
      break;
    case Opcode::gtrr:
      return {Mode::reg, Mode::reg};
      break;
    case Opcode::eqir:
      return {Mode::val, Mode::reg};
      break;
    case Opcode::eqri:
      return {Mode::reg, Mode::val};
      break;
    case Opcode::eqrr:
      return {Mode::reg, Mode::reg};
      break;
    default:
      throw std::runtime_error("This should never happen");
      break;
  }
}

std::pair<uint, uint> get_values(const Opcode& opcode,
                                 const std::vector<uint>& instruction,
                                 std::vector<uint>* registers) {
  auto modes = get_modes(opcode);
  std::pair<uint, uint> values;
  values.first = modes.first == Mode::reg
                     ? registers->operator[](instruction[1])
                     : instruction[1];
  values.second = modes.second == Mode::reg
                      ? registers->operator[](instruction[2])
                      : instruction[2];

  return values;
}

struct Example {
  std::vector<uint> registers_before;
  std::vector<uint> instruction;
  std::vector<uint> registers_after;

  friend std::ostream& operator<<(std::ostream& os, const Example& example) {
    os << "Before: [";
    for (auto it = example.registers_before.begin();
         it != example.registers_before.end(); it++) {
      os << *it;
      if (std::next(it) != example.registers_before.end()) {
        os << ", ";
      } else {
        os << "]\n";
      }
    }

    for (auto it = example.instruction.begin(); it != example.instruction.end();
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
         it != example.registers_after.end(); it++) {
      os << *it;
      if (std::next(it) != example.registers_after.end()) {
        os << ", ";
      } else {
        os << "]";
      }
    }

    return os;
  }
};

void run_opcode(const Opcode opcode, const std::vector<uint>& instruction,
                std::vector<uint>* registers) {
  auto c = instruction[3];
  auto [val_a, val_b] = get_values(opcode, instruction, registers);
  DBGVAR(std::cerr, opcode);
  DBGVAR(std::cerr, instruction);
  DBGVAR(std::cerr, *registers);
  DBGVAR(std::cerr, val_a);
  DBGVAR(std::cerr, val_b);
  switch (opcode) {
    case Opcode::addr:
      // addr (add register) stores into register C the result of adding
      // register A and register B.
      registers->operator[](c) = val_a + val_b;
      break;
    case Opcode::addi:
      // addi (add immediate) stores into register C the result of adding
      // register A and value B.
      registers->operator[](c) = val_a + val_b;
      break;
    case Opcode::mulr:
      // mulr (multiply register) stores into register C the result of
      // multiplying register A and register B.
      registers->operator[](c) = val_a * val_b;
      break;
    case Opcode::muli:
      // muli (multiply immediate) stores into register C the result of
      // multiplying register A and value B.
      registers->operator[](c) = val_a * val_b;
      break;
    case Opcode::banr:
      // banr (bitwise AND register) stores into register C the result of the
      // bitwise AND of register A and register B.
      registers->operator[](c) = val_a & val_b;
      break;
    case Opcode::bani:
      // bani (bitwise AND immediate) stores into register C the result of the
      // bitwise AND of register A and value B.
      registers->operator[](c) = val_a & val_b;
      break;
    case Opcode::borr:
      // borr (bitwise OR register) stores into register C the result of the
      // bitwise OR of register A and register B.
      registers->operator[](c) = val_a | val_b;
      break;
    case Opcode::bori:
      // bori (bitwise OR immediate) stores into register C the result of the
      // bitwise OR of register A and value B.
      registers->operator[](c) = val_a | val_b;
      break;
    case Opcode::setr:
      // setr (set register) copies the contents of register A into register C.
      // (Input B is ignored.)
      registers->operator[](c) = val_a;
      break;
    case Opcode::seti:
      // seti (set immediate) stores value A into register C. (Input B is
      // ignored.)
      registers->operator[](c) = val_a;
      break;
    case Opcode::gtir:
      // gtir (greater-than immediate/register) sets register C to 1 if value A
      // is greater than register B. Otherwise, register C is set to 0.
      registers->operator[](c) = static_cast<uint>(val_a > val_b);
      break;
    case Opcode::gtri:
      // gtri (greater-than register/immediate) sets register C to 1 if register
      // A is greater than value B. Otherwise, register C is set to 0.
      registers->operator[](c) = static_cast<uint>(val_a > val_b);
      break;
    case Opcode::gtrr:
      registers->operator[](c) = static_cast<uint>(val_a > val_b);
      break;
    case Opcode::eqir:
      // eqir (equal immediate/register) sets register C to 1 if value A is
      // equal to register B. Otherwise, register C is set to 0.
      registers->operator[](c) = static_cast<uint>(val_a == val_b);
      break;
    case Opcode::eqri:
      // eqri (equal register/immediate) sets register C to 1 if register A is
      // equal to value B. Otherwise, register C is set to 0.
      registers->operator[](c) = static_cast<uint>(val_a == val_b);
      break;
    case Opcode::eqrr:
      // eqrr (equal register/register) sets register C to 1 if register A is
      // equal to register B. Otherwise, register C is set to 0.
      registers->operator[](c) = static_cast<uint>(val_a == val_b);
      break;
    default:
      std::stringstream ss;
      ss << "Encountered unknown opcode: " << opcode;
      throw std::runtime_error(ss.str());
      break;
  }
  DBGVAR(std::cerr, *registers);
}

std::vector<Opcode> matching_opcodes(const Example& example) {
  std::vector<Opcode> matches;

  for (auto opcode : ALL_OPCODES) {
    auto registers = example.registers_before;
    run_opcode(opcode, example.instruction, &registers);
    if (registers == example.registers_after) {
      matches.push_back(opcode);
    }
  }

  return matches;
}

std::vector<uint> extract_registers(std::string input) {
  utils::replace_all_substrings(&input, ", ", ",");
  utils::replace_all_substrings(&input, "]", "");
  auto input_vec = utils::split_string(input, ',');
  assert(input_vec.size() == 4);

  std::vector<uint> result;
  for (size_t index{0}; index < 4; index++) {
    result.push_back(std::stoi(input_vec[index]));
  }

  return result;
}

std::vector<Example> extract_examples(const std::vector<std::string>& input) {
  std::vector<Example> examples;
  for (auto it = input.begin();; it++) {
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

std::vector<std::vector<uint>> extract_instructions(
    const std::vector<std::string>& input) {
  std::vector<std::vector<uint>> instructions;
  for (auto it = input.rbegin();; it++) {
    auto splits = utils::split_string(*it, ' ');
    if (splits.size() == 0) {
      break;
    }
    std::vector<uint> instruction;
    std::transform(splits.begin(), splits.end(),
                   std::back_inserter(instruction),
                   [](std::string number) { return std::stoul(number); });
    instructions.push_back(instruction);
  }
  std::reverse(instructions.begin(), instructions.end());

  return instructions;
}

std::map<uint, Opcode> find_opcodes(const std::vector<Example>& examples) {
  std::map<uint, std::set<Opcode>> opcode_candidates;
  std::set<Opcode> opcode_set;
  for (auto opcode : ALL_OPCODES) {
    opcode_set.insert(opcode);
  }
  for (uint code{0}; code < 16; ++code) {
    opcode_candidates.insert({code, opcode_set});
  }

  for (auto example : examples) {
    auto code = example.instruction[0];
    auto matches = matching_opcodes(example);
    for (auto it{opcode_candidates.at(code).begin()};
         it != opcode_candidates.at(code).end();) {
      if (std::find(matches.begin(), matches.end(), *it) != matches.end()) {
        ++it;
      } else {
        it = opcode_candidates.at(code).erase(it);
      }
    }
  }

  bool searching{true};
  while (searching) {
    searching = false;
    for (uint ref{0}; ref < 16; ++ref) {
      if (opcode_candidates.at(ref).size() > 1) {
        continue;
      } else if (opcode_candidates.at(ref).size() == 1) {
        auto opcode = *opcode_candidates.at(ref).begin();
        for (uint code{0}; code < 16; ++code) {
          if (ref == code) {
            continue;
          }
          if (opcode_candidates.at(code).count(opcode) > 0) {
            opcode_candidates.at(code).erase(opcode);
            searching = true;
          }
        }
      } else {
        throw std::runtime_error("Yikes!");
      }
    }
  }

  std::map<uint, Opcode> lookup;

  for (auto [code, candidates] : opcode_candidates) {
    assert(candidates.size() == 1);
    lookup.insert({code, *candidates.begin()});
  }

  return lookup;
}

auto part_one(const std::vector<std::string>& input) {
  auto examples = extract_examples(input);
  size_t result{0};
  for (auto example : examples) {
    DBGVAR(std::cerr, example);
    auto opcodes = matching_opcodes(example);
    DBGVAR(std::cerr, opcodes);
    if (opcodes.size() >= 3) {
      result++;
    }
  }
  return result;
}

auto part_two(const std::vector<std::string>& input) {
  auto examples = extract_examples(input);
  auto instructions = extract_instructions(input);
  auto opcode_lookup = find_opcodes(examples);
  std::vector<uint> registers{{0, 0, 0, 0}};

  for (auto instruction : instructions) {
    run_opcode(opcode_lookup.at(instruction[0]), instruction, &registers);
  }
  return registers[0];
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2018/input_16.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

