#include <algorithm>
#include <cassert>
#include <map>
#include <optional>
#include <variant>

#include "../utils/input.hpp"


int64_t
modulo(int64_t a, int64_t b) {
  if (a >= 0) {
    return a % b;
  } else {
    auto div = a % b;
    if (div < 0) {
      div = -div;
    }
    return (b - div) % b;
  }
}


enum class Op : int {
  snd,
  set,
  add,
  mul,
  mod,
  rcv,
  jgz,
};


std::ostream&
operator<<(std::ostream& os, const Op& op) {
  switch (op) {
    case Op::snd: os << "snd"; break;
    case Op::set: os << "set"; break;
    case Op::add: os << "add"; break;
    case Op::mul: os << "mul"; break;
    case Op::mod: os << "mod"; break;
    case Op::rcv: os << "rcv"; break;
    case Op::jgz: os << "jgz"; break;
    default:
      throw std::runtime_error("This should never happen!");
      break;
  }

  return os;
}


struct Instruction {
  Op op;
  std::vector<std::variant<char, int64_t>> parameters;
};

std::ostream&
operator<<(std::ostream& os, const Instruction& instruction) {
  os << instruction.op << " ";
  for (auto it{instruction.parameters.begin()}; it != instruction.parameters.end(); ++it) {
    if (std::holds_alternative<char>(*it)) {
      os << std::get<char>(*it);
    } else {
      assert(std::holds_alternative<int64_t>(*it));
      os << std::get<int64_t>(*it);
    }
    if (std::next(it) != instruction.parameters.end()) {
      os << " ";
    }
  }

  return os;
}

Op prepare_op(const std::string& line) {
  if (line == "snd") {
    return Op::snd;
  } else if (line == "set") {
    return Op::set;
  } else if (line == "add") {
    return Op::add;
  } else if (line == "mul") {
    return Op::mul;
  } else if (line == "mod") {
    return Op::mod;
  } else if (line == "rcv") {
    return Op::rcv;
  } else if (line == "jgz") {
    return Op::jgz;
  } else {
    throw std::invalid_argument("Invalid op name: " + line);
  }
}


Instruction
prepare_instruction(const std::string& line) {
  auto splits = utils::split_string(line, ' ');
  auto op = prepare_op(splits[0]);
  std::vector<std::variant<char, int64_t>> parameters;
  for (size_t index{1}; index < splits.size(); ++index) {
    auto split = splits[index];
    if (std::all_of(split.begin(), split.end(), [] (char c) { return c == '-' || isdigit(c); })) {
        parameters.push_back(std::stoi(split));
    } else {
      assert(split.size() == 1);
      parameters.push_back(static_cast<char>(split[0]));
    }
  }
  return Instruction{
    op,
    parameters
  };
}


std::vector<Instruction>
prepare_instructions(const std::vector<std::string>& input) {
  std::vector<Instruction> instructions;

  for (auto line: input) {
    instructions.push_back(prepare_instruction(line));
  }

  return instructions;
}


class CPU {
 private:
  std::vector<Instruction> m_instructions;
  size_t m_instruction_pointer;
  std::map<char, int64_t> m_registers;
  std::optional<int64_t> m_sound;

  int64_t
  get_value(const std::variant<char, int64_t>& parameter) const {
    if (std::holds_alternative<char>(parameter)) {
        return (m_registers.count(std::get<char>(parameter)) > 0) ? m_registers.at(std::get<char>(parameter)) : 0;
    } else {
      assert(std::holds_alternative<int64_t>(parameter));
      return std::get<int64_t>(parameter);
    }

    throw std::runtime_error("Failed to obtain value!");
  }

  void set_value(char address, int64_t value) {
    m_registers.emplace(address, 0);
    m_registers[address] = value;
  }

  std::optional<int64_t>
  execute(const Instruction& instruction) {
    auto op = instruction.op;

    switch (op) {
      case Op::snd: // snd X plays a sound with a frequency equal to the value of X.
        assert(instruction.parameters.size() == 1);
        m_sound = get_value(instruction.parameters[0]);
        m_instruction_pointer++;
        break;
      case Op::set: // set X Y sets register X to the value of Y.
        assert(instruction.parameters.size() == 2);
        assert(std::holds_alternative<char>(instruction.parameters[0]));
        set_value(std::get<char>(instruction.parameters[0]), get_value(instruction.parameters[1]));
        m_instruction_pointer++;
        break;
      case Op::add: // add X Y increases register X by the value of Y.
        assert(instruction.parameters.size() == 2);
        set_value(std::get<char>(instruction.parameters[0]), get_value(instruction.parameters[0]) + get_value(instruction.parameters[1]));
        m_instruction_pointer++;
        break;
      case Op::mul: // mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
        assert(instruction.parameters.size() == 2);
        set_value(std::get<char>(instruction.parameters[0]), get_value(instruction.parameters[0]) * get_value(instruction.parameters[1]));
        m_instruction_pointer++;
        break;
      case Op::mod: // mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
        assert(instruction.parameters.size() == 2);
        set_value(std::get<char>(instruction.parameters[0]), modulo(get_value(instruction.parameters[0]), get_value(instruction.parameters[1])));
        m_instruction_pointer++;
        break;
      case Op::rcv: // rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
        assert(instruction.parameters.size() == 1);
        std::cout << m_sound.value() << std::endl;
        if (get_value(instruction.parameters[0]) != 0) {
          return m_sound;
        }
        m_instruction_pointer++;
        break;
      case Op::jgz: // jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
        assert(instruction.parameters.size() == 2);
        if (get_value(instruction.parameters[0]) > 0) {
          assert(get_value(instruction.parameters[1]) + static_cast<int64_t>(m_instruction_pointer) >= 0);
          m_instruction_pointer += get_value(instruction.parameters[1]);
        } else {
          m_instruction_pointer++;
        }
        break;
    }
    return std::nullopt;
  }

 public:
  explicit CPU(const std::vector<Instruction> instructions) :
    m_instructions(instructions),
    m_instruction_pointer(0),
    m_sound(std::nullopt) {
  }

  std::optional<int64_t>
  step() {
    std::cout << m_instructions[m_instruction_pointer] << std::endl;
    return execute(m_instructions[m_instruction_pointer]);
  }
};


auto
part_one(const std::vector<std::string>& input) {
  auto instructions = prepare_instructions(input);
  CPU cpu(instructions);

  std::optional<int64_t> frequency{std::nullopt};
  do {
    frequency = cpu.step();
  } while (!frequency.has_value() || frequency.value() == 0);

  return frequency.value();
}


// auto
// part_two(size_t  number_of_steps) {
//   // takes ~10 minutes with release binaries
//   number_of_steps = 30;
//   return run(number_of_steps, 50000000, 0);
// }


int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_18.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two =  part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
