#include "../utils/input.hpp"

enum Operation {
  copy,
  increment,
  decrement,
  jump_not_zero,
  toggle,
};

using Instruction =
    std::tuple<Operation, std::optional<char>, std::optional<int>,
               std::optional<char>, std::optional<int>>;

std::ostream& operator<<(std::ostream& os, Instruction instruction) {
  switch (std::get<0>(instruction)) {
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
    case toggle:
      os << "toggle";
      break;
  }
  if (std::get<1>(instruction).has_value()) {
    os << " " << std::get<1>(instruction).value();
  } else if (std::get<2>(instruction).has_value()) {
    os << " " << std::get<2>(instruction).value();
  }

  if (std::get<3>(instruction).has_value()) {
    os << " " << std::get<3>(instruction).value();
  } else if (std::get<4>(instruction).has_value()) {
    os << " " << std::get<4>(instruction).value();
  }
  return os;
}

class CPU {
 private:
  std::map<char, int> registers;
  int instruction_pointer;
  std::vector<Instruction> instructions;

  Instruction perform_toggle(const Instruction& instruction) {
    Instruction result = instruction;
    switch (std::get<0>(instruction)) {
      case Operation::copy:
        std::get<0>(result) = Operation::jump_not_zero;
        break;
      case Operation::increment:
        std::get<0>(result) = Operation::decrement;
        break;
      case Operation::decrement:
        std::get<0>(result) = Operation::increment;
        break;
      case Operation::jump_not_zero:
        std::get<0>(result) = Operation::copy;
        break;
      case Operation::toggle:
        std::get<0>(result) = Operation::increment;
        break;
    }

    return result;
  }

  bool is_valid(const Instruction& instruction) const {
    switch (std::get<0>(instruction)) {
      case Operation::copy:
        return (std::get<1>(instruction).has_value() ||
                std::get<2>(instruction).has_value()) &&
               (std::get<3>(instruction).has_value() ||
                !std::get<4>(instruction).has_value());
        break;
      case Operation::increment:
        return std::get<1>(instruction).has_value() &&
               !(std::get<2>(instruction).has_value() ||
                 std::get<3>(instruction).has_value() ||
                 std::get<4>(instruction).has_value());
        break;
      case Operation::decrement:
        return std::get<1>(instruction).has_value() &&
               !(std::get<2>(instruction).has_value() ||
                 std::get<3>(instruction).has_value() ||
                 std::get<4>(instruction).has_value());
        break;
      case Operation::jump_not_zero:
        return (std::get<1>(instruction).has_value() ||
                std::get<2>(instruction).has_value()) &&
               (std::get<3>(instruction).has_value() ||
                std::get<4>(instruction).has_value());
        break;
      case Operation::toggle:
        return (std::get<1>(instruction).has_value() ||
                std::get<2>(instruction).has_value()) &&
               !(std::get<3>(instruction).has_value() ||
                 std::get<4>(instruction).has_value());
        break;
    }

    throw std::runtime_error("This should never happen");
  }

 public:
  explicit CPU(const std::vector<Instruction>& instructions)
      : registers({{'a', 0}, {'b', 0}, {'c', 0}, {'d', 0}}),
        instruction_pointer(0),
        instructions(instructions) {}

  bool step() {
    auto instruction = instructions[instruction_pointer];
    if (is_valid(instruction)) {
      switch (std::get<0>(instruction)) {
        int value;
        case copy:
          if (std::get<1>(instruction).has_value()) {
            value = registers.at(std::get<1>(instruction).value());
          } else if (std::get<2>(instruction).has_value()) {
            value = std::get<2>(instruction).value();
          } else {
            instruction_pointer++;
            break;
          }
          if (std::get<3>(instruction).has_value()) {
            registers.insert_or_assign(std::get<3>(instruction).value(), value);
          }
          instruction_pointer++;
          break;
        case increment:
          if (std::get<1>(instruction).has_value()) {
            registers[std::get<1>(instruction).value()]++;
          }
          instruction_pointer++;
          break;
        case decrement:
          if (std::get<1>(instruction).has_value()) {
            registers[std::get<1>(instruction).value()]--;
          }
          instruction_pointer++;
          break;
        case jump_not_zero:
          if (std::get<1>(instruction).has_value()) {
            value = registers.at(std::get<1>(instruction).value());
          } else if (std::get<2>(instruction).has_value()) {
            value = std::get<2>(instruction).value();
          } else {
            throw std::runtime_error("This should never happen!");
          }
          if (value == 0) {
            instruction_pointer++;
          } else if (std::get<3>(instruction).has_value()) {
            instruction_pointer +=
                registers.at(std::get<3>(instruction).value());
          } else if (std::get<4>(instruction).has_value()) {
            instruction_pointer += std::get<4>(instruction).value();
          } else {
            throw std::runtime_error("This should never happen!");
          }
          break;
        case toggle:
          if (std::get<1>(instruction).has_value()) {
            value = registers.at(std::get<1>(instruction).value());
          } else if (std::get<2>(instruction).has_value()) {
            value = std::get<2>(instruction).value();
          } else {
            instruction_pointer++;
            break;
          }
          if (instruction_pointer >= -value &&
              instruction_pointer + value <
                  static_cast<int>(instructions.size())) {
            int target = instruction_pointer + value;
            instructions[target] = perform_toggle(instructions[target]);
          }
          instruction_pointer++;
          break;
      }
    } else {
      instruction_pointer++;
    }
    return 0 <= instruction_pointer &&
           instruction_pointer < static_cast<int>(instructions.size());
  }

  int read_register(const char& name) const { return registers.at(name); }

  void set_register(const char& name, const int& value) {
    registers.insert_or_assign(name, value);
  }
};

std::vector<Instruction> prepare_input(const std::vector<std::string>& input) {
  std::vector<Instruction> instructions;
  std::vector<std::string> splits;
  for (auto line : input) {
    splits = utils::split_string(line, ' ');
    if (splits[0] == "cpy") {
      assert(splits.size() == 3);
      if (splits[1][0] >= 'a' && splits[1][0] <= 'z') {
        instructions.push_back({copy, splits[1][0], {}, splits[2][0], {}});
      } else {
        instructions.push_back(
            {copy, {}, std::stoi(splits[1]), splits[2][0], {}});
      }
    } else if (splits[0] == "inc") {
      assert(splits.size() == 2);
      instructions.push_back({increment, splits[1][0], {}, {}, {}});
    } else if (splits[0] == "dec") {
      assert(splits.size() == 2);
      instructions.push_back({decrement, splits[1][0], {}, {}, {}});
    } else if (splits[0] == "jnz") {
      assert(splits.size() == 3);
      if (splits[1][0] >= 'a' && splits[1][0] <= 'z') {
        if (splits[2][0] >= 'a' && splits[2][0] <= 'z') {
          instructions.push_back(
              {jump_not_zero, splits[1][0], {}, splits[2][0], {}});
        } else {
          instructions.push_back(
              {jump_not_zero, splits[1][0], {}, {}, std::stoi(splits[2])});
        }
      } else {
        if (splits[2][0] >= 'a' && splits[2][0] <= 'z') {
          instructions.push_back(
              {jump_not_zero, {}, std::stoi(splits[1]), splits[2][0], {}});
        } else {
          instructions.push_back({jump_not_zero,
                                  {},
                                  std::stoi(splits[1]),
                                  {},
                                  std::stoi(splits[2])});
        }
      }
    } else if (splits[0] == "tgl") {
      assert(splits.size() == 2);
      instructions.push_back({toggle, splits[1][0], {}, {}, {}});
    } else {
      throw std::invalid_argument("Invalid instruction: " + line);
    }
  }

  return instructions;
}

auto part_one(const std::vector<std::string>& input) {
  auto instructions = prepare_input(input);
  CPU cpu(instructions);
  cpu.set_register('a', 7);
  while (cpu.step()) {
  }
  return cpu.read_register('a');
}

auto part_two(const std::vector<std::string>& input) {
  auto instructions = prepare_input(input);
  CPU cpu(instructions);
  cpu.set_register('a', 12);
  while (cpu.step()) {
  }
  return cpu.read_register('a');
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_23.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

