#include <algorithm>
#include <cassert>
#include <map>

#include "../utils/input.hpp"

#define assertm(exp, msg) assert(((void)msg, exp))

std::vector<int64_t> prepare_input(const std::vector<std::string>& input) {
  std::vector<int64_t> intcodes;
  std::transform(input.begin(), input.end(), std::back_inserter(intcodes),
                 [](std::string code) -> int64_t {
                   return std::strtoll(code.c_str(), NULL, 10);
                 });
  return intcodes;
}

struct Instruction {
  int64_t opcode;
  std::vector<int64_t> parameters;
};

enum class Status { ok, input_required, output_produced };

enum class Direction { north, south, west, east };

int64_t decode(Direction direction) {
  int64_t result{-1};

  switch (direction) {
    case Direction::north:
      result = 1;
      break;
    case Direction::south:
      result = 2;
      break;
    case Direction::west:
      result = 3;
      break;
    case Direction::east:
      result = 4;
      break;
  }

  return result;
}

std::ostream& operator<<(std::ostream& os, Status status) {
  switch (status) {
    case Status::ok:
      os << "Status::ok:";
      break;
    case Status::input_required:
      os << "Status::input_required";
      break;
    case Status::output_produced:
      os << "Status::output_produced";
      break;
  }
  return os;
}

class CPU {
 private:
  std::map<size_t, int64_t> m_memory;
  size_t m_instruction_pointer;
  size_t m_relative_base;
  int64_t m_output;
  bool m_verbose;
  bool m_interactive;
  bool m_waiting_for_input;

 public:
  explicit CPU(const std::vector<int64_t>& intcodes, bool verbose = true,
               bool interactive = true)
      : m_instruction_pointer(0),
        m_relative_base(0),
        m_output(0),
        m_verbose(verbose),
        m_interactive(interactive),
        m_waiting_for_input(false) {
    for (size_t index{0}; index < intcodes.size(); index++) {
      this->get_memory(index) = static_cast<int64_t>(intcodes[index]);
    }
  }

  int64_t get_mode(const Instruction& instruction, const size_t& index) {
    return static_cast<int64_t>(instruction.opcode /
                                utils::pow(static_cast<size_t>(10),
                                           static_cast<size_t>(2 + index))) %
           10;
  }

  int64_t& get_parameter(Instruction instruction, const size_t& index) {
    auto mode = get_mode(instruction, index);
    switch (mode) {
      case 0:  // position mode
        return this->get_memory(instruction.parameters[index]);
        break;
      case 1:  // immediate mode
        return instruction.parameters[index];
        break;
      case 2:  // relative mode
        return this->get_memory(this->m_relative_base +
                                instruction.parameters[index]);
        break;
      default:
        throw std::runtime_error("Invalid parameter mode " +
                                 std::to_string(mode));
        break;
    }
  }

  std::pair<bool, Status> execute(const Instruction& instruction) {
    bool halting{false};
    std::string input{""};
    bool update_instruction_pointer{true};
    auto status{Status::ok};
    if (!this->m_waiting_for_input) {
      switch (instruction.opcode % 100) {
        case 1:
          assert(instruction.parameters.size() == 3);
          assertm(get_mode(instruction, 2) != 1,
                  "Parameters that an instruction writes to cannot be in "
                  "immediate mode.");  // NOLINT
          this->get_parameter(instruction, 2) =
              get_parameter(instruction, 0) + get_parameter(instruction, 1);
          break;
        case 2:
          assert(instruction.parameters.size() == 3);
          assertm(get_mode(instruction, 2) != 1,
                  "Parameters that an instruction writes to cannot be in "
                  "immediate mode.");  // NOLINT
          this->get_parameter(instruction, 2) =
              get_parameter(instruction, 0) * get_parameter(instruction, 1);
          break;
        case 3:
          assert(instruction.parameters.size() == 1);
          assertm(get_mode(instruction, 2) != 1,
                  "Parameters that an instruction writes to cannot be in "
                  "immediate mode.");  // NOLINT
          assert(get_mode(instruction, 0) != 1);

          status = Status::input_required;
          if (this->m_interactive || this->m_verbose) {
            std::cout << "Input required:" << std::endl;
          }
          if (this->m_interactive) {
            std::cin >> input;
            this->get_parameter(instruction, 0) =
                std::strtoll(input.c_str(), NULL, 10);
            status = Status::ok;
          } else {
            this->m_waiting_for_input = true;
            update_instruction_pointer = false;
          }
          break;
        case 4:
          assert(instruction.parameters.size() == 1);
          status = Status::output_produced;
          this->m_output = get_parameter(instruction, 0);
          if (this->m_verbose) {
            std::cout << "Output: " << this->m_output << std::endl;
          }
          break;
        case 5:
          assert(instruction.parameters.size() == 2);
          if (get_parameter(instruction, 0) != 0) {
            this->m_instruction_pointer = get_parameter(instruction, 1);
            update_instruction_pointer = false;
          }
          break;
        case 6:
          assert(instruction.parameters.size() == 2);
          if (get_parameter(instruction, 0) == 0) {
            this->m_instruction_pointer = get_parameter(instruction, 1);
            update_instruction_pointer = false;
          }
          break;
        case 7:
          assert(instruction.parameters.size() == 3);
          assertm(get_mode(instruction, 2) != 1,
                  "Parameters that an instruction writes to cannot be in "
                  "immediate mode.");  // NOLINT
          assert(get_mode(instruction, 2) != 1);
          this->get_parameter(instruction, 2) = static_cast<int64_t>(
              get_parameter(instruction, 0) < get_parameter(instruction, 1));
          break;
        case 8:
          assert(instruction.parameters.size() == 3);
          assertm(get_mode(instruction, 2) != 1,
                  "Parameters that an instruction writes to cannot be in "
                  "immediate mode.");  // NOLINT
          this->get_parameter(instruction, 2) = static_cast<int64_t>(
              get_parameter(instruction, 0) == get_parameter(instruction, 1));
          break;
        case 9:
          assert(instruction.parameters.size() == 1);
          this->m_relative_base += get_parameter(instruction, 0);
          break;
        case 99:
          halting = true;
          break;
        default:
          throw std::runtime_error("Encountered invalid opcode: " +
                                   std::to_string(instruction.opcode));
          break;
      }

      if (update_instruction_pointer) {
        this->m_instruction_pointer += 1 + instruction.parameters.size();
      }
    }

    return std::make_pair(halting, status);
  }

  auto get_current_instruction() {
    int64_t opcode = this->get_memory(this->m_instruction_pointer);
    Instruction instruction;
    instruction.opcode = opcode;
    std::vector<int64_t> parameters;
    switch (opcode % 100) {
      case 1:
        parameters = {this->get_memory(this->m_instruction_pointer + 1),
                      this->get_memory(this->m_instruction_pointer + 2),
                      this->get_memory(this->m_instruction_pointer + 3)};
        break;
      case 2:
        parameters = {this->get_memory(this->m_instruction_pointer + 1),
                      this->get_memory(this->m_instruction_pointer + 2),
                      this->get_memory(this->m_instruction_pointer + 3)};
        break;
      case 3:
        parameters = {this->get_memory(this->m_instruction_pointer + 1)};
        break;
      case 4:
        parameters = {this->get_memory(this->m_instruction_pointer + 1)};
        break;
      case 5:
        parameters = {this->get_memory(this->m_instruction_pointer + 1),
                      this->get_memory(this->m_instruction_pointer + 2)};
        break;
      case 6:
        parameters = {this->get_memory(this->m_instruction_pointer + 1),
                      this->get_memory(this->m_instruction_pointer + 2)};
        break;
      case 7:
        parameters = {this->get_memory(this->m_instruction_pointer + 1),
                      this->get_memory(this->m_instruction_pointer + 2),
                      this->get_memory(this->m_instruction_pointer + 3)};
        break;
      case 8:
        parameters = {this->get_memory(this->m_instruction_pointer + 1),
                      this->get_memory(this->m_instruction_pointer + 2),
                      this->get_memory(this->m_instruction_pointer + 3)};
        break;
      case 9:
        parameters = {this->get_memory(this->m_instruction_pointer + 1)};
        break;
      default:
        parameters = {};
        break;
    }
    instruction.parameters = parameters;

    return instruction;
  }

  std::pair<bool, Status> execute() {
    auto instruction = this->get_current_instruction();
    return execute(instruction);
  }

  int64_t run() {
    auto [halting, status] = execute();
    while ((!halting) && status != Status::input_required) {
      auto update = execute();
      halting = std::get<0>(update);
      status = std::get<1>(update);
    }
    if ((!halting) && status == Status::input_required) {
      std::cout
          << "Stopping because I encountered status `" << status << "`. "
          << "Are you unintentionally running the CPU in non-interactive-mode?"
          << std::endl;
    }
    return this->get_memory(0);
  }

  int64_t& get_memory(const size_t& index) {
    if (this->m_memory.count(index) == 0) {
      this->m_memory.insert({index, 0});
    }
    return this->m_memory.at(index);
  }

  void set_input(int64_t input) {
    if (this->m_verbose) {
      std::cout << "Setting input: " << input << std::endl;
    }

    auto instruction = this->get_current_instruction();
    assert(instruction.parameters.size() == 1);
    assertm(get_mode(instruction, 2) != 1,
            "Parameters that an instruction writes to cannot be in "
            "immediate mode.");  // NOLINT
    assert(get_mode(instruction, 0) != 1);
    this->get_parameter(instruction, 0) = input;
    this->m_instruction_pointer += 1 + instruction.parameters.size();
    this->m_waiting_for_input = false;
  }

  int64_t get_output() const { return this->m_output; }
};

auto step(CPU* cpu, Direction direction) {
  auto state = cpu->execute();
  while ((!std::get<0>(state)) && std::get<1>(state) == Status::ok) {
    state = cpu->execute();
  }
  if ((!std::get<0>(state)) && std::get<1>(state) == Status::input_required) {
    cpu->set_input(decode(direction));
    state = cpu->execute();
  }
  while ((!std::get<0>(state)) && std::get<1>(state) == Status::ok) {
    state = cpu->execute();
  }
  assert(std::get<1>(state) == Status::output_produced);
  return cpu->get_output();
}

int64_t part_one(const std::vector<std::string>& input) {
  auto intcodes = prepare_input(input);
  CPU cpu(intcodes, true, false);
  std::cout << step(&cpu, Direction::north) << std::endl;
  return cpu.get_output();
}

// int64_t part_two(const std::vector<std::string>& input) {
//   auto intcodes = prepare_input(input);
//   CPU cpu(intcodes);
//   std::cout << step(&cpu, Direction::south) << std::endl;
//   return cpu.get_output();
// }

int main() {
  std::filesystem::path input_path{"../2019/data/input_15.txt"};

  utils::Reader reader(input_path);
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two =  part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

