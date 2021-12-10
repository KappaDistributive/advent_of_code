#include <algorithm>
#include <array>
#include <deque>
#include <optional>
#include <cassert>

#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input) {
    std::vector<int> intcodes;
    std::transform(
      input.begin(),
      input.end(),
      std::back_inserter(intcodes),
        [](std::string code) -> int { return std::stoi(code); });
    return intcodes;
}

struct Instruction {
    int opcode;
    std::vector<int> parameters;
};

class CPU {
 private:
    std::vector<int> memory;
    size_t instruction_pointer;
    std::deque<int>* input_tape;
    const bool use_input_tape;
    bool verbose;
    bool is_waiting_for_input{false};
    int output{-1};

 public:
    explicit CPU(const std::vector<int>& intcodes,
                 bool verbose = false)
        : memory(intcodes),
          instruction_pointer(0),
          input_tape(nullptr),
          use_input_tape(false),
          verbose(verbose) {
    }

    explicit CPU(const std::vector<int>& intcodes,
                 std::deque<int>* input_tape,
                 bool verbose = false)
        : memory(intcodes),
          instruction_pointer(0),
          input_tape(input_tape),
          use_input_tape(true),
          verbose(verbose) {
    }

    int get_parameter(const Instruction& instruction, const size_t& index) {
        int mode{
          static_cast<int>((instruction.opcode /
          utils::pow(
            static_cast<size_t>(10),
            static_cast<size_t>(2 + index))) % 10)};
        switch (mode) {
            case 0:  // position mode
                return memory[instruction.parameters[index]];
                break;
            case 1:  // immediate mode
                return instruction.parameters[index];
                break;
            default:
                throw std::runtime_error(
                    "Invalid parameter mode " + std::to_string(mode));
                break;
        }
    }

    bool execute(const Instruction& instruction) {
        bool halting{false};
        std::string raw_input{""};
        int input{-1};
        bool update_instruction_pointer{true};
        is_waiting_for_input = false;

        switch (instruction.opcode % 100) {
            case 1:
                assert(instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] =
                    get_parameter(instruction, 0) +
                    get_parameter(instruction, 1);
                break;
            case 2:
                assert(instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] =
                  get_parameter(instruction, 0) *
                  get_parameter(instruction, 1);
                break;
            case 3:
                assert(instruction.parameters.size() == 1);
                if (use_input_tape) {
                    if (input_tape->size() > 0) {
                        input = input_tape->front();
                        input_tape->pop_front();
                    } else {
                        is_waiting_for_input = true;
                    }
                } else {
                    std::cout << "Input required:" << std::endl;
                    std::cin >> raw_input;
                    input = std::stoi(raw_input);
                }
                memory[instruction.parameters[0]] = input;
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
                memory[instruction.parameters[2]] = static_cast<int>(
                  get_parameter(instruction, 0) <
                  get_parameter(instruction, 1));
                break;
            case 8:
                assert(instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] = static_cast<int>(
                  get_parameter(instruction, 0) ==
                  get_parameter(instruction, 1));
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

        if (!is_waiting_for_input && update_instruction_pointer) {
            this->instruction_pointer += 1 + instruction.parameters.size();
        }

        return is_waiting_for_input || halting;
    }

    bool execute() {
        int opcode = this->memory[this->instruction_pointer];
        Instruction instruction;
        instruction.opcode = opcode;
        std::vector<int> parameters;
        switch (opcode % 100) {
            case 1:
                parameters = {
                  this->memory[this->instruction_pointer+1],
                  this->memory[this->instruction_pointer+2],
                  this->memory[this->instruction_pointer+3] };
                break;
            case 2:
                parameters = {
                  this->memory[this->instruction_pointer+1],
                  this->memory[this->instruction_pointer+2],
                  this->memory[this->instruction_pointer+3] };
                break;
            case 3:
                parameters = {this->memory[this->instruction_pointer+1]};
                break;
            case 4:
                parameters = {this->memory[this->instruction_pointer+1]};
                break;
            case 5:
                parameters = {
                  this->memory[this->instruction_pointer+1],
                  this->memory[this->instruction_pointer+2] };
                break;
            case 6:
                parameters = {
                  this->memory[this->instruction_pointer+1],
                  this->memory[this->instruction_pointer+2] };
                break;
            case 7:
                parameters = {
                  this->memory[this->instruction_pointer+1],
                  this->memory[this->instruction_pointer+2],
                  this->memory[this->instruction_pointer+3] };
                break;
            case 8:
                parameters = {
                  this->memory[this->instruction_pointer+1],
                  this->memory[this->instruction_pointer+2],
                  this->memory[this->instruction_pointer+3] };
                break;
            default:
                parameters = {};
                break;
        }
        instruction.parameters = parameters;
        return execute(instruction);
    }

    std::pair<int, bool> run() {
        while (!execute()) {
        }
        return {this->memory[0], is_waiting_for_input};
    }

    void reset_instruction_pointer() {
        instruction_pointer = 0;
    }

    int get_output() const {
        return output;
    }

    void set_memory(size_t location, int value) {
        this->memory[location] = value;
    }

    std::vector<int> get_memory() const {
        return this->memory;
    }
};

int part_one(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    std::array<int, 5> phase_settings{{0, 1, 2, 3, 4}};
    std::array<std::deque<int>*, 5> inputs = {nullptr};
    for (size_t index{0}; index < 5; index++) {
        inputs[index] = new std::deque<int>();
    }
    int result{0};
    do {
        int thrust{0};
        for (size_t index{0}; index < 5; index++) {
            inputs[index]->push_back(phase_settings[index]);
            inputs[index]->push_back(thrust);
            CPU cpu(intcodes, inputs[index]);
            assert(!std::get<1>(cpu.run()));
            thrust = cpu.get_output();
        }
        if (thrust > result) {
            result = thrust;
        }
    } while (std::next_permutation(
        phase_settings.begin(),
        phase_settings.end()));

    for (size_t index{0}; index < 5; index++) {
        delete inputs[index];
    }
    return result;
}

int part_two(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    std::array<int, 5> phase_settings{{5, 6, 7, 8, 9}};
    bool done = false;
    std::array<std::deque<int>*, 5> inputs = {nullptr};
    std::array<CPU*, 5> amplifiers = {nullptr};
    int result{0};
    do {
        done = false;
        // initialize inputs & cpus
        for (size_t index{0}; index < 5; index++) {
            delete inputs[index];
            inputs[index] = new std::deque<int>();
            inputs[index]->push_back(phase_settings[index]);
            if (index == 0) {
                inputs[index]->push_back(0);
            }
            delete amplifiers[index];
            amplifiers[index] = new CPU(intcodes, inputs[index]);
        }

        while (!done) {
            done = true;
            for (size_t index{0}; index < 5; index++) {
                auto [_, is_waiting_for_input] = amplifiers[index]->run();
                inputs[(index + 1) % 5]->push_back(
                    amplifiers[index]->get_output());
                if (is_waiting_for_input) {
                    done = false;
                }
            }
        }
        if (amplifiers[4]->get_output() > result) {
            result = amplifiers[4]->get_output();
        }
    } while (std::next_permutation(
        phase_settings.begin(),
        phase_settings.end()));

    for (size_t index{0}; index < 5; index++) {
        delete inputs[index];
        delete amplifiers[index];
    }
    return result;
}

int main() {
    utils::Reader reader(
        std::filesystem::path("../../data/2019/input_07.txt"));
    auto input = utils::split_string(reader.get_lines()[0], ',');

    auto answer_one =  part_one(input);
    std::cout << "The answer to part one is: " << answer_one << std::endl;
    auto answer_two =  part_two(input);
    std::cout << "The answer to part two is: " << answer_two << std::endl;
    return 0;
}

