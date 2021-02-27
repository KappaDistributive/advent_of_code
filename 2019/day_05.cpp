#include <algorithm>
#include <cassert>

#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input) {
    std::vector<int> intcodes;
    std::transform(input.begin(), input.end(), std::back_inserter(intcodes),
        [](std::string code) -> int { return std::stoi(code); }
    );
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

 public:
    explicit CPU(const std::vector<int>& intcodes)
        : memory(intcodes), instruction_pointer(0) {
    }

    int get_parameter(const Instruction& instruction, const size_t& index) {
        int mode{(instruction.opcode / utils::pow(10, 2 + index)) % 10};
        switch (mode) {
            case 0:  // position mode
                return memory[instruction.parameters[index]];
                break;
            case 1: // immediate mode
                return instruction.parameters[index];
                break;
            default:
                throw std::runtime_error("Invalid parameter mode " + std::to_string(mode));
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
                memory[instruction.parameters[2]] = get_parameter(instruction, 0) + get_parameter(instruction, 1);
                break;
            case 2:
                assert(instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] = get_parameter(instruction, 0) * get_parameter(instruction, 1);
                break;
            case 3:
                assert(instruction.parameters.size() == 1);
                std::cout << "Input required:" << std::endl;
                std::cin >> input;
                memory[instruction.parameters[0]] = std::stoi(input);
                break;
            case 4:
                assert(instruction.parameters.size() == 1);
                std::cout << "Output: " << get_parameter(instruction, 0) << std::endl;
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
                memory[instruction.parameters[2]] = static_cast<int>(get_parameter(instruction, 0) < get_parameter(instruction, 1));
                break;
            case 8:
                assert(instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] = static_cast<int>(get_parameter(instruction, 0) == get_parameter(instruction, 1));
                break;
            case 99:
                halting = true;
                break;
            default:
                throw std::runtime_error("Encountered invalid opcode: " + std::to_string(instruction.opcode)); break;
        }

        if (update_instruction_pointer) {
            this->instruction_pointer += 1 + instruction.parameters.size();
        }

        return halting;
    }

    bool execute() {
        int opcode = this->memory[this->instruction_pointer];
        Instruction instruction;
        instruction.opcode = opcode;
        std::vector<int> parameters;
        switch (opcode % 100) {
            case 1:
                parameters = { this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2], this->memory[this->instruction_pointer+3] };
                break;
            case 2:
                parameters = { this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2], this->memory[this->instruction_pointer+3] };
                break;
            case 3:
                parameters = {this->memory[this->instruction_pointer+1]};
                break;
            case 4:
                parameters = {this->memory[this->instruction_pointer+1]};
                break;
            case 5:
                parameters = {this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2]};
                break;
            case 6:
                parameters = {this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2]};
                break;
            case 7:
                parameters = { this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2], this->memory[this->instruction_pointer+3] };
                break;
            case 8:
                parameters = { this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2], this->memory[this->instruction_pointer+3] };
                break;
            default:
                parameters = {};
                break;
        }
        instruction.parameters = parameters;
        return execute(instruction);
    }

    int run() {
        while (!execute()) {}
        return this->memory[0];
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
    CPU cpu(intcodes);
    return cpu.run();
}

int part_two(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    CPU cpu(intcodes);
    return cpu.run();
}

int main() {
    utils::Reader reader(std::filesystem::path("../2019/data/input_05.txt"));
    auto input = utils::split_string(reader.get_lines()[0], ',');

    auto answer_one =  part_one(input);
    std::cout << "The answer to part one is: " << answer_one << std::endl;
    auto answer_two =  part_two(input);
    std::cout << "The answer to part two is: " << answer_two << std::endl;
    return 0;
}

