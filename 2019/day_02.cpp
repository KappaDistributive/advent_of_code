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

struct Instruction
{
    int opcode;
    std::vector<int> parameters; 
};
class CPU
{
private:
    std::vector<int> memory;
    size_t instruction_pointer;

public:
    explicit CPU(const std::vector<int>& intcodes)
        : memory(intcodes), instruction_pointer(0)
    {

    }

    bool execute (const Instruction& instruction)
    {
        bool halting{false};
        switch (instruction.opcode)
        {
            case 1:
                assert (instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] = memory[instruction.parameters[0]] + memory[instruction.parameters[1]];
                break;
            case 2:
                assert (instruction.parameters.size() == 3);
                memory[instruction.parameters[2]] = memory[instruction.parameters[0]] * memory[instruction.parameters[1]];
                break;
            case 99:
                halting = true;
                break;
            default:
                throw std::runtime_error("Encountered invalid opcode: " + std::to_string(instruction.opcode)); break;
        }
        this->instruction_pointer += 4;
        return halting;
    }

    bool execute ()
    {
        Instruction instruction {
            .opcode = this->memory[this->instruction_pointer],
            .parameters = {this->memory[this->instruction_pointer+1], this->memory[this->instruction_pointer+2], this->memory[this->instruction_pointer+3]}
        };
        return execute(instruction);
    }

    int run ()
    {
        while (!execute()) {}
        return this->memory[0];
    }

    void set_memory(size_t location, int value)
    {
        this->memory[location] = value;
    }

    std::vector<int> get_memory() const
    {
        return this->memory;
    }
};

int part_one(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    CPU cpu(intcodes);
    cpu.set_memory(1, 12);
    cpu.set_memory(2, 2);
    return cpu.run();
}

int part_two(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    const int target{19690720};
    for (int noun{0}; noun < 100; noun++)
    {
        for (int verb{0}; verb < 100; verb++)
        {
            CPU cpu(intcodes);
            cpu.set_memory(1, noun);
            cpu.set_memory(2, verb);
            if (cpu.run() == target)
            {
                return 100 * noun + verb;
            }
        }
    }
    return -1;
}

int main() {
    utils::Reader reader(std::filesystem::path("../2019/data/input_02.txt"));
    auto input = utils::split_string(reader.get_lines()[0], ',');

    auto answer_one =  part_one(input);
    std::cout << "The answer to part one is: " << answer_one << std::endl;
    auto answer_two =  part_two(input);
    std::cout << "The answer to part two is: " << answer_two << std::endl;
    return 0;
}
