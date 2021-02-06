#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input) {
    std::vector<int> intcodes;
    for (auto line: input) {
        intcodes.push_back(std::stoi(line));
    }
    return intcodes;
}

int part_one(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    auto max_element = std::max_element(intcodes.begin(), intcodes.end());
    assert (max_element != intcodes.end());
    while (intcodes.size() < *max_element + 1)
    {
        intcodes.push_back(-1);
    }
    intcodes[1] = 12;
    intcodes[2] = 2;
    size_t position{0};
    auto current_intcode = intcodes[0];

    while (current_intcode != 99)
    {
        for (auto intcode: intcodes) std::cout << intcode << ", ";
        std::cout << std::endl;
        switch (current_intcode)
        {
            case 1:
                intcodes[intcodes[position + 3]] = intcodes[intcodes[position + 1]] + intcodes[intcodes[position + 2]];
                break;
            case 2:
                intcodes[intcodes[position + 3]] = intcodes[intcodes[position + 1]] * intcodes[intcodes[position + 2]];
                break;
            default:
                throw std::runtime_error("Invalid intcode: " + std::to_string(current_intcode));
        }
        position += 4;
        current_intcode = intcodes[position];
    }

    for (auto intcode: intcodes) std::cout << intcode << ", ";
    std::cout << std::endl;
    return intcodes[0];
}

int part_two(const std::vector<std::string>& input) {
    auto intcodes = prepare_input(input);
    return -7;
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
