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
    return 7;
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
