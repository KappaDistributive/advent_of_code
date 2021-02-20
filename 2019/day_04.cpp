#include "../utils/input.hpp"

bool has_pair(const int& candidate, bool strict = false)
{
    auto code = std::to_string(candidate);

    for (size_t index{0}; index + 1 < code.size(); index++)
    {
        if (code[index] == code[index+1])
        {
            if (strict)
            {
                if (
                    (index == 0 || code[index-1] != code[index])  && // check lower part
                    (index + 2 >= code.size() || code[index+2] != code[index]) // check upper part
                   )
                {
                    return true;
                }
            }
            else
            {
                return true;
            }
        }
    }

    return false;
}

bool never_decreasing(const int& candidate)
{
    auto code = std::to_string(candidate);

    for (size_t index{0}; index + 1 < code.size(); index++)
    {
        if (code[index+1] < code[index])
        {
            return false;
        }
    }

    return true;
}

size_t part_one(const int& min, const int& max) {
    size_t valid_passwords{0};

    for (int candidate{min}; candidate <= max; candidate++)
    {
        valid_passwords += has_pair(candidate) && never_decreasing(candidate);
    }

    return valid_passwords;
}

int part_two(const int& min, const int& max) {
    size_t valid_passwords{0};

    for (int candidate{min}; candidate <= max; candidate++)
    {
        valid_passwords += has_pair(candidate, true) && never_decreasing(candidate);
    }

    return valid_passwords;
}

int main() {
    utils::Reader reader(std::filesystem::path("../2019/data/input_04.txt"));
    auto input = utils::split_string(reader.get_lines()[0], '-');
    assert (input.size() == 2);

    auto answer_one =  part_one(std::stoi(input[0]), std::stoi(input[1]));
    std::cout << "The answer to part one is: " << answer_one << std::endl;
    auto answer_two =  part_two(std::stoi(input[0]), std::stoi(input[1]));
    std::cout << "The answer to part two is: " << answer_two << std::endl;
    return 0;
}
