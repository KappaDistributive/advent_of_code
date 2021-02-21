#include <algorithm>
#include <cassert>
#include <limits>
#include <map>
#include <set>

#include "../utils/input.hpp"

std::vector<std::pair<char, int>> parse_line(const std::string& line)
{
    auto splits = utils::split_string(line, ',');
    std::vector<std::pair<char, int>> result;
    std::transform(splits.begin(), splits.end(), std::back_insert_iterator(result),
        [](std::string entry) -> std::pair<char, int> { return std::make_pair(entry[0], std::stoi(entry.substr(1, entry.size())));}
    );
    return result;
}

void walk_the_line(const std::vector<std::pair<char, int>>& line, std::map<std::pair<int, int>, std::tuple<bool, bool, int, int>>& visits, bool first = true)
{
    std::pair<int, int> position{0, 0}, step{0, 0};
    int steps{0};
    
    for (auto [direction, distance]: line)
    {
        switch (direction)
        {
            case 'U': step = {0, 1}; break;
            case 'D': step = {0, -1}; break;
            case 'L': step = {-1, 0}; break;
            case 'R': step = {1, 0}; break;
            default: throw std::runtime_error("Invalid direction: " + direction); break;
        }
        for (int index{0}; index < distance; index++)
        {
            steps++;
            position.first += step.first;
            position.second += step.second;
            visits.emplace(position, std::tuple(false, false, -1, -1));
            if (first && !std::get<0>(visits.at(position)))
            {
                std::get<0>(visits.at(position)) = true;
                std::get<2>(visits.at(position)) = steps;
            }
            else if (!first && !std::get<1>(visits.at(position)))
            {
                std::get<1>(visits.at(position)) = true;
                std::get<3>(visits.at(position)) = steps;
            }
        }
    }
}

int part_one(const std::vector<std::string>& input) {
    std::vector<std::vector<std::pair<char, int>>> lines;
    std::transform(input.begin(), input.end(), std::back_insert_iterator(lines),
        [] (const std::string& line) -> std::vector<std::pair<char, int>> { return parse_line(line);}
    );
    auto first = lines[0];
    auto second = lines[1];
    std::map<std::pair<int, int>, std::tuple<bool, bool, int, int>> visits;
    visits.insert(std::make_pair(std::make_pair(0,0), std::make_tuple(true, true, -1, -1)));

    walk_the_line(first, visits, true);
    walk_the_line(second, visits, false);

    int result{std::numeric_limits<int>::max()};
    for (auto [key, value]: visits)
    {
        if (std::get<0>(value) && std::get<1>(value) && key != std::make_pair(0, 0))
        {
            result = std::min(result, abs(key.first) + abs(key.second));
        }
    }
    return result;
}

int part_two(const std::vector<std::string>& input) {
    std::vector<std::vector<std::pair<char, int>>> lines;
    std::transform(input.begin(), input.end(), std::back_insert_iterator(lines),
        [] (const std::string& line) -> std::vector<std::pair<char, int>> { return parse_line(line);}
    );
    auto first = lines[0];
    auto second = lines[1];
    std::map<std::pair<int, int>, std::tuple<bool, bool, int, int>> visits;
    visits.insert(std::make_pair(std::make_pair(0,0), std::make_tuple(true, true, 0, 0)));

    walk_the_line(first, visits, true);
    walk_the_line(second, visits, false);

    int result{std::numeric_limits<int>::max()};
    for (auto [key, value]: visits)
    {
        if (std::get<0>(value) && std::get<1>(value) && key != std::make_pair(0, 0))
        {
            assert (std::get<2>(value) > 0 && std::get<3>(value) > 0);
            result = std::min(result, std::get<2>(value) + std::get<3>(value));
        }
    }
    return result;
}

int main() {
    utils::Reader reader(std::filesystem::path("../2019/data/input_03.txt"));
    auto input = reader.get_lines();

    auto answer_one =  part_one(input);
    std::cout << "The answer to part one is: " << answer_one << std::endl;
    auto answer_two =  part_two(input);
    std::cout << "The answer to part two is: " << answer_two << std::endl;
    return 0;
}
