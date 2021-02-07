#include <algorithm>
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

int part_one(const std::vector<std::string>& input) {
    std::vector<std::vector<std::pair<char, int>>> lines;
    std::transform(input.begin(), input.end(), std::back_insert_iterator(lines),
        [] (const std::string& line) -> std::vector<std::pair<char, int>> { return parse_line(line);}
    );
    auto first = lines[0];
    auto second = lines[1];
    std::map<std::pair<int, int>, std::pair<bool, bool>> visits;
    visits.insert(std::make_pair(std::make_pair(0,0), std::make_pair(true, true)));
    std::pair<int, int> position{0,0}, step{0, 0};

    for (auto [direction, distance]: first)
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
            position.first += step.first;
            position.second += step.second;
            visits.emplace(position, std::make_pair(false, false));
            visits.at(position).first = true;
        }
    }

    position = {0, 0};
    for (auto [direction, distance]: second)
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
            position.first += step.first;
            position.second += step.second;
            visits.emplace(position, std::make_pair(false, false));
            visits.at(position).second = true;
        }
    }

    std::set<std::pair<int, int>> intersections;

    for (auto [key, value]: visits)
    {
        if (value.first && value.second && key != std::make_pair(0, 0))
        {
            intersections.insert(key);
        }
    }

    int result{std::numeric_limits<int>::max()};
    for (auto intersection: intersections)
    {
        result = std::min(result, abs(intersection.first) + abs(intersection.second));
        // std::cout << "(" << intersection.first << ", " << intersection.second << ")" << std::endl;
    }

    return result;
}

int part_two(const std::vector<std::string>& input) {
    return -111;
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
