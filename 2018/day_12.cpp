#include <deque>
#include <map>
#include <regex>

#include "../utils/input.hpp"

std::pair<std::deque<char>, std::map<std::string, char>> prepare_input(const std::vector<std::string>& input) {
    std::regex initial_state_regex{"^initial state: ([\\.#]+)\\s*$"};
    std::regex transformation_regex{"^([\\.#]+) => ([\\.#])\\s*$"};
    std::smatch matches;

    std::deque<char> initial_state;;
    std::map<std::string, char> transformations;
    for (auto line: input) {
        if (std::regex_match(line, matches, initial_state_regex)) {
            assert(initial_state.size() == 0);
            for (auto character: matches[1].str()) {
                initial_state.push_back(character);
            }

        } else if (std::regex_match(line, matches, transformation_regex)) {
            transformations.insert({matches[1].str(), matches[2].str()[0]});
        }
    }

    return {initial_state, transformations};
}

std::deque<char> step(const std::deque<char>& state, const std::map<std::string, char>& transformations) {
    std::deque<char> new_state;
    int index;
    for (index = -5; index < static_cast<int>(state.size()) + 5; index++) {
        std::string local_state;
        for (int offset{0}; offset < 5; offset++) {
            if (index + offset >= 0 && index + offset < state.size()) {
                local_state += state[index + offset];
            } else {
                local_state += '.';
            }
        }
        assert(local_state.size() == 5);
        try {
            auto pot = transformations.at(local_state);
            new_state.push_back(pot);
        } catch (const std::out_of_range) {
            new_state.push_back('.');
        }
    }
    return new_state;
}

std::pair<std::deque<char>, int> strip(const std::deque<char>& input, char symbol) {
    auto left = input.begin();
    auto right = input.end();
    int offset{0};
    while (*left == symbol) {
        offset++;
        left++;
    }
    while (*std::prev(right) == symbol && right != input.begin()) {
        right--;
        if (left == right) {
            break;
        }
    }
    
    return {std::deque<char>(left, right), offset};
}

void update(std::deque<char>& state, int& offset, const std::map<std::string, char>& transformations) {
    state = step(state, transformations);
    offset -= 3;
    auto stripped_update = strip(state, '.');
    state = std::get<0>(stripped_update);
    offset += std::get<1>(stripped_update);
}

long long score(const std::deque<char>& state, const long& offset) {
    long long result{0};
    for (long long index{0}; index < state.size(); index++) {
        if (state[index] == '#') {
            result += offset + index;
        }
    }

    return result;
}

int part_one(const std::vector<std::string>& input) {
    std::deque<char> state;
    std::map<std::string, char> transformations;
    auto prepared_input = prepare_input(input);
    int offset{0};
    state = std::get<0>(prepared_input);
    transformations = std::get<1>(prepared_input);

    std::cout << "0: " << utils::stringify(state) << std::endl;
    for (size_t time{1}; time <= 20; time++) {
        update(state, offset, transformations);
        std::cout << "Offset: " << offset << std::endl;
        std::cout << time << ": " << utils::stringify(state) << std::endl;
    }

    return score(state, offset);
}


long long part_two(const std::vector<std::string>& input) {
    std::deque<char> state;
    std::map<std::string, char> transformations;
    auto prepared_input = prepare_input(input);
    int offset{0};
    int transition_offset;
    state = std::get<0>(prepared_input);
    transformations = std::get<1>(prepared_input);
    size_t time{0};
    while (true) {
        time++;
        auto old_offset = offset;
        auto old_state = state;
        update(state, offset, transformations);
        if (old_state == state) {
            transition_offset = offset - old_offset;
            std::cout << time << ": Found fixed point! Transition-Offset = " << transition_offset << std::endl;
            break;
        }
    }
    long long final_offset = static_cast<long long>(offset) + static_cast<long long>(50000000000ll - time) * static_cast<long long>(transition_offset);
    return score(state, final_offset);
}


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_12.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
