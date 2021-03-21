#include <algorithm>
#include <cassert>
#include <regex>
#include <stdexcept>
#include <thread>

#include "../utils/input.hpp"

std::string swap_position(const std::string& input, const size_t& x, const size_t& y) {
    assert(x < input.size() && y < input.size());
    if (x == y) {
        return input;
    }
    size_t left_index = std::min(x, y);
    size_t right_index = std::max(x, y);

    return input.substr(0, left_index) +
           input[right_index] +
           input.substr(left_index+1, right_index-left_index-1) +
           input[left_index] + input.substr(right_index+1);
}

std::string swap_letter(const std::string& input, const char& x, const char& y) {
    if (x == y) {
        return input;
    }
    size_t x_index = std::find(input.begin(), input.end(), x) - input.begin();
    size_t y_index = std::find(input.begin(), input.end(), y) - input.begin();

    return swap_position(input, x_index, y_index);
}

std::string rotate(const std::string& input, const int& rotation) {
    std::vector<char> buffer = std::vector<char>();
    for (auto c: input) {
        buffer.push_back(c);
    }
    buffer = utils::rotate_vector(buffer, rotation);
    std::string result;
    for (auto c: buffer) {
        result += c;
    }
    return result;
}

std::string rotate_relative(const std::string& input, const char& x) {
    size_t rotation = std::find(input.begin(), input.end(), x) - input.begin();
    if (rotation >= 4) {
        rotation++;
    }
    return rotate(input, rotation+1);
}

std::string reverse_positions(const std::string& input, size_t x, size_t y) {
    assert(x < input.size() && y < input.size());
    if (x == y) {
        return input;
    }
    size_t left_index = std::min(x, y);
    size_t right_index = std::max(x, y);
    std::string result = input.substr(0, left_index);
    for (size_t index{0}; index < right_index - left_index + 1; index++) {
        result += input[right_index - index];
    }
    result += input.substr(right_index+1);

    return result;
}

std::string move(const std::string& input, const size_t& x, const size_t& y) {
    assert(x < input.size() && y < input.size());
    std::string result;
    if (x < y) {
        return input.substr(0, x) +
               input.substr(x+1, y-x) +
               input[x] +
               input.substr(y+1);
    } else {
        return input.substr(0, y) +
               input[x] +
               input.substr(y, x-y) +
               input.substr(x+1);
    }
}

std::string apply_rule(const std::string& input, const std::string& rule) {
    std::regex swap_position_regex{"^swap position (\\d+) with position (\\d+)$"},
               swap_letter_regex{"^swap letter (\\w) with letter (\\w)$"},
               rotate_left_regex{"^rotate left (\\d+) steps?$"},
               rotate_right_regex{"^rotate right (\\d+) steps?$"},
               rotate_relative_regex{"^rotate based on position of letter (\\w)$"},
               reverse_positions_regex{"^reverse positions (\\d+) through (\\d+)$"},
               move_regex{"^move position (\\d+) to position (\\d+)$"};
    std::smatch matches;

    if (std::regex_match(rule, matches, swap_position_regex)) {
        return swap_position(input, std::stoi(matches[1]), std::stoi(matches[2]));
    } else if (std::regex_match(rule, matches, swap_letter_regex)) {
        return swap_letter(input, matches[1].str()[0], matches[2].str()[0]);
    } else if (std::regex_match(rule, matches, rotate_left_regex)) {
        return rotate(input, -std::stoi(matches[1]));
    } else if (std::regex_match(rule, matches, rotate_right_regex)) {
        return rotate(input, std::stoi(matches[1]));
    } else if (std::regex_match(rule, matches, rotate_relative_regex)) {
        return rotate_relative(input, matches[1].str()[0]);
    } else if (std::regex_match(rule, matches, reverse_positions_regex)) {
        return reverse_positions(input, std::stoi(matches[1]), std::stoi(matches[2]));
    } else if (std::regex_match(rule, matches, move_regex)) {
        return move(input, std::stoi(matches[1]), std::stoi(matches[2]));
    } else {
        throw std::runtime_error("No regex matched rule: `" + rule + "`");
    }
}

std::string part_one(const std::vector<std::string>& input) {
    std::string result{"abcdefgh"};
    for (auto rule: input) {
        result = apply_rule(result, rule);
    }
    return result;
}

bool test_candidate(const std::string& candidate, const std::string& target, const std::vector<std::string>& input) {
    std::string temp = candidate;
    for (auto rule: input) {
        temp = apply_rule(temp, rule);
    }
    return temp == target;
}

std::string part_two(const std::vector<std::string>& input) {
    const std::string& target{"fbgdceah"};
    // There are 8! = 40_320 possible permutations.
    // Therefore, the following brute-force-approach is barely feasible:
    std::vector<char> permutation{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'};
    size_t round{0};
    std::vector<std::string> candidates;

    do {
        std::string candidate;
        for (auto c: permutation) {
            candidate += c;
        }
        std::cout << "Round: " << ++round << "; testing " << candidate << std::endl;
        for (auto rule: input) {
            candidate = apply_rule(candidate, rule);
        }
        if (candidate == target) {
            candidate = "";
            for (auto c: permutation) {
                candidate += c;
            }
            return candidate;
        }
    } while (std::next_permutation(permutation.begin(), permutation.end()));

    throw std::runtime_error("Failed to find a suitable permutation!");
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_21.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

