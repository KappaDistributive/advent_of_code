#include <algorithm>
#include <regex>
#include <set>

#include "../utils/input.hpp"

std::set<std::pair<char, char>> prepare_input(const std::vector<std::string>& input) {
    std::regex re{"^Step (\\w) must be finished before step (\\w) can begin.$"};
    std::smatch matches;
    std::set<std::pair<char, char>> restrictions;

    for (auto line: input) {
        std::regex_match(line, matches, re);
        restrictions.insert({matches[1].str()[0], matches[2].str()[0]});
    }
    return restrictions;
}

std::vector<char> prepare_alphabet(const std::vector<std::string>& input) {
    std::regex re{"^Step (\\w) must be finished before step (\\w) can begin.$"};
    std::smatch matches;
    std::vector<char> alphabet;

    for (auto line: input) {
        std::regex_match(line, matches, re);
        char before = matches[1].str()[0];
        char after = matches[2].str()[0];
        if (std::find(alphabet.begin(), alphabet.end(), before) == alphabet.end()) {
            alphabet.push_back(before);
        }
        if (std::find(alphabet.begin(), alphabet.end(), after) == alphabet.end()) {
            alphabet.push_back(after);
        }
    }
    std::sort(alphabet.begin(), alphabet.end());

    return alphabet;
}

size_t fac(size_t n) {
    if (n <= 1) {
        return 1;
    } else {
        return n * fac(n-1);
    }
}

std::string part_one(const std::vector<std::string>& input) {
    auto restrictions = prepare_input(input);
    auto alphabet = prepare_alphabet(input);
    std::string result;
    while (result.size() < alphabet.size()) {
        std::set<char> candidates;
        for (auto c: alphabet) {
            candidates.insert(c);
        }
        for (auto c: result) {
            candidates.erase(c);
        }
        for (auto [before, after]: restrictions) {
            if (std::find(result.begin(), result.end(), before) == result.end()) {
                candidates.erase(after);
            }
        }
        result += *std::min_element(candidates.begin(), candidates.end());
    }
    
    return result;
}

size_t part_two(const std::vector<std::string>& input) {
    return 9;
}



int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_07.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

