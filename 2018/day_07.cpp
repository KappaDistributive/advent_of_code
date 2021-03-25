#include <algorithm>
#include <regex>
#include <set>

#include "../utils/input.hpp"


std::set<std::pair<char, char>>
prepare_input(const std::vector<std::string>& input) {
    std::regex re{"^Step (\\w) must be finished before step (\\w) can begin.$"};
    std::smatch matches;
    std::set<std::pair<char, char>> restrictions;

    for (auto line : input) {
        std::regex_match(line, matches, re);
        restrictions.insert({matches[1].str()[0], matches[2].str()[0]});
    }
    return restrictions;
}


std::vector<char> prepare_alphabet(const std::vector<std::string>& input) {
    std::regex re{"^Step (\\w) must be finished before step (\\w) can begin.$"};
    std::smatch matches;
    std::vector<char> alphabet;

    for (auto line : input) {
        std::regex_match(line, matches, re);
        char before = matches[1].str()[0];
        char after = matches[2].str()[0];
        if (std::find(alphabet.begin(),
                      alphabet.end(),
                      before)
            == alphabet.end()) {
            alphabet.push_back(before);
        }
        if (std::find(alphabet.begin(),
                      alphabet.end(),
                      after)
            == alphabet.end()) {
            alphabet.push_back(after);
        }
    }
    std::sort(alphabet.begin(), alphabet.end());

    return alphabet;
}


std::set<char>
available_tasks(const std::vector<char>& alphabet,
                const std::set<std::pair<char, char>>& restrictions,
                const std::string& done_tasks,
                const std::string& started_tasks = "") {
    std::set<char> candidates;
    for (auto c : alphabet) {
        candidates.insert(c);
    }
    for (auto c : done_tasks + started_tasks) {
        candidates.erase(c);
    }
    for (auto [before, after] : restrictions) {
        if (std::find(done_tasks.begin(),
                      done_tasks.end(),
                      before)
            == done_tasks.end()) {
            candidates.erase(after);
        }
    }
    return candidates;
}

std::string part_one(const std::vector<std::string>& input) {
    auto restrictions = prepare_input(input);
    auto alphabet = prepare_alphabet(input);
    std::string result;
    while (result.size() < alphabet.size()) {
        auto candidates = available_tasks(alphabet, restrictions, result);
        result += *std::min_element(candidates.begin(), candidates.end());
    }

    return result;
}

size_t part_two(const std::vector<std::string>& input) {
    auto restrictions = prepare_input(input);
    auto alphabet = prepare_alphabet(input);
    size_t second{0};
    std::string result;
    std::string started;
    std::vector<std::tuple<char, int>> workers = {
        {'\0', -1},
        {'\0', -1},
        {'\0', -1},
        {'\0', -1},
        {'\0', -1},
    };
    bool finished_new_work{false};
    while (result.size() < alphabet.size()) {
        finished_new_work = false;
        for (size_t index{0}; index < workers.size(); index++) {
            auto worker = workers[index];
            if (std::get<0>(worker) == '\0') {  // worker is idle
                if (started.size() < alphabet.size()) {
                    auto candidates = available_tasks(
                        alphabet,
                        restrictions,
                        result,
                        started);
                    if (candidates.size() > 0) {
                        auto candidate = *std::min_element(candidates.begin(),
                                                           candidates.end());
                        started += candidate;
                        workers[index] = {
                            candidate,
                            second + static_cast<int>(candidate - 'A') + 61};
                    }
                }
            } else if (std::get<1>(worker) == second) {  // worker is done
                finished_new_work = true;
                result += std::get<0>(worker);
                workers[index] = {'\0', -1};
            }
        }
        if (!finished_new_work) {
            second++;
        }
    }

    return second;
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
