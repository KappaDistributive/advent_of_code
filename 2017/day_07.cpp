#include <cassert>
#include <regex>  // NOLINT
#include <set>

#include "../utils/data.hpp"
#include "../utils/input.hpp"

std::vector<std::tuple<std::string, int, std::vector<std::string>>>
prepare_input(const std::vector<std::string>& input) {
  std::regex node_re{"^(\\w+) \\((\\d+)\\).*$"};
  std::smatch matches;
  std::vector<std::tuple<std::string, int, std::vector<std::string>>> result;

  for (auto line : input) {
    std::regex_match(line, matches, node_re);
    std::string name = matches[1].str();
    int weight = std::stoi(matches[2].str());
    std::vector<std::string> children;

    auto splits = utils::split_string(line, '>');
    if (splits.size() > 1) {
      auto tail = splits[1];
      utils::replace_all_substrings(&tail, ",", "");
      auto children_str = utils::split_string(tail, ' ');
      for (auto child : children_str) {
        auto copy = child;
        utils::replace_all_substrings(&copy, " ", "");
        children.push_back(copy);
      }
    }
    result.push_back({name, weight, children});
  }

  return result;
}

std::string part_one(const std::vector<std::string>& input) {
  auto prepared_input = prepare_input(input);
  std::set<std::string> root_candidates;

  for (auto [name, weight, children] : prepared_input) {
    root_candidates.insert(name);
  }

  for (auto [name, weight, children] : prepared_input) {
    for (auto child : children) {
      root_candidates.erase(child);
    }
  }

  assert(root_candidates.size() == 1);

  return *root_candidates.begin();
}

int part_two(const std::vector<std::string>& input) {
  return 34;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_07.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

