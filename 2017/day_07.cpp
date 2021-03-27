#include <cassert>
#include <map>
#include <regex>  // NOLINT
#include <set>

#include "../utils/data.hpp"
#include "../utils/input.hpp"


std::map<std::string, std::pair<int, std::vector<std::string>>>
prepare_input(const std::vector<std::string>& input) {
  std::regex node_re{"^(\\w+) \\((\\d+)\\).*$"};
  std::smatch matches;
  std::map<std::string, std::pair<int, std::vector<std::string>>> result;

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
    result.insert({name, {weight, children}});
  }

  return result;
}


std::string part_one(const std::vector<std::string>& input) {
  auto prepared_input = prepare_input(input);
  std::set<std::string> root_candidates;

  for (auto [name, meta] : prepared_input) {
    root_candidates.insert(name);
  }

  for (auto [name, meta] : prepared_input) {
    auto [weight, children] = meta;
    for (auto child : children) {
      root_candidates.erase(child);
    }
  }

  assert(root_candidates.size() == 1);

  return *root_candidates.begin();
}


utils::Tree<utils::Node<std::pair<std::string, int>>>
create_tree(const std::vector<std::string>& input) {
  using Node = utils::Node<std::pair<std::string, int>>;
  using Tree = utils::Tree<Node>;

  auto prepared_input = prepare_input(input);
  auto root_name = part_one(input);
  auto [root_weight, _] = prepared_input.at(root_name);
  std::pair<std::string, int> root_data = {root_name, root_weight};
  Tree tree(Node{root_data});
  std::set<Node*> leafs{&tree.getRoot()};

  while (leafs.size() > 0) {
    std::set<Node*> new_leafs;
    for (auto leaf : leafs) {
      auto [weight, children] = prepared_input.at(leaf->getData().first);
      for (auto child_name : children) {
        auto child_meta = prepared_input.at(child_name);
        auto [child_weight, __] = child_meta;
        auto child_node = leaf->addChild(
          Node{std::make_pair(child_name, child_weight)});
        new_leafs.insert(child_node);
      }
    }
    leafs = new_leafs;
  }

  return tree;
}


int part_two(const std::vector<std::string>& input) {
  auto tree = create_tree(input);

  std::cout << tree.getRoot().size() << std::endl;
  return 5;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_07_mock.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

