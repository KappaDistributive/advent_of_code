#include <cassert>
#include <map>
#include <set>
#include <stack>

#include "../utils/tree.hpp"
#include "../utils/input.hpp"

using Node = utils::tree::Node<std::string>;
using Tree = utils::tree::Tree<Node>;


Tree prepare_input(const std::vector<std::string>& input) {
  std::map<std::string, std::set<std::string>> orbits;
  for (auto line : input) {
    auto splits = utils::split_string(line, ')');
    assert(splits.size() == 2);
    if (orbits.count(splits[0]) == 0) {
      orbits.insert({splits[0], {splits[1]}});
    } else {
      orbits.at(splits[0]).insert(splits[1]);
    }
  }

  Tree tree(Node{"COM"});

  std::set<Node*> centers = {&tree.getRoot()};

  while (centers.size() > 0) {
    std::set<Node*> new_centers;
    for (auto center : centers) {
      if (orbits.count(center->getData()) == 0) {
        continue;
      }
      for (auto satellite : orbits.at(center->getData())) {
        auto moon = center->addChild(Node{satellite});
        new_centers.insert(moon);
      }
    }
    centers = new_centers;
  }


  return tree;
}

int countOrbits(Node* node, const size_t& level = 0) {
  size_t orbits{level};
  for (auto moon : node->getChildren()) {
    orbits += countOrbits(moon, level+1);
  }

  return orbits;
}

int part_one(const std::vector<std::string>& input) {
  auto tree = prepare_input(input);
  return countOrbits(&tree.getRoot());
}

std::vector<Node*> findPath(Node* origin, Node* destination) {
  std::set<std::stack<Node*>> paths;
  std::stack<Node*> path;
  path.push(origin);
  paths.insert(path);
  bool searching = !(origin == destination);

  while (searching) {
    std::set<std::stack<Node*>> extended_paths;
    for (auto path : paths) {
      Node* leaf = path.top();
      for (auto child : leaf->getChildren()) {
        auto extended_path = path;
        extended_path.push(child);
        extended_paths.insert(extended_path);
        if (child == destination) {
          std::vector<Node*> result;
          while (extended_path.size() > 0) {
            result.push_back(extended_path.top());
            extended_path.pop();
          }
          return std::vector<Node*>(result.rbegin(), result.rend());
        }
      }
    }
    paths = extended_paths;
  }

  throw std::runtime_error("No path found!");
}

int part_two(const std::vector<std::string>& input) {
  auto tree = prepare_input(input);
  auto root = &tree.getRoot();
  auto you = tree.findByData(Node("YOU"))[0];
  auto san = tree.findByData(Node("SAN"))[0];
  auto path_to_you = findPath(root, you);
  auto path_to_san = findPath(root, san);
  size_t common_length{0};
  while (path_to_you[common_length] == path_to_san[common_length]) {
    common_length++;
  }
  return path_to_you.size() + path_to_san.size() - 2 * (common_length + 1);
}

int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_06.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

