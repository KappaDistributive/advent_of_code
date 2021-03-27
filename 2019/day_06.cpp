#include <cassert>
#include <map>
#include <set>

#include "../utils/data.hpp"
#include "../utils/input.hpp"

using Node = utils::Node<std::string>;
using Tree = utils::Tree<Node>;


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

int part_two(const std::vector<std::string>& input) {
  return -8;
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

