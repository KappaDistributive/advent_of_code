#include <regex>
#include <vector>

#include "../utils/graph.hpp"
#include "../utils/input.hpp"

using utils::graph::DirectedGraph;
using utils::graph::Node;

DirectedGraph<int> prepare_input(const std::vector<std::string>& input) {
  std::regex re{"(\\d+)\\s<->\\s([\\d,\\s]+)"};
  std::smatch matches;
  DirectedGraph<int> graph;

  for (auto line : input) {
    std::regex_match(line, matches, re);
    std::string split = matches[2].str();
    utils::replace_all_substrings(&split, " ", "");
    std::string origin = matches[1].str();
    std::vector<std::string> destinations = utils::split_string(split, ',');

    for (auto destination : destinations) {
      auto node_origin = graph.addNode(std::stoi(origin));
      auto node_destination =  graph.addNode(std::stoi(destination));
      graph.addEdge(node_origin, node_destination);
      graph.addEdge(node_destination, node_origin);
    }
  }

  return graph;
}

size_t part_one(const std::vector<std::string>& input) {
  auto graph = prepare_input(input);

  auto node = graph.findNodeByData(0);

  return graph.getComponent(node).size();
}

size_t part_two(const std::vector<std::string>& input) {
  return 764321;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_12.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

