#include <regex>  // NOLINT
#include <vector>
#include "boost/graph/adjacency_matrix.hpp"

#include "../utils/input.hpp"

typedef boost::adjacency_matrix<boost::directedS> Graph;

Graph
prepare_input(const std::vector<std::string>& input) {
  std::regex re{"(\\d+)\\s<->\\s([\\d,\\s]+)"};
  std::smatch matches;
  Graph graph(input.size());

  for (auto line : input) {
    std::regex_match(line, matches, re);
    std::string split = matches[2].str();
    utils::replace_all_substrings(&split, " ", "");
    std::string origin = matches[1].str();
    std::vector<std::string> destinations = utils::split_string(split, ',');

    for (auto destination : destinations) {
      add_edge(std::stoi(origin), std::stoi(destination), graph);
      add_edge(std::stoi(destination), std::stoi(origin), graph);
    }
  }

  return graph;
}

std::set<int> graphComponent(const int& vertex, const Graph& graph) {
  std::set<int> component;
  component.insert(vertex);
  bool searching{true};

  while (searching) {
    searching = false;
    for (auto origin : component) {
      auto [begin, end] = boost::adjacent_vertices(origin, graph);
      auto it = begin;
      while (it != end) {
        auto destination = *it;
        if (component.count(destination) == 0) {
          searching = true;
          component.insert(destination);
        }
        it++;
      }
    }
  }

  return component;
}

size_t part_one(const std::vector<std::string>& input) {
  auto graph = prepare_input(input);
  return graphComponent(0, graph).size();
}

size_t part_two(const std::vector<std::string>& input) {
  auto graph = prepare_input(input);
  std::set<int> visited;
  auto [begin, end] = boost::vertices(graph);
  auto it = begin;
  size_t num_components{0};

  while (it != end) {
    auto vertex = *it;
    if (visited.count(vertex) == 0) {
      auto component = graphComponent(vertex, graph);
      num_components++;
      for (auto entry : component) {
        visited.insert(entry);
      }
    }
    it++;
  }

  return num_components;
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

