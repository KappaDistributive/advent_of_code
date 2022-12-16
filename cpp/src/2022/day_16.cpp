#include <regex>

#include "../utils/combinatorics.hpp"
#include "../utils/input.hpp"

static const std::regex valve_regex(
    "^Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)$");

struct Network {
  std::vector<std::string> valves;
  std::map<std::string, std::set<std::string>> exists;
  std::map<std::string, size_t> flows;
  std::map<std::pair<std::string, std::string>, size_t> distances;

  Network(const std::vector<std::string> &input) {
    for (auto line : input) {
      std::smatch matches;
      std::regex_match(line, matches, valve_regex);
      assert(matches.ready());
      std::string name{matches[1].str()};
      valves.push_back(name);
      flows.emplace(std::make_pair(name, std::stoul(matches[2].str())));
      std::set<std::string> exit_nodes;
      for (auto exit : utils::split_string(matches[3].str(), ',')) {
        utils::replace_all_substrings(&exit, " ", "");
        exit_nodes.insert(exit);
      }
      exists.emplace(std::make_pair(name, exit_nodes));
    }

    for (auto source : valves) {
      for (auto destination : valves) {
        distances.emplace(std::make_pair(std::make_pair(source, destination),
                                         distance(source, destination, {})));
      }
    }
  }
  size_t distance(std::string source, std::string destination,
                  std::vector<std::string> path) {
    path.push_back(source);
    if (source == destination) {
      return path.size() - 1;
    }
    std::vector<size_t> distances;
    std::transform(
        this->exists.at(source).cbegin(), this->exists.at(source).cend(),
        std::back_inserter(distances), [&](std::string node) {
          return std::find(path.cbegin(), path.cend(), node) == path.cend()
                     ? distance(node, destination, path)
                     : std::numeric_limits<size_t>::max();
        });

    return *std::min_element(distances.cbegin(), distances.cend());
  }
};

size_t best_flow(const std::vector<std::string> path, const Network &network,
                 size_t time_left, const std::set<std::string> &allowed_nodes) {
  size_t max_flow{0};

  for (auto node : network.valves) {
    if (allowed_nodes.count(node) == 0 || network.flows.at(node) == 0 ||
        std::find(path.cbegin(), path.cend(), node) != path.cend()) {
      continue;
    }
    auto distance = network.distances.at(std::make_pair(path.back(), node));
    if (time_left <= distance) {
      continue;
    }
    auto time =
        time_left - network.distances.at(std::make_pair(path.back(), node)) - 1;
    auto new_path = path;
    new_path.push_back(node);
    max_flow = std::max(max_flow,
                        network.flows.at(node) * time +
                            best_flow(new_path, network, time, allowed_nodes));
  }

  return max_flow;
}

auto parse_input(const std::vector<std::string> &input) {
  return Network(input);
}

auto part_one(const Network &network) {
  std::set<std::string> allowed_nodes;
  for (auto node : network.valves) {
    allowed_nodes.insert(node);
  }
  return best_flow({"AA"}, network, 30, allowed_nodes);
}

auto part_two(const Network &network) {
  std::vector<std::string> key_nodes;
  for (auto node : network.valves) {
    if (node == "AA" || network.flows.at(node) > 0) {
      key_nodes.push_back(node);
    }
  }
  utils::combinatorics::Powerset<std::string> subsets(key_nodes);
  std::map<std::set<std::string>, size_t> scores;
  for (auto subset : subsets) {
    std::set<std::string> nodes;
    for (auto node : subset) {
      nodes.insert(node);
    }
    nodes.insert("AA");
    if (scores.count(nodes) == 0) {
      scores.emplace(
          std::make_pair(nodes, best_flow({"AA"}, network, 26, nodes)));
    }
  }

  size_t result{0};
  for (auto [used_nodes, score] : scores) {
    std::set<std::string> remaining_nodes;
    remaining_nodes.insert("AA");
    for (auto node : key_nodes) {
      if (used_nodes.count(node) == 0) {
        remaining_nodes.insert(node);
      }
    }
    result = std::max(result, score + scores.at(remaining_nodes));
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) {
  std::map<std::vector<std::string>, size_t> best_flows;
  return 1;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_16_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_16.txt"};
  utils::Reader reader(input_path);
  auto input = parse_input(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
