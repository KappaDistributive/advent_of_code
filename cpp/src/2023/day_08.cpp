#include "../utils/input.hpp"
#include <regex>
#include <utility>

class Map {
public:
  std::string m_instructions;
  size_t m_instruction_index;
  std::string m_node;
  std::map<std::string, std::pair<std::string, std::string>> m_network;

  Map(const std::vector<std::string> &input)
      : m_instruction_index(0), m_node("AAA") {
    this->m_instructions = input[0];
    std::regex regex{"^(\\w+)\\s=\\s\\((\\w+),\\s(\\w+)\\)$"};
    std::smatch matches;
    for (size_t index{2}; index < input.size(); ++index) {
      std::regex_match(input[index], matches, regex);
      assert(matches.size() == 4);
      this->m_network.insert(
          std::make_pair(matches[1].str(),
                         std::make_pair(matches[2].str(), matches[3].str())));
    }
  }

  std::string step() {
    this->m_node = this->step(this->m_node);
    return this->m_node;
  }

  std::string step(std::string node, bool auto_inc = true) {
    auto instruction = this->m_instructions[this->m_instruction_index %
                                            this->m_instructions.size()];
    if (auto_inc) {
      ++this->m_instruction_index;
    }
    switch (instruction) {
    case 'L':
      node = this->m_network[node].first;
      break;
    case 'R':
      node = this->m_network[node].second;
      break;
    default:
      throw std::runtime_error(
          std::format("Invalid instruction: {}", instruction));
    }

    return node;
  }
};

auto part_one(Map map) {
  size_t steps{1};
  while (map.step() != "ZZZ") {
    ++steps;
  }
  return steps;
}

auto part_two(Map map) {
  std::vector<std::string> nodes;
  std::vector<size_t> steps;

  for (auto [node, _] : map.m_network) {
    if (node[node.size() - 1] == 'A') {
      nodes.push_back(node);
    }
  }
  for (auto &node : nodes) {
    size_t step{0};
    map.m_instruction_index = 0;
    while (node[node.size() - 1] != 'Z') {
      node = map.step(node);
      ++step;
    }
    std::cout << step << std::endl;
    steps.push_back(step);
  }
  size_t answer{steps[0]};
  for (size_t index{1}; index < steps.size(); ++index) {
    answer = std::lcm(answer, steps[index]);
  }
  return answer;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_08_mock2.txt"};
  std::filesystem::path input_path{"../../data/2023/input_08.txt"};
  utils::Reader reader(input_path);
  auto map = Map(reader.get_lines());

  // std::cout << std::format("The answer to part one is: {}", part_one(map)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(map)) << std::endl;

  return 0;
}
