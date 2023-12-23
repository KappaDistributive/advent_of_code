#include "../utils/input.hpp"
#include "fmt/core.h"
#include <regex>
#include <stdexcept>
#include <utility>

class Map {
 private:
   std::string m_instructions;
   size_t m_instruction_index;
   std::string m_node;
   std::map<std::string, std::pair<std::string, std::string>> m_network;
 public:
  Map(const std::vector<std::string>& input) : m_instruction_index(0), m_node("AAA") {
    this->m_instructions = input[0];
    std::regex regex{"^(\\w+)\\s=\\s\\((\\w+),\\s(\\w+)\\)$"};
    std::smatch matches;
    for (size_t index{2}; index < input.size(); ++index) {
      std::regex_match(input[index], matches, regex);
      assert (matches.size() == 4);
      this->m_network.insert(std::make_pair(matches[1].str(), std::make_pair(matches[2].str(), matches[3].str())));
    }
  }

  std::string step() {
    auto instruction = this->m_instructions[this->m_instruction_index++ % this->m_instructions.size()];
    switch (instruction) {
      case 'L': this->m_node = this->m_network[this->m_node].first; break;
      case 'R': this->m_node = this->m_network[this->m_node].second; break;
      default: throw std::runtime_error(fmt::format("Invalid instruction: {}", instruction));
    }

    return this->m_node;
  }

};

auto part_one(Map map) {
  size_t steps{1};
  while (map.step() != "ZZZ") {
    ++steps;
  }
  return steps;
}

auto part_two() {
  return 2;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_08.txt"};
  utils::Reader reader(input_path);
  auto map = Map(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(map));
  fmt::print("The answer to part two is: {}\n", part_two());

  return 0;
}
