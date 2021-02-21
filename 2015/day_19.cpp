#include <cassert>
#include <regex>
#include <set>

#include "../utils/input.hpp"

class Replacement {
  std::regex source_regex;
  std::string source_pattern;
  std::string target;

 public:
  explicit Replacement(std::string description, bool inverted = false) {
    std::smatch matches;
    std::regex re{"(\\w+)\\s=>\\s(\\w+)"};
    std::regex_match(description, matches, re);
    assert(matches.size() == 3);
    if (inverted) {
      source_pattern = matches[2].str();
      source_regex = std::regex{"(" + matches[2].str() + ")"};
      target = matches[1].str();
    } else {
      source_pattern = matches[1].str();
      source_regex = std::regex{"(" + matches[1].str() + ")"};
      target = matches[2].str();
    }
  }

  std::vector<std::string> apply(const std::string& input) const {
    std::vector<std::string> replacements;
    std::string suffix = input;
    std::string replacement;
    std::smatch matches;
    size_t offset{0};

    // std::cout << "Replacement: " << source_pattern << " => " << target << std::endl;
    // std::cout << "Before: " << input << std::endl;

    for (auto it =std::sregex_iterator(input.begin(), input.end(), source_regex); it != std::sregex_iterator(); ++it) {
      matches = *it;
      replacement = input.substr(0, matches.position(1)) +
                    target +
                    input.substr(matches.position(1) + source_pattern.size(), input.size());
      // std::cout << "After: " << replacement << std::endl;
      replacements.push_back(replacement);
    }

    return replacements;
  }
};

std::pair<std::vector<Replacement>, std::string> prepare_input(const std::vector<std::string>& input, bool inverted = false) {
  std::string molecule;
  std::vector<Replacement> replacements;
  bool first_section{true};

  for (auto line: input) {
    if (line.size() == 0) {
      first_section = false;
    } else if (first_section) {
      replacements.push_back(Replacement(line, inverted));
    } else {
      assert(molecule.size() == 0);
      molecule = line;
    }
  }

  return std::make_pair(replacements, molecule);
}

int part_one(const std::vector<std::string>& input) {
  auto [replacements, molecule] = prepare_input(input);
  std::set<std::string> molecules;

  // assert (replacements.size() == 3);
  // std::cout << "Molecule: " << molecule << std::endl;

  for (auto replacement: replacements) {
    for (auto result: replacement.apply(molecule)) {
      // std::cout << "Result: " << result << std::endl;
      molecules.insert(result);
    }
  }

  return molecules.size();
}

int part_two(const std::vector<std::string>& input) {
  auto [replacements, molecule] = prepare_input(input, true);
  int stage{0};

  while (molecule != "e") {
    stage++;
    for (auto replacement: replacements) {
      auto new_molecules = replacement.apply(molecule);
      if (new_molecules.size() > 0) {
        molecule = new_molecules[0]; // greedy decoding
        break;
      }
    }
  }
  return stage;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_19.txt"));
  const auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

