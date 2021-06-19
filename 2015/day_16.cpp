#include "../utils/input.hpp"

std::map<std::string, int> get_requirements(
    const std::vector<std::string>& ticker) {
  std::map<std::string, int> requirements;
  std::regex re{"^(\\w+):\\s(\\d+)$"};
  std::smatch matches;

  for (auto line : ticker) {
    std::regex_match(line, matches, re);
    assert(matches.size() == 3);
    requirements.insert(
        std::make_pair(matches[1].str(), std::stoi(matches[2].str())));
  }

  return requirements;
}

std::vector<std::map<std::string, int>> get_aunts(
    const std::vector<std::string>& input) {
  std::vector<std::map<std::string, int>> aunts;
  std::regex re{"(\\w+): (\\d+)"};
  std::smatch matches;
  std::string temp;

  for (auto line : input) {
    temp = line;
    std::map<std::string, int> aunt;
    while (std::regex_search(temp, matches, re)) {
      assert(matches.size() == 3);
      if (matches[1].str() == "Sue") {
        continue;
      }
      aunt.insert(
          std::make_pair(matches[1].str(), std::stoi(matches[2].str())));
      temp = matches.suffix().str();
    }
    aunts.push_back(aunt);
  }

  return aunts;
}

int part_one(const std::vector<std::string>& input,
             const std::vector<std::string>& ticker) {
  int result{0};
  auto requirements = get_requirements(ticker);
  auto aunts = get_aunts(input);

  for (size_t index{0}; index < aunts.size(); index++) {
    auto aunt = aunts[index];
    bool found = true;
    for (auto [key, value] : aunt) {
      if (requirements.at(key) != value) {
        found = false;
        break;
      }
    }
    if (found) {
      result = index + 1;
      break;
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input,
             const std::vector<std::string>& ticker) {
  int result{0};
  auto requirements = get_requirements(ticker);
  auto aunts = get_aunts(input);

  for (size_t index{0}; index < aunts.size(); index++) {
    auto aunt = aunts[index];
    bool found = true;
    for (auto [key, value] : aunt) {
      if (key == "cats" || key == "trees") {
        if (requirements.at(key) >= value) {
          found = false;
          break;
        }
      } else if (key == "pomeranians" || key == "goldfish") {
        if (requirements.at(key) <= value) {
          found = false;
          break;
        }
      } else if (requirements.at(key) != value) {
        found = false;
        break;
      }
    }
    if (found) {
      result = index + 1;
      break;
    }
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_16.txt"));
  auto input = reader.get_lines();
  std::vector<std::string> ticker = {
      "children: 3", "cats: 7",    "samoyeds: 2", "pomeranians: 3",
      "akitas: 0",   "vizslas: 0", "goldfish: 5", "trees: 3",
      "cars: 2",     "perfumes: 1"};

  std::cout << "The answer to part one is: " << part_one(input, ticker)
            << std::endl;
  std::cout << "The answer to part two is: " << part_two(input, ticker)
            << std::endl;

  return 0;
}

