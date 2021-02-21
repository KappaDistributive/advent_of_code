#include <cassert>
#include <map>
#include <regex>

#include "../utils/input.hpp"

class Seating {
 private:
  std::vector<std::string> names;
  std::map<std::pair<std::string, std::string>, int> happiness_deltas;
  std::regex re{"^(\\w+)\\swould\\s(lose|gain)\\s(\\d+)\\shappiness\\sunits\\sby\\ssitting\\snext\\sto\\s(\\w+)\\.$"};

 public:
  explicit Seating(const std::vector<std::string>& input) {
    std::smatch matches;
    for (auto line: input) {
      std::regex_match(line, matches, re);
      assert(matches.size() == 5);
      std::string alice = matches[1].str();
      bool gain = matches[2].str() == "gain";
      int happiness = std::stoi(matches[3].str());
      if (!gain) {
        happiness = -happiness;
      }
      std::string bob = matches[4].str();
      auto it_alice = std::find(names.begin(), names.end(), alice);
      if (it_alice == names.end()) {
        names.push_back(alice);
      }
      auto it_bob = std::find(names.begin(), names.end(), bob);
      if (it_bob == names.end()) {
        names.push_back(bob);
      }
      happiness_deltas.insert(std::make_pair(std::make_pair(alice, bob), happiness));
    }
  }

  void add_me() {
    for (auto name: names) {
      happiness_deltas.insert(std::make_pair(std::make_pair("<<me>>", name), 0));
      happiness_deltas.insert(std::make_pair(std::make_pair(name, "<<me>>"), 0));
    }
    names.push_back("<<me>>");
  }

  int get_total_happiness_delta(std::vector<int> indices) const {
    int happiness_delta{0};
    assert(indices.size() == names.size());

    for (int index{0}; index < indices.size(); index++) {
      happiness_delta += happiness_deltas.at(std::make_pair(names[indices[index]], names[indices[(index+indices.size()-1)%indices.size()]]));
      happiness_delta += happiness_deltas.at(std::make_pair(names[indices[index]], names[indices[(index+1)%indices.size()]]));
    }

    return happiness_delta;
  }

  std::vector<std::string> get_names() const {
    return names;
  }
};


int part_one(const std::vector<std::string>& input) {
  int result{0};
  std::vector<int> indices;
  Seating seating(input);

  for (size_t index{1}; index < seating.get_names().size(); index++) {
    indices.push_back(index);
  }

  do {
    auto current_indices = indices;
    current_indices.insert(current_indices.begin(), 0);
    auto current_happiness = seating.get_total_happiness_delta(current_indices);

    if (current_happiness > result) {
      result = current_happiness;
    }
  } while (std::next_permutation(indices.begin(), indices.end()));

  return result;
}

int part_two(const std::vector<std::string>& input) {
  int result{0};
  std::vector<int> indices;
  Seating seating(input);
  seating.add_me();

  for (size_t index{1}; index < seating.get_names().size(); index++) {
    indices.push_back(index);
  }

  do {
    auto current_indices = indices;
    current_indices.insert(current_indices.begin(), 0);
    auto current_happiness = seating.get_total_happiness_delta(current_indices);

    if (current_happiness > result) {
      result = current_happiness;
    }
  } while (std::next_permutation(indices.begin(), indices.end()));

  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_13.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

