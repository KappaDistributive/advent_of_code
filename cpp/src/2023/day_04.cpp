#include "../utils/input.hpp"
#include <algorithm>
#include <regex> //NOLINT
#include <string>
#include <vector>

class Card {
 public:
  int id;
  std::vector<int> winners;
  std::vector<int> numbers;

  Card(const std::string& description) {
    std::regex regex{"^Card\\s+(\\d+)\\:\\s([\\s\\d]+)\\s\\|\\s([\\s\\d]+)$"};
    std::smatch matches;
    std::regex_match(description, matches, regex);

    this->id = std::stoi(matches[1].str());
    for (auto number : utils::split_string(matches[2].str(), ' ')) {
      this->winners.push_back(std::stoi(number));
    }
    for (auto number : utils::split_string(matches[3].str(), ' ')) {
      this->numbers.push_back(std::stoi(number));
    }
  }

  int worth() const {
    int result{0};
    for (auto number : this->numbers) {
      if (std::find(this->winners.cbegin(), this->winners.cend(), number) != this->winners.cend()) {
        ++result;
      }
    }
    if (result > 0) {
      result = utils::pow<int>(2, result - 1);
    }

    return result;
  }
};

auto parse_input(const std::vector<std::string>& input) {
  std::vector<Card> cards;
  for (auto line: input) {
    cards.push_back(Card(line));
  }

  return cards;
}

auto part_one(const std::vector<Card>& cards) {
  int result{0};
  for (const auto& card: cards) {
    result += card.worth();
  }
  return result;
}

auto part_two() {
  return 0;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_04_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_04.txt"};
  utils::Reader reader(input_path);
  auto cards = parse_input(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(cards));
  fmt::print("The answer to part two is: {}\n", part_two());

  return 0;
}
