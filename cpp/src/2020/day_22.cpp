#include <array>
#include <deque>
#include <regex>

#include "../utils/input.hpp"

std::pair<std::deque<int>, std::deque<int>> prepare_input(const std::vector<std::string>& input) {
  std::regex re{"^Player\\s(\\d+):$"};
  std::smatch matches;
  std::pair<std::deque<int>, std::deque<int>> decks;

  int turn{0};

  for (auto line: input) {
    if (std::regex_match(line, matches, re)) {
      turn = std::stoi(matches[1].str());
    } else if (line.size() != 0) {
      switch (turn) {
        case 1:
          decks.first.push_back(std::stoi(line));
          break;
        case 2:
          decks.second.push_back(std::stoi(line));
          break;
        default:
          throw std::invalid_argument("Invalid turn.");
          break;
      }
    }
  }

  return decks;
}

int calculate_score(const std::deque<int> deck) {
  int score{0};
  for (int index{1}; index <= static_cast<int>(deck.size()); index++) {
    score += index * deck[deck.size() - index];
  }
  return score;
}

int part_one(const std::vector<std::string>& input) {
  auto decks = prepare_input(input);

  // play the game
  while (decks.first.size() != 0 && decks.second.size() != 0) {
    if (decks.first[0] > decks.second[0]) {
      decks.first.push_back(decks.first[0]);
      decks.first.push_back(decks.second[0]);
    } else {
      decks.second.push_back(decks.second[0]);
      decks.second.push_back(decks.first[0]);
    }
    decks.first.pop_front();
    decks.second.pop_front();
  }

  // calculate score
  std::deque<int>& winner = decks.first.size() > 0 ? decks.first : decks.second;
  return calculate_score(winner);
}

// int part_two(const std::vector<std::string>& input) {
//   return 3;
// }

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2020/input_22.txt"));
  const std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  // std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

