#include <regex>  // NOLINT

#include "../utils/input.hpp"

#define assertm(exp, msg) assert(((void)msg, exp))

enum class Shuffle : size_t { new_stack, cut, increment };

std::ostream& operator<<(std::ostream& os, const Shuffle shuffle) {
  switch (shuffle) {
    case Shuffle::new_stack:
      os << "deal into new stack";
      break;
    case Shuffle::cut:
      os << "cut";
      break;
    case Shuffle::increment:
      os << "deal with increment";
      break;
    default:
      throw std::runtime_error("Unknown shuffle type");
  }
  return os;
}

auto prepare_input(const std::vector<std::string>& input) {
  std::regex new_stack_regex{"deal into new stack"};
  std::regex cut_regex{"cut (-?\\d+)"};
  std::regex increment_regex{"deal with increment (-?\\d+)"};
  std::smatch matches;

  std::vector<std::pair<Shuffle, int>> shuffles;

  for (auto line : input) {
    if (std::regex_match(line, matches, new_stack_regex); matches.size() > 0) {
      shuffles.push_back({Shuffle::new_stack, 0});
    } else if (std::regex_match(line, matches, cut_regex); matches.size() > 0) {
      shuffles.push_back({Shuffle::cut, std::stoi(matches[1].str())});
    } else if (std::regex_match(line, matches, increment_regex);
               matches.size() > 0) {
      shuffles.push_back({Shuffle::increment, std::stoi(matches[1].str())});
    } else {
      throw std::runtime_error("Invalid input");
    }
  }

  return shuffles;
}

auto perform_shuffle(const std::vector<int>& deck,
                     std::pair<Shuffle, int> shuffle) {
  std::vector<int> shuffled_deck;
  switch (shuffle.first) {
    case Shuffle::new_stack:
      for (auto it{deck.rbegin()}; it != deck.rend(); ++it) {
        shuffled_deck.push_back(*it);
      }
      break;
    case Shuffle::cut:
      for (size_t index{0}; index < deck.size(); ++index) {
        shuffled_deck.push_back(
            deck[static_cast<size_t>(index + deck.size() + shuffle.second) %
                 deck.size()]);
      }
      break;
    case Shuffle::increment:
      shuffled_deck = deck;
      for (size_t index{0}; index < deck.size(); ++index) {
        shuffled_deck[index * static_cast<size_t>(shuffle.second) %
                      deck.size()] = deck[index];
      }
      break;
    default:
      throw std::runtime_error("Unknown shuffle type");
      break;
  }
  return shuffled_deck;
}

auto part_one(const std::vector<std::string>& input) {
  auto shuffles = prepare_input(input);
  std::vector<int> deck;
  for (int card{0}; card < 10007; ++card) {
    deck.push_back(card);
  }
  for (auto shuffle : shuffles) {
    deck = perform_shuffle(deck, shuffle);
    // std::cout << shuffle.first << ": " << shuffle.second << std::endl;
    // for (size_t index{0}; index < 10; ++index) {
    //   std::cout << deck[index] << ", ";
    // }
    // std::cout << std::endl;
  }
  for (size_t index{0}; index < deck.size(); ++index) {
    if (deck[index] == 2019) {
      return index;
    }
  }
  throw std::runtime_error("This should never happen");
}

// auto part_two(const std::vector<std::string>& input) { return 234; }

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2019/input_22.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

