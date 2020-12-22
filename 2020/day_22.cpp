#include <array>
#include <deque>
#include <regex>

#include "../utils/input.hpp"

std::array<std::deque<int>, 2> prepare_input (const std::vector<std::string>& input)
{
  std::regex re{"^Player\\s(\\d+):$"};
  std::smatch matches;
  std::array<std::deque<int>, 2> decks;

  int turn{0};

  for (auto line: input)
  {
    if (std::regex_match(line, matches, re))
    {
      turn = std::stoi(matches[1].str());
    }
    else if (line.size() != 0)
    {
      switch (turn)
      {
        case 1:
          decks[0].push_back(std::stoi(line));
          break;
        case 2:
          decks[1].push_back(std::stoi(line));
          break;
        default:
          throw std::invalid_argument("Invalid turn.");
          break;
      }
    }
  }

  return decks;
}

int calculate_score(const std::deque<int> deck)
{
  int score{0};
  for (int index{1}; index <= deck.size(); index++)
  {
    score += index * deck[deck.size() - index];
  }
  return score;
}

int part_one(const std::vector<std::string>& input)
{
  auto decks = prepare_input(input);
  bool player_one{true};

  // play the game
  while (decks[0].size() != 0 && decks[1].size() != 0)
  {
    if (decks[0][0] > decks[1][0])
    {
      decks[0].push_back(decks[0][0]);
      decks[0].push_back(decks[1][0]);
    }
    else
    {
      decks[1].push_back(decks[1][0]);
      decks[1].push_back(decks[0][0]);
    }
    decks[0].pop_front();
    decks[1].pop_front();
  }

  // calculate score
  std::deque<int>& winner = decks[0].size() > 0 ? decks[0] : decks[1];
  return calculate_score(winner);
}

int part_two(const std::vector<std::string>& input)
{
  return 3;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_22.txt"));
  const std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
