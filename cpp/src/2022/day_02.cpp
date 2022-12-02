#include <algorithm>
#include <cassert>

#include "../utils/input.hpp"

enum class Move { Rock, Paper, Scissors };

// (opponent, player) -> score
static const std::map<std::pair<Move, Move>, int> SCORES{{
    {{Move::Rock, Move::Rock}, 3},
    {{Move::Rock, Move::Paper}, 6},
    {{Move::Rock, Move::Scissors}, 0},
    {{Move::Paper, Move::Rock}, 0},
    {{Move::Paper, Move::Paper}, 3},
    {{Move::Paper, Move::Scissors}, 6},
    {{Move::Scissors, Move::Rock}, 6},
    {{Move::Scissors, Move::Paper}, 0},
    {{Move::Scissors, Move::Scissors}, 3},
}};

// (opponent, player) -> move
static const std::map<std::pair<Move, Move>, Move> REVERT{{
    {{Move::Rock, Move::Rock}, Move::Scissors},
    {{Move::Rock, Move::Paper}, Move::Rock},
    {{Move::Rock, Move::Scissors}, Move::Paper},
    {{Move::Paper, Move::Rock}, Move::Rock},
    {{Move::Paper, Move::Paper}, Move::Paper},
    {{Move::Paper, Move::Scissors}, Move::Scissors},
    {{Move::Scissors, Move::Rock}, Move::Paper},
    {{Move::Scissors, Move::Paper}, Move::Scissors},
    {{Move::Scissors, Move::Scissors}, Move::Rock},
}};

static const std::map<Move, int> VALUES{{
    {Move::Rock, 1},
    {Move::Paper, 2},
    {Move::Scissors, 3},
}};

Move decode(char input) {
  Move move;
  switch (input) {
  case 'A':
    move = Move::Rock;
    break;
  case 'B':
    move = Move::Paper;
    break;
  case 'C':
    move = Move::Scissors;
    break;
  case 'X':
    move = Move::Rock;
    break;
  case 'Y':
    move = Move::Paper;
    break;
  case 'Z':
    move = Move::Scissors;
    break;
  default:
    throw std::runtime_error(fmt::format("Invalid move `{}`\n", input));
  }
  return move;
}

auto prepare_input(const std::vector<std::string> &input) {
  std::vector<std::pair<Move, Move>> strategy;
  for (auto line : input) {
    assert(line.size() == 3);
    strategy.push_back({decode(line[0]), decode(line[2])});
  }

  return strategy;
}

auto part_one(const std::vector<std::pair<Move, Move>> &strategy) {
  int answer{0};
  for (const auto &moves : strategy) {
    answer += VALUES.at(std::get<1>(moves)) + SCORES.at(moves);
  }

  return answer;
}

auto part_two(const std::vector<std::pair<Move, Move>> &strategy) {
  int answer{0};
  for (const auto &moves : strategy) {
    Move player = REVERT.at(moves);
    answer += VALUES.at(player) + SCORES.at({std::get<0>(moves), player});
  }

  return answer;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_02_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_02.txt"};
  utils::Reader reader(input_path);
  auto input = prepare_input(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
