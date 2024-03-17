#include <regex>  // NOLINT

#include "../utils/input.hpp"

template <size_t win_condition>
class DiracDice {
 private:
  size_t m_num_rounds{0};
  size_t m_position_player_one;
  size_t m_position_player_two;
  size_t m_score_player_one{0};
  size_t m_score_player_two{0};

 public:
  explicit DiracDice(const std::vector<std::string>& input) {
    std::regex position_regex{"Player (\\d) starting position: (\\d)"};
    std::smatch matches;

    for (auto line : input) {
      std::regex_match(line, matches, position_regex);
      assertm(matches.size() == 3,
              std::format("Illegal input line `{}`.", line).c_str());

      if (std::stoi(matches[1].str()) == 1) {
        this->m_position_player_one = std::stoul(matches[2].str());
        assert(this->m_position_player_one-- > 0);

      } else {
        assertm(std::stoi(matches[1].str()) == 2,
                std::format("Illegal input line `{}`.", line).c_str());
        this->m_position_player_two = std::stoul(matches[2].str());
        assert(this->m_position_player_two-- > 0);
      }
    }
  }

  size_t position(size_t player) const noexcept {
    switch (player) {
      case 1:
        return (this->m_position_player_one % 10) + 1;
      case 2:
        return (this->m_position_player_two % 10) + 1;
      default:
        return 0;
    }
  }

  bool step() {
    size_t dice_sum{0};
    for (size_t index{0}; index < 3; ++index) {
      dice_sum += ((index + 3 * this->m_num_rounds) % 100) + 1;
    }
    return this->step(dice_sum);
  }

  bool step(size_t dice_sum) {
    if (this->m_num_rounds % 2 == 0) {
      this->m_position_player_one += dice_sum;
      this->m_position_player_one %= 10;
      this->m_score_player_one += this->position(1);
    } else {
      this->m_position_player_two += dice_sum;
      this->m_position_player_two %= 10;
      this->m_score_player_two += this->position(2);
    }

    ++this->m_num_rounds;

    return !this->done();
  }

  size_t final_result() const noexcept {
    if (!this->done()) {
      return 0;
    } else {
      return std::min(this->m_score_player_one, this->m_score_player_two) * 3 *
             this->m_num_rounds;
    }
  }

  bool done() const noexcept {
    return std::max(this->m_score_player_one, this->m_score_player_two) >=
           win_condition;
  }

  auto score_player_one() const noexcept { return this->m_score_player_one; }

  auto score_player_two() const noexcept { return this->m_score_player_one; }

  bool operator<(const DiracDice& other) const noexcept {
    return this->m_num_rounds < other.m_num_rounds ||
           this->m_position_player_one < other.m_position_player_one ||
           this->m_score_player_one < other.m_score_player_one ||
           this->m_position_player_two < other.m_position_player_two ||
           this->m_score_player_two < other.m_score_player_two;
  }

  friend std::ostream& operator<<(std::ostream& os, const DiracDice& game) {
    os << "Board:    ";
    for (size_t index{1}; index <= 10; ++index) {
      os << index << ' ';
    }
    os << '\n' << "Player 1: ";
    for (size_t index{1}; index < game.position(1); ++index) {
      os << "  ";
    }
    os << game.position(1) << '\n';
    os << "Score: " << std::setw(5) << game.m_score_player_one << '\n';
    os << "Player 2: ";
    for (size_t index{1}; index < game.position(2); ++index) {
      os << "  ";
    }
    os << game.position(2);
    os << "\nScore: " << std::setw(5) << game.m_score_player_two;
    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  DiracDice<1000> game{input};
  do {
    // std::cout << game << std::endl;
  } while (game.step());
  // std::cout << game << std::endl;

  return game.final_result();
}

auto part_two(const std::vector<std::string>& input) {
  constexpr size_t win_condition{21};
  std::map<DiracDice<win_condition>, size_t> games;
  DiracDice<win_condition> game{input};
  games.insert(std::make_pair(game, 1));
  size_t player_one_wins{0};
  size_t player_two_wins{0};
  const std::vector<std::pair<size_t, size_t>> dice_sums{
      {{3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}}};

  while (!games.empty()) {
    std::map<DiracDice<win_condition>, size_t> new_games;

    for (const auto& [game, count] : games) {
      for (auto [dice_sum, multiplier] : dice_sums) {
        auto new_game = game;
        new_game.step(dice_sum);
        if (new_game.done()) {
          if (new_game.score_player_one() >= win_condition) {
            player_one_wins += count * multiplier;
          } else {
            player_two_wins += count * multiplier;
          }
        } else {
          if (new_games.count(new_game) == 0) {
            new_games.insert({new_game, count * multiplier});
          } else {
            new_games.at(new_game) += count * multiplier;
          }
        }
      }
    }
    games = new_games;
    // std::cout << std::format("{}\t{}\n", player_one_wins, player_two_wins);
  }

  return std::max(player_one_wins, player_two_wins);
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
    std::format("../../data/2021/input_21{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << std::format("The answer to part one is: {}", answer_one) << std::endl;
  auto answer_two = part_two(input);
  std::cout << std::format("The answer to part two is: {}", answer_two) << std::endl;

  return 0;
}
