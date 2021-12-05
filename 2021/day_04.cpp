#include <algorithm>

#include "../utils/input.hpp"

class Board {
 private:
  std::array<int, 25> m_state;

 public:
  std::array<bool, 25> m_marks;
  explicit Board(const std::array<int, 25> state)
      : m_state(state), m_marks(std::array<bool, 25>{false}) {}

  void mark(int move) {
    for (size_t index{0}; index < m_state.size(); ++index) {
      if (this->m_state[index] == move) {
        this->m_marks[index] = true;
      }
    }
  }

  bool has_won() const {
    bool winning{false};

    // check rows
    for (size_t y{0}; y < 5 && !winning; ++y) {
      winning = true;
      for (size_t x{0}; x < 5 && winning; ++x) {
        winning &= this->m_marks[y * 5 + x];
      }
      if (winning) {
        return true;
      }
    }

    // check columns
    for (size_t x{0}; x < 5 && !winning; ++x) {
      winning = true;
      for (size_t y{0}; y < 5 && winning; ++y) {
        winning &= this->m_marks[y * 5 + x];
      }
      if (winning) {
        return true;
      }
    }

    /*// check diagonal `\`
    winning = true;
    for (size_t offset{0}; offset < 5 && winning; ++offset) {
      winning &= this->m_marks[offset * 5 + offset];
    }
    if (winning) {
      return true;
    }

    // check diagonal `/`
    winning = true;
    for (size_t offset{0}; offset < 5 && winning; ++offset) {
      winning &= this->m_marks[offset * 5 + (4 - offset)];
    }*/

    return winning;
  }

  auto sub_score() const {
    if (!this->has_won()) {
      return 0;
    }

    int score{0};
    for (size_t index{0}; index < this->m_state.size(); ++index) {
      if (!this->m_marks[index]) {
        score += this->m_state[index];
      }
    }

    return score;
  }

  friend std::ostream& operator<<(std::ostream& os, const Board& board) {
    for (size_t y{0}; y < 5; ++y) {
      for (size_t x{0}; x < 5; ++x) {
        if (board.m_marks[y + 5 + x]) {
          os << "\033[1m";
        }
        os << std::setw(2) << board.m_state[y * 5 + x];
        if (board.m_marks[y + 5 + x]) {
          os << "\033[0m";
        }
        if (x + 1 < 5) {
          os << ' ';
        }
      }
      os << '\n';
    }

    return os;
  }
};

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<int> moves;
  std::vector<Board> boards;

  std::array<int, 25> new_board;
  int row{-1};

  for (size_t line_index{0}; line_index < input.size(); ++line_index) {
    if (line_index == 0) {
      auto splits = utils::split_string(input[line_index], ',');
      for (auto split : splits) {
        moves.push_back(std::stoi(split));
      }
    } else {
      if (input[line_index] == "") {
        if (row != -1) {
          assert(row == 4);
          boards.push_back(Board(new_board));
          row = -1;
        }
      } else {
        ++row;
        int column{0};
        for (auto split : utils::split_string(input[line_index], ' ')) {
          new_board[row * 5 + column] = std::stoi(split);
          column += 1;
        }
        assert(column == 5);
      }
    }
  }

  if (new_board[0] != -1) {
    boards.push_back(Board(new_board));
  }

  return std::make_pair(moves, boards);
}

void print(const std::vector<int>& moves, const std::vector<Board>& boards) {
  for (auto move : moves) {
    std::cout << move << ", ";
  }
  std::cout << std::endl;

  for (auto board : boards) {
    std::cout << board << '\n' << std::endl;
  }
}

auto part_one(const std::vector<std::string>& input) {
  auto [moves, boards] = prepare_input(input);
  int score{0};
  print(moves, boards);

  for (auto move : moves) {
    for (auto& board : boards) {
      board.mark(move);
      score = board.sub_score() * move;
      if (score != 0) {
        return score;
      }
    }
  }

  return -1;
}

auto part_two(const std::vector<std::string>& input) { return 0; }

int main() {
  // std::filesystem::path input_path{"../2021/data/input_04_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_04.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;

  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
