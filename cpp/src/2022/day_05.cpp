#include "../utils/input.hpp"

#include <cassert>
#include <regex> // NOLINT

struct Move {
  const size_t source;
  const size_t destination;
  const size_t num_moves;
  size_t num_moves_completed = 0;

  friend std::ostream &operator<<(std::ostream &os, const Move &move) {
    os << "moved " << move.num_moves_completed << " / " << move.num_moves
       << " from " << move.source << " to " << move.destination;
    return os;
  }
};

class Dock {
private:
  std::vector<std::stack<char>> m_stacks;
  std::vector<Move> m_moves;

  std::vector<std::stack<char>>
  decode_stacks(const std::vector<std::string> &input) const {
    std::vector<std::stack<char>> stacks;
    const std::regex index_regex{"^(:?(\\d+)|\\s)+$"};
    std::smatch matches;
    size_t line_index{0};
    for (const auto &line : input) {
      if (!std::regex_match(line, matches, index_regex)) {
        ++line_index;
        continue;
      }
      auto splits = utils::split_string(line, ' ');
      for (auto _ : splits) {
        stacks.push_back(std::stack<char>{});
      }
      break;
    }
    assert(line_index > 0);
    for (size_t index{line_index}; index > 0; --index) {
      auto line = input[index - 1];
      for (size_t offset{1}; offset < line.size(); offset += 4) {
        if (line[offset] != ' ') {
          stacks[(offset - 1) / 4].push(line[offset]);
        }
      }
    }
    return stacks;
  }

  std::vector<Move> decode_moves(const std::vector<std::string> &input) const {
    std::vector<Move> moves;
    const std::regex move_regex{"^move (\\d+) from (\\d+) to (\\d+)$"};
    std::smatch matches;
    for (const auto &line : input) {
      if (!std::regex_match(line, matches, move_regex)) {
        continue;
      }
      moves.push_back(Move{
          std::stoull(matches[2].str()),
          std::stoull(matches[3].str()),
          std::stoull(matches[1].str()),
      });
    }

    return moves;
  }

public:
  Dock(const std::vector<std::string> &input) {
    this->m_stacks = this->decode_stacks(input);
    this->m_moves = this->decode_moves(input);
  }

  bool move(bool part_two = false) {
    for (auto &move : this->m_moves) {
      if (part_two) {
        if (move.num_moves_completed < move.num_moves) {
          std::stack<char> buffer;
          while (move.num_moves_completed < move.num_moves) {
            assert(!this->m_stacks[move.source - 1].empty());
            buffer.push(this->m_stacks[move.source - 1].top());
            this->m_stacks[move.source - 1].pop();
            ++move.num_moves_completed;
          }
          while (!buffer.empty()) {
            this->m_stacks[move.destination - 1].push(buffer.top());
            buffer.pop();
          }
          return true;
        }
      } else {
        if (move.num_moves_completed < move.num_moves) {
          assert(!this->m_stacks[move.source - 1].empty());
          this->m_stacks[move.destination - 1].push(
              this->m_stacks[move.source - 1].top());
          this->m_stacks[move.source - 1].pop();
          ++move.num_moves_completed;
          return true;
        }
      }
    }
    return false;
  }

  std::string top() const {
    std::string result{""};
    for (const auto &stack : this->m_stacks) {
      result.push_back(stack.empty() ? ' ' : stack.top());
    }
    return result;
  }

  friend std::ostream &operator<<(std::ostream &os, const Dock &dock) {
    size_t max_height{0};
    std::vector<std::vector<char>> stacks;
    for (const auto &stack : dock.m_stacks) {
      max_height = std::max(stack.size(), max_height);
      stacks.push_back(std::vector<char>{});
    }
    for (size_t stack_index{0}; stack_index < stacks.size(); ++stack_index) {
      std::stack<char> temp_stack = dock.m_stacks[stack_index];
      while (!temp_stack.empty()) {
        stacks[stack_index].push_back(temp_stack.top());
        temp_stack.pop();
      }
    }
    for (size_t height{0}; height < max_height; ++height) {
      for (size_t position{0}; position < stacks.size(); ++position) {
        os << ' ';
        if (max_height - height - 1 < stacks[position].size()) {
          os << '['
             << stacks[position]
                      [height - (max_height - stacks[position].size())]
             << ']';
        } else {
          os << "   ";
        }
      }
      os << std::endl;
    }
    os << "  ";
    for (size_t index{0}; index < stacks.size(); ++index) {
      os << index + 1;
      if (index + 1 < stacks.size()) {
        os << "   ";
      }
    }
    os << std::endl;
    for (Move move : dock.m_moves) {
      os << move << std::endl;
    }

    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Dock dock(input);
  do {
    // std::cout << dock << std::endl;
  } while (dock.move());
  return dock.top();
}

auto part_two(const std::vector<std::string> &input) {
  Dock dock(input);
  do {
    // std::cout << dock << std::endl;
  } while (dock.move(true));
  return dock.top();
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_05_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_05.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
