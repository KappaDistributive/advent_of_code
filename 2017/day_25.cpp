#include <cassert>
#include <regex>  // NOLINT

#include "../utils/input.hpp"

struct Op {
  int write_value;
  bool move_right;
  std::string next_state;
};

class TuringMachine {
 private:
  std::vector<int> m_tape_right;
  std::vector<int> m_tape_left;
  int m_cursor;
  std::string m_state;
  std::map<std::string, std::map<int, Op>> m_states;
  size_t m_num_steps{0};
  size_t m_diagonstic_interval{0};

  int& tape(int position) {
    if (position >= 0) {
      if (static_cast<size_t>(position) >= this->m_tape_right.size()) {
        this->m_tape_right.resize(position + 100);
      }
      return this->m_tape_right[position];
    } else {
      if (static_cast<size_t>(-position - 1) >= this->m_tape_left.size()) {
        this->m_tape_left.resize(-position + 99);
      }
      return this->m_tape_left[-position - 1];
    }
  }

 public:
  explicit TuringMachine(const std::vector<std::string>& description)
      : m_cursor(0), m_state("A") {
    const std::regex initial_state_regex{"^Begin in state (\\w+)\\.$"};
    const std::regex checksum_regex{
        "^Perform a diagnostic checksum after (\\d+) steps\\.$"};
    const std::regex new_state_regex{"^In state (\\w+):$"};
    const std::regex current_value_regex{
        "^\\s*If the current value is (\\d+):$"};
    const std::regex write_value_regex{"^\\s*- Write the value (\\d+)\\.$"};
    const std::regex move_regex{"^\\s*- Move one slot to the (left|right)\\.$"};
    const std::regex next_state_regex{"^\\s*- Continue with state (\\w+)\\.$"};

    std::string state;
    int current_value;
    Op partial;

    std::smatch matches;
    for (auto it{description.begin()}; it != description.end(); ++it) {
      if (std::regex_match(*it, matches, initial_state_regex)) {
        this->m_state = matches[1].str();
      } else if (std::regex_match(*it, matches, checksum_regex)) {
        this->m_diagonstic_interval = std::stoull(matches[1].str());
        std::cout << "Setting diagnostic interval to `"
                  << this->m_diagonstic_interval << "`." << std::endl;
      } else if (std::regex_match(*it, matches, new_state_regex)) {
        state = matches[1].str();
        std::cout << "Beginning the creation of state `" << state << "`."
                  << std::endl;
      } else if (std::regex_match(*it, matches, current_value_regex)) {
        current_value = std::stoi(matches[1].str());
        std::cout << "Found current value `" << current_value << "`."
                  << std::endl;
      } else if (std::regex_match(*it, matches, write_value_regex)) {
        partial.write_value = std::stoi(matches[1].str());
        std::cout << "Found Op.write_value `" << partial.write_value << "`."
                  << std::endl;
      } else if (std::regex_match(*it, matches, move_regex)) {
        if (matches[1].str() == "left") {
          partial.move_right = false;
        } else {
          assert(matches[1].str() == "right");
          partial.move_right = true;
        }
        std::cout << "Found Op.move_right `" << partial.move_right << "`."
                  << std::endl;
      } else if (std::regex_match(*it, matches, next_state_regex)) {
        partial.next_state = matches[1].str();
        std::cout << "Found Op.next_state`" << partial.next_state << "`."
                  << std::endl;

        std::cout << "Finished building Op." << std::endl;
        if (this->m_states.count(state) == 0) {
          this->m_states.insert(std::make_pair(
              state, std::map<int, Op>{{current_value, partial}}));
        } else {
          this->m_states.at(state).insert(
              std::make_pair(current_value, partial));
        }
      } else {
        std::cout << "Skipping line: `" << *it << "`." << std::endl;
      }
    }
  }

  bool step() {
    auto op_map = this->m_states.at(this->m_state);
    auto op = op_map.at(this->tape(this->m_cursor));
    this->tape(this->m_cursor) = op.write_value;
    if (op.move_right) {
      ++this->m_cursor;
    } else {
      --this->m_cursor;
    }
    this->m_state = op.next_state;

    ++this->m_num_steps;
    return (this->m_num_steps % this->m_diagonstic_interval != 0);
  }

  int checkum() const {
    int result{0};
    for (auto value : this->m_tape_left) {
      result += value;
    }
    for (auto value : this->m_tape_right) {
      result += value;
    }

    return result;
  }

  int tape(int position) const {
    int result{0};
    if (position >= 0) {
      if (static_cast<size_t>(position) < this->m_tape_right.size()) {
        result = this->m_tape_right[position];
      }
    } else {
      if (static_cast<size_t>(-position - 1) < this->m_tape_left.size()) {
        result = this->m_tape_left[-position - 1];
      }
    }
    return result;
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  const TuringMachine& machine) {
    os << "... ";
    for (int cursor{-10}; cursor <= 10; ++cursor) {
      os << (cursor == machine.m_cursor ? '[' : ' ');
      os << machine.tape(cursor);
      os << (cursor == machine.m_cursor ? ']' : ' ');
    }
    os << " ... ";

    if (machine.m_num_steps == 0) {
      os << "(before any steps; about to run state " << machine.m_state << ")";
    } else {
      os << "(after " << machine.m_num_steps << "step"
         << (machine.m_num_steps > 1 ? "s" : "") << "; about to run state "
         << machine.m_state << ")";
    }

    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  TuringMachine machine{input};
  do {
    // std::cout << machine << std::endl;
  } while (machine.step());

  return machine.checkum();
}

// auto part_two(const std::vector<std::string>& input) {
//   return result;
// }

int main() {
  // std::filesystem::path input_path{"../2017/data/input_25_mock.txt"};
  std::filesystem::path input_path{"../2017/data/input_25.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  // std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
