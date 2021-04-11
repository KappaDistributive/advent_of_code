#include <array>
#include <cassert>
#include <regex>  // NOLINT
#include <vector>

#include "../utils/input.hpp"


class Bot {
 private:
  std::array<int, 2> data;

 public:
  Bot()
    : data({-1, -1}) {
  }

  int low(bool erase = false) {
    int result;
    size_t index;
    if (this->data[0] == -1) {
      result = this->data[1];
      index = 1;
    } else if (this->data[1] == -1) {
      result = this->data[0];
      index = 0;
    } else if (this->data[0] < this->data[1]) {
      result = this->data[0];
      index = 0;
    } else {
      result = this->data[1];
      index = 1;
    }
    if (erase) {
      this->data[index] = -1;
    }
    return result;
  }

  int high(bool erase = false) {
    int result;
    size_t index;
    if (this->data[0] == -1) {
      result = this->data[1];
      index = 1;
    } else if (this->data[1] == -1) {
      result = this->data[0];
      index = 0;
    } else if (this->data[0] > this->data[1]) {
      result = this->data[0];
      index = 0;
    } else {
      result = this->data[1];
      index = 1;
    }
    if (erase) {
      this->data[index] = -1;
    }
    return result;
  }

  void add(const int& value) {
    if (this->data[0] == -1) {
      this->data[0] = value;
    } else if (this->data[1] == -1) {
      this->data[1] = value;
    } else {
      std::runtime_error("No storage space available.");
    }
  }

  bool is_ready() {
    return (this->data[0] != -1 && this->data[1] != -1);
  }

  friend std::ostream& operator<< (std::ostream& os, Bot& bot) {
    os << "(" << bot.low() << ", " << bot.high() << ")";
    return os;
  }
};


std::vector<std::pair<std::string, bool>>
prepare_input(const std::vector<std::string>& input) {
  std::vector<std::pair<std::string, bool>> instructions;
  for (auto line : input) {
    instructions.push_back({line, false});
  }
  return instructions;
}


int part_one(const std::vector<std::string>& input) {
  std::vector<Bot> bots;
  std::vector<int> outputs;

  for (size_t index{0}; index < input.size(); index++) {
    bots.push_back(Bot());
    outputs.push_back(-1);
  }
  bool searching{true};
  auto instructions = prepare_input(input);
  std::regex assignment_regex{"^value\\s(\\d+)\\sgoes\\sto\\sbot\\s(\\d+)$"};
  std::regex transition_regex{"^bot\\s(\\d+)\\sgives\\slow\\sto\\s(bot|output)\\s(\\d+)\\sand\\shigh\\sto\\s(bot|output)\\s(\\d+)$"};  // NOLINT
  std::smatch matches;

  while (searching) {
    for (size_t index{0}; index < instructions.size(); index++) {
      auto [instruction, done] =  instructions[index];
      // searching = false;
      if (!done) {
        if (std::regex_match(instruction, matches, assignment_regex)) {
          int value = std::stoi(matches[1].str());
          int bot_index = std::stoi(matches[2].str());
          bots[bot_index].add(value);
          instructions[index] = {instruction, true};
          if (bots[bot_index].low() == 17 && bots[bot_index].high() == 61) {
            return bot_index;
          }
        } else if (std::regex_match(instruction, matches, transition_regex)) {
          int sender_index = std::stoi(matches[1].str());
          if (bots[sender_index].is_ready()) {
            bool low_receiver_is_bot;
            if (matches[2].str() == "bot") {
              low_receiver_is_bot = true;
            } else {
              assert(matches[2].str() == "output");
              low_receiver_is_bot = false;
            }
            int low_index = std::stoi(matches[3].str());
            bool high_receiver_is_bot;
            if (matches[4].str() == "bot") {
              high_receiver_is_bot = true;
            } else {
              assert(matches[4].str() == "output");
              high_receiver_is_bot = false;
            }
            int high_index = std::stoi(matches[5].str());

            if (low_receiver_is_bot) {
              bots[low_index].add(bots[sender_index].low(true));
              if (bots[low_index].low() == 17 && bots[low_index].high() == 61) {
                return low_index;
              }
            } else {
              outputs[low_index] = bots[sender_index].low(true);
            }
            if (high_receiver_is_bot) {
              bots[high_index].add(bots[sender_index].high(true));
              if (bots[high_index].low() == 17 &&
                  bots[high_index].high() == 61) {
                return high_index;
              }
            } else {
              outputs[high_index] = bots[sender_index].high(true);
            }
            instructions[index] = {instruction, true};
          }
        } else {
          throw std::runtime_error("Invalid instruction:\n" + instruction);
        }
      }
    }
  }
  return -1;
}


int part_two(const std::vector<std::string>& input) {
  std::vector<Bot> bots;
  std::vector<int> outputs;

  for (size_t index{0}; index < input.size(); index++) {
    bots.push_back(Bot());
    outputs.push_back(-1);
  }
  bool searching{true};
  auto instructions = prepare_input(input);
  std::regex assignment_regex{"^value\\s(\\d+)\\sgoes\\sto\\sbot\\s(\\d+)$"};
  std::regex transition_regex{"^bot\\s(\\d+)\\sgives\\slow\\sto\\s(bot|output)\\s(\\d+)\\sand\\shigh\\sto\\s(bot|output)\\s(\\d+)$"};  // NOLINT
  std::smatch matches;

  while (searching) {
    searching = false;
    for (size_t index{0}; index < instructions.size(); index++) {
      auto [instruction, done] =  instructions[index];
      if (!done) {
        searching = true;
        if (std::regex_match(instruction, matches, assignment_regex)) {
          int value = std::stoi(matches[1].str());
          int bot_index = std::stoi(matches[2].str());
          bots[bot_index].add(value);
          instructions[index] = {instruction, true};
        } else if (std::regex_match(instruction, matches, transition_regex)) {
          int sender_index = std::stoi(matches[1].str());
          if (bots[sender_index].is_ready()) {
            bool low_receiver_is_bot;
            if (matches[2].str() == "bot") {
              low_receiver_is_bot = true;
            } else {
              assert(matches[2].str() == "output");
              low_receiver_is_bot = false;
            }
            int low_index = std::stoi(matches[3].str());
            bool high_receiver_is_bot;
            if (matches[4].str() == "bot") {
              high_receiver_is_bot = true;
            } else {
              assert(matches[4].str() == "output");
              high_receiver_is_bot = false;
            }
            int high_index = std::stoi(matches[5].str());

            if (low_receiver_is_bot) {
              bots[low_index].add(bots[sender_index].low(true));
            } else {
              outputs[low_index] = bots[sender_index].low(true);
            }
            if (high_receiver_is_bot) {
              bots[high_index].add(bots[sender_index].high(true));
            } else {
              outputs[high_index] = bots[sender_index].high(true);
            }
            instructions[index] = {instruction, true};
          }
        } else {
          throw std::runtime_error("Invalid instruction:\n" + instruction);
        }
      }
    }
  }
  return outputs[0] * outputs[1] * outputs[2];
}


int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_10.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
