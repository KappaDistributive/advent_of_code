#include "../utils/input.hpp"
#include "fmt/core.h"
#include <cstddef>
#include <regex> // NOLINT
#include <vector>

struct Reveal {
  int red;
  int green;
  int blue;
};

class Game {
private:
  size_t m_id;
  std::vector<Reveal> m_reveals;
  Reveal m_limits{12, 13, 14};

public:
  Game(const std::string &description) {
    this->m_id =
        std::stoull(utils::split_string(description.substr(5), ':')[0]);
    auto tail = description.substr(description.find(':') + 1);
    auto rounds = utils::split_string(tail, ';');

    const std::regex color_regex{"(\\d+)\\s(red|blue|green)"};
    for (auto round : rounds) {
      std::sregex_iterator it(round.begin(), round.end(), color_regex);
      std::sregex_iterator end;
      Reveal reveal{0, 0, 0};
      for (; it != end; ++it) {
        std::string color = (*it)[2].str();
        int amount = std::stoi((*it)[1].str());
        if (color == "red") {
          assert(reveal.red == 0);
          reveal.red = amount;
        } else if (color == "green") {
          assert(reveal.green == 0);
          reveal.green = amount;
        } else {
          assert(color == "blue");
          assert(reveal.blue == 0);
          reveal.blue = amount;
        }
      }
      this->m_reveals.push_back(reveal);
    }
  }

  bool is_legal() const {
    for (auto reveal : this->m_reveals) {
      if (reveal.red > this->m_limits.red) {
        return false;
      }
      if (reveal.green > this->m_limits.green) {
        return false;
      }
      if (reveal.blue > this->m_limits.blue) {
        return false;
      }
    }
    return true;
  }

  size_t id() const { return this->m_id; }

  int power() const {
    Reveal reference{0, 0, 0};
    for (auto reveal : this->m_reveals) {
      reference.red = std::max(reference.red, reveal.red);
      reference.green = std::max(reference.green, reveal.green);
      reference.blue = std::max(reference.blue, reveal.blue);
    }
    return reference.red * reference.green * reference.blue;
  }
};

auto parse_input(const std::vector<std::string> &input) {
  std::vector<Game> games;
  for (auto line : input) {
    games.push_back(Game(line));
  }
  return games;
}

auto part_one(const std::vector<Game> games) {
  size_t result{0};
  for (auto game : games) {
    if (game.is_legal()) {
      result += game.id();
    }
  }
  return result;
}

auto part_two(const std::vector<Game> &games) {
  int result{0};
  for (auto game : games) {
    result += game.power();
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_02_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_02.txt"};
  utils::Reader reader(input_path);
  auto games = parse_input(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(games));
  fmt::print("The answer to part two is: {}\n", part_two(games));

  return 0;
}
