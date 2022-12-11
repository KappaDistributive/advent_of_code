#include <functional>
#include <queue>

#include "../utils/input.hpp"

struct Monkey {
  std::queue<int> worry_levels;
  std::function<int(int, bool)> operation;
  std::function<int(int, bool)> test;
  size_t num_inspections;

  Monkey(const std::vector<std::string> &description) : num_inspections(0) {
    assert(description.size() == 6);
    assert(description[1].starts_with("  Starting items:"));
    for (auto split : utils::split_string(description[1].substr(17), ',')) {
      worry_levels.push(std::stoi(split));
    }

    // operation
    if (description[2] == "  Operation: new = old * old") {
      operation = [](int worry_level, bool verbose) {
        int result = worry_level * worry_level;
        if (verbose)
          fmt::print("    Worry level is multiplied by itself to {}.\n",
                     result);
        return result;
      };
    } else if (description[2].starts_with("  Operation: new = old *")) {
      operation = [description](int worry_level, bool verbose) {
        fmt::print("### {}\n", description[2].substr(24));
        int factor = std::stoi(description[2].substr(24));
        int result = worry_level * factor;
        if (verbose)
          fmt::print("    Worry level is mutiplied by {} to {}.\n", factor,
                     result);
        return result;
      };
    } else if (description[2].starts_with("  Operation: new = old +")) {
      operation = [description](int worry_level, bool verbose) {
        int summand = std::stoi(description[2].substr(24));
        int result = worry_level + summand;
        if (verbose)
          fmt::print("    Worry level is increased by {} to {}.\n", summand,
                     result);
        return result;
      };
    } else {
      throw std::runtime_error(
          fmt::format("Failed to parse operation: {}\n", description[2]));
    }
    // test
    int divosor = std::stoi(utils::split_string(description[3], ' ').back());
    int target_true =
        std::stoi(utils::split_string(description[4], ' ').back());
    int target_false =
        std::stoi(utils::split_string(description[5], ' ').back());
    test = [divosor, target_true, target_false](int worry_level, bool verbose) {
      bool is_divisible = worry_level % divosor == 0;
      if (verbose) {
        fmt::print("    Current worry level is ");
        if (!is_divisible) {
          fmt::print("not ");
        }
        fmt::print("divisible by {}.\n", divosor);
      }
      return is_divisible ? target_true : target_false;
    };
  }
};

struct Game {
  std::vector<Monkey> monkeys;
  size_t num_round{0};
  bool verbose = false;

  void round() {
    ++num_round;
    for (size_t monkey{0}; monkey < monkeys.size(); ++monkey) {
      if (verbose)
        fmt::print("Monkey {}:\n", monkey);
      while (!monkeys[monkey].worry_levels.empty()) {
        auto item = monkeys[monkey].worry_levels.front();
        monkeys[monkey].worry_levels.pop();
        if (verbose)
          fmt::print("  Monkey inspects an item wity worry level of {}.\n",
                     item);
        ++monkeys[monkey].num_inspections;
        item = monkeys[monkey].operation(item, verbose);
        item = item / 3;
        if (verbose)
          fmt::print("    Monkey gets bored with item. Worry level is divided "
                     "by 3 to {}.\n",
                     item);
        auto target_monkey = monkeys[monkey].test(item, verbose);
        monkeys[target_monkey].worry_levels.push(item);
        if (verbose)
          fmt::print("    Item with worry level {} is thrown to monkey {}.\n",
                     item, target_monkey);
      }
    }

    if (verbose)
      status();
  }

  void status() {
    fmt::print("After round {}, the monkeys are holding items with these worry "
               "levels:\n",
               num_round);
    for (size_t monkey{0}; monkey < monkeys.size(); ++monkey) {
      fmt::print("Monkey {}: ", monkey);
      auto items = monkeys[monkey].worry_levels;
      while (!items.empty()) {
        auto item = items.front();
        items.pop();
        fmt::print("{}", item);
        if (!items.empty()) {
          fmt::print(", ");
        }
      }
      fmt::print("\n");
    }
  }
};

auto prepare_input(const std::vector<std::string> &input) {
  std::vector<std::string> buffer;
  std::vector<Monkey> monkeys;
  for (const auto &line : input) {
    if (line == "") {
      monkeys.push_back(Monkey(buffer));
      buffer = {};
    } else {
      buffer.push_back(line);
    }
  }
  monkeys.push_back(Monkey(buffer));

  return Game{monkeys};
}

auto part_one(const std::vector<std::string> &input) {
  auto game = prepare_input(input);
  game.verbose = true;
  for (size_t round{1}; round <= 20; ++round) {
    game.round();
  }

  auto monkeys = game.monkeys;
  std::sort(monkeys.begin(), monkeys.end(), [](Monkey lhs, Monkey rhs) {
    return lhs.num_inspections > rhs.num_inspections;
  });

  return monkeys[0].num_inspections * monkeys[1].num_inspections;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_11_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_11.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
