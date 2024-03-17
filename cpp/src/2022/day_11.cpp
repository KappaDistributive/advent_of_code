#include <functional>
#include <queue>

#include "../utils/input.hpp"

struct Monkey {
  std::queue<uint64_t> worry_levels;
  std::function<uint64_t(uint64_t, bool)> operation;
  uint64_t divisor;
  std::function<uint64_t(uint64_t, bool)> test;
  size_t num_inspections;

  Monkey(const std::vector<std::string> &description) : num_inspections(0) {
    assert(description.size() == 6);
    assert(description[1].starts_with("  Starting items:"));
    for (auto split : utils::split_string(description[1].substr(17), ',')) {
      worry_levels.push(std::stoi(split));
    }

    // operation
    if (description[2] == "  Operation: new = old * old") {
      operation = [](uint64_t worry_level, bool verbose) {
        uint64_t result = worry_level * worry_level;
        if (verbose)
          std::cout << std::format("    Worry level is multiplied by itself to {}.",
                     result) << std::endl;
        return result;
      };
    } else if (description[2].starts_with("  Operation: new = old *")) {
      operation = [description](uint64_t worry_level, bool verbose) {
        uint64_t factor = std::stoi(description[2].substr(24));
        uint64_t result = worry_level * factor;
        if (verbose)
          std::cout << std::format("    Worry level is mutiplied by {} to {}.", factor,
                     result) << std::endl;
        return result;
      };
    } else if (description[2].starts_with("  Operation: new = old +")) {
      operation = [description](uint64_t worry_level, bool verbose) {
        uint64_t summand = std::stoi(description[2].substr(24));
        uint64_t result = worry_level + summand;
        if (verbose)
          std::cout << std::format("    Worry level is increased by {} to {}.", summand,
                     result) << std::endl;
        return result;
      };
    } else {
      throw std::runtime_error(
          std::format("Failed to parse operation: {}", description[2]));
    }
    // test
    divisor = std::stoi(utils::split_string(description[3], ' ').back());
    uint64_t div = divisor;
    uint64_t target_true =
        std::stoi(utils::split_string(description[4], ' ').back());
    uint64_t target_false =
        std::stoi(utils::split_string(description[5], ' ').back());
    test = [div, target_true, target_false](uint64_t worry_level,
                                            bool verbose) {
      bool is_divisible = worry_level % div == 0;
      if (verbose) {
        std::cout << "    Current worry level is ";
        if (!is_divisible) {
          std::cout << "not ";
        }
        std::cout << std::format("divisible by {}.", div) << std::endl;
      }
      return is_divisible ? target_true : target_false;
    };
  }
};

struct Game {
  std::vector<Monkey> monkeys;
  uint64_t common_divisor;
  size_t num_round{0};
  bool nancy = true;
  bool verbose = false;

  void round() {
    ++num_round;
    for (size_t monkey{0}; monkey < monkeys.size(); ++monkey) {
      if (verbose)
        std::cout << std::format("Monkey {}:", monkey) << std::endl;
      while (!monkeys[monkey].worry_levels.empty()) {
        auto item = monkeys[monkey].worry_levels.front();
        monkeys[monkey].worry_levels.pop();
        if (verbose)
          std::cout << std::format("  Monkey inspects an item wity worry level of {}.",
                     item) << std::endl;
        ++monkeys[monkey].num_inspections;
        item = monkeys[monkey].operation(item, verbose);
        if (nancy) {
          item /= 3;
          if (verbose)
            std::cout << std::format(
                "    Monkey gets bored with item. Worry level is divided "
                "by 3 to {}.",
                item) << std::endl;
        } else {
          item = item % common_divisor;
        }
        auto target_monkey = monkeys[monkey].test(item, verbose);
        monkeys[target_monkey].worry_levels.push(item);
        if (verbose)
          std::cout << std::format("    Item with worry level {} is thrown to monkey {}.",
                     item, target_monkey) << std::endl;
      }
    }

    if (verbose)
      status();
  }

  void status() {
    std::cout << std::format("After round {}, the monkeys are holding items with these worry "
               "levels:",
               num_round) << std::endl;
    for (size_t monkey{0}; monkey < monkeys.size(); ++monkey) {
      std::cout << std::format("Monkey {}: ", monkey);
      auto items = monkeys[monkey].worry_levels;
      while (!items.empty()) {
        auto item = items.front();
        items.pop();
        std::cout << item;
        if (!items.empty()) {
          std::cout << ", ";
        }
      }
      std::cout << std::endl;
    }
  }
};

auto prepare_input(const std::vector<std::string> &input) {
  std::vector<std::string> buffer;
  std::vector<Monkey> monkeys;
  uint64_t common_divisor{1};
  for (const auto &line : input) {
    if (line == "") {
      monkeys.push_back(Monkey(buffer));
      buffer = {};
    } else {
      buffer.push_back(line);
    }
  }
  monkeys.push_back(Monkey(buffer));

  for (const auto &monkey : monkeys) {
    common_divisor *= monkey.divisor;
  }

  return Game{monkeys, common_divisor};
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

auto part_two(const std::vector<std::string> &input) {
  auto game = prepare_input(input);
  game.verbose = false;
  game.nancy = false;

  for (size_t round{1}; round <= 10000; ++round) {
    game.round();
    if (round == 1 || round == 20 || round % 1000 == 0) {
      std::cout << std::format("== After round {} ==", round) << std::endl;
      for (size_t monkey{0}; monkey < game.monkeys.size(); ++monkey) {
        std::cout << std::format("Monkey {} inspected items {} times.", monkey,
                   game.monkeys[monkey].num_inspections) << std::endl;
      }
    }
  }

  auto monkeys = game.monkeys;
  std::sort(monkeys.begin(), monkeys.end(), [](Monkey lhs, Monkey rhs) {
    return lhs.num_inspections > rhs.num_inspections;
  });

  return monkeys[0].num_inspections * monkeys[1].num_inspections;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_11_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_11.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
