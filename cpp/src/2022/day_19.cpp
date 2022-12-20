#include <regex>

#include "../utils/input.hpp"

struct Pack {
  int ore;
  int clay;
  int obsidian;
  int geode;

  void operator+=(const Pack &other) {
    this->ore += other.ore;
    this->clay += other.clay;
    this->obsidian += other.obsidian;
    this->geode += other.geode;
  }

  void operator-=(const Pack &other) {
    this->ore -= other.ore;
    this->clay -= other.clay;
    this->obsidian -= other.obsidian;
    this->geode -= other.geode;
  }

  Pack operator+(const Pack &other) {
    return Pack{this->ore + other.ore, this->clay + other.clay,
                this->obsidian + other.obsidian, this->geode + other.geode};
  }

  Pack operator+(int index) {
    Pack result{this->ore, this->clay, this->obsidian, this->geode};
    switch (index) {
    case 0:
      ++result.ore;
      break;
    case 1:
      ++result.clay;
      break;
    case 2:
      ++result.obsidian;
      break;
    case 3:
      ++result.geode;
      break;
    default:
      throw std::runtime_error("");
    }

    return result;
  }

  bool operator<=(const Pack &other) {
    return (this->ore <= other.ore) && (this->clay <= other.clay) &&
           (this->obsidian <= other.obsidian) && (this->geode <= other.geode);
  }

  friend std::ostream &operator<<(std::ostream &os, const Pack &pack) {
    os << "{Ore: " << pack.ore << ", Clay: " << pack.clay
       << ", Obsidian: " << pack.obsidian << ", Geode: " << pack.geode << '}';
    return os;
  }
};

struct State {
  Pack resources;
  Pack bots;
  int minutes;

  friend std::ostream &operator<<(std::ostream &os, const State &state) {
    os << "State{ Minutes: " << state.minutes
       << ", Resources: " << state.resources << ", Bots: " << state.bots
       << " }";
    return os;
  }
};

size_t find_max_geodes(const std::array<Pack, 4> &costs, int max_minutes) {
  int ore_max{0};
  for (const auto &cost : costs) {
    ore_max = std::max(ore_max, cost.ore);
  }
  int clay_max{0};
  for (const auto &cost : costs) {
    clay_max = std::max(clay_max, cost.clay);
  }
  std::stack<State> buffer;
  buffer.push(State{Pack{0, 0, 0, 0}, Pack{1, 0, 0, 0}, max_minutes});
  int best{0};

  while (!buffer.empty()) {
    auto state = buffer.top();
    buffer.pop();
    best = std::max(best,
                    state.resources.geode + state.minutes * state.bots.geode);
    for (int resource_type{3}; resource_type >= 0; --resource_type) {
      if (resource_type == 0 && state.bots.ore >= ore_max) {
        continue;
      }
      if (resource_type == 1 && state.bots.clay >= clay_max) {
        continue;
      }
      auto local_state = state;
      auto bot_costs = costs[resource_type];
      while (local_state.minutes > 1) {
        if (bot_costs <= local_state.resources) {
          local_state.resources += local_state.bots;
          --local_state.minutes;
          local_state.resources -= bot_costs;
          buffer.push(State{local_state.resources,
                            local_state.bots + resource_type,
                            local_state.minutes});
          break;
        }
        local_state.resources += local_state.bots;
        --local_state.minutes;
      }
    }
  }

  return best;
}

std::array<Pack, 4> get_costs(std::string blueprint) {
  std::regex numbers_regex{"(\\d+)"};
  std::smatch matches;
  std::vector<int> numbers;
  while (std::regex_search(blueprint, matches, numbers_regex)) {
    numbers.push_back(std::stoi(matches[1].str()));
    blueprint = matches.suffix().str();
  }
  assert(numbers.size() == 7);

  std::array<Pack, 4> costs;
  costs[0] = Pack{numbers[1], 0, 0, 0};
  costs[1] = Pack{numbers[2], 0, 0, 0};
  costs[2] = Pack{numbers[3], numbers[4], 0, 0};
  costs[3] = Pack{numbers[5], 0, numbers[6], 0};

  return costs;
}

auto part_one(const std::vector<std::string> &input) {
  int result{0};
  for (int index{0}; index < static_cast<int>(input.size()); ++index) {
    result += (index + 1) * find_max_geodes(get_costs(input[index]), 24);
  }

  return result;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_19_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_19.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
