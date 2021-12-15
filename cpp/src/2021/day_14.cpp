#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::map<std::pair<char, char>, size_t> polymer;
  for (size_t index{0}; index + 1 < input[0].size(); ++index) {
    std::pair<char, char> current_pair{
        std::make_pair(input[0][index], input[0][index + 1])};
    polymer.try_emplace(current_pair, 0);
    ++polymer.at(current_pair);
  }

  polymer.insert(
      std::make_pair(std::make_pair(input[0][input[0].size() - 1], '_'), 1));

  std::map<std::pair<char, char>, char> insertion_rules;

  for (size_t index{1}; index < input.size(); ++index) {
    auto splits = utils::split_string(input[index], ' ');
    if (splits.size() != 3) {
      continue;
    }
    insertion_rules.insert(std::make_pair(
        std::make_pair(splits[0][0], splits[0][1]), splits[2][0]));
  }

  return std::make_pair(polymer, insertion_rules);
}

auto step(const std::map<std::pair<char, char>, size_t>& polymer,
          const std::map<std::pair<char, char>, char>& insertion_rules) {
  std::map<std::pair<char, char>, size_t> new_polymer;

  for (auto [pair, count] : polymer) {
    if (insertion_rules.count(pair) > 0) {
      char insertion{insertion_rules.at(pair)};
      std::pair<char, char> lhs{pair.first, insertion};
      std::pair<char, char> rhs{insertion, pair.second};

      new_polymer.try_emplace(lhs, 0);
      new_polymer.try_emplace(rhs, 0);

      new_polymer.at(lhs) += count;
      new_polymer.at(rhs) += count;
    } else {
      new_polymer.insert(std::make_pair(pair, 1));
    }
  }

  return new_polymer;
}

auto element_count(const std::map<std::pair<char, char>, size_t>& polymer) {
  std::map<char, size_t> result;

  for (auto [pair, count] : polymer) {
    result.emplace(pair.first, 0);
    // result.emplace(pair.second, 0);
    result.at(pair.first) += count;
    // result.at(pair.second) += count;
  }

  return result;
}

auto run(const std::vector<std::string>& input, size_t num_iterations) {
  auto [polymer, insertion_rules] = prepare_input(input);

  // for (auto [pair, count] : polymer) {
  //   fmt::print("{}{}: {}\n", pair.first, pair.second, count);
  // }
  // for (auto [pair, insertion] : insertion_rules) {
  //   fmt::print("{}{} -> {}\n", pair.first, pair.second, insertion);
  // }

  for (size_t step_index{0}; step_index < num_iterations; ++step_index) {
    polymer = step(polymer, insertion_rules);
  }

  auto element_counts = element_count(polymer);

  // for (auto [element, count] : element_counts) {
  //   fmt::print("{}: {}\n", element, count);
  // }

  size_t maximum =
      std::max_element(element_counts.begin(), element_counts.end(),
                       [](const auto& lhs, const auto& rhs) {
                         return lhs.second < rhs.second;
                       })
          ->second;
  size_t minimum =
      std::min_element(element_counts.begin(), element_counts.end(),
                       [](const auto& lhs, const auto& rhs) {
                         return lhs.second < rhs.second;
                       })
          ->second;

  return maximum - minimum;
}

auto part_one(const std::vector<std::string>& input) {
  return run(input, 10);
}

auto part_two(const std::vector<std::string>& input) {
  return run(input, 40);
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_14{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);
  auto answer_two = part_two(input);
  fmt::print("The answer to part two is: {}\n", answer_two);

  return 0;
}
