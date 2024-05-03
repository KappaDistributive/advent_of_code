#include "../utils/input.hpp"

auto parse_input(const std::vector<std::string> &input) {
  std::vector<std::vector<int>> result;
  for (auto line : input) {
    std::vector<int> entries;
    auto splits = utils::split_string(line, ' ');
    for (auto entry : splits) {
      entries.push_back(std::stoi(entry));
    }
    result.push_back(entries);
  }
  return result;
}

int next_value(const std::vector<int>& history) {
  std::vector<std::vector<int>> expanded_history;
  expanded_history.push_back(history);
  bool done = false;
  size_t depth = 0;
  while (!done) {
    done = true;
    expanded_history.push_back(std::vector<int>{});
    for (size_t index{0}; index + 1 < expanded_history[depth].size(); ++index) {
      expanded_history[depth + 1].push_back(expanded_history[depth][index + 1] -
                                            expanded_history[depth][index]);
      if (expanded_history[depth + 1].back() != 0) {
        done = false;
      }
    }
    ++depth;
  }

  size_t index = expanded_history.size() - 1;
  expanded_history[index].push_back(0);
  while (index-- > 0) {
    expanded_history[index].push_back(expanded_history[index].back() +
                                      expanded_history[index + 1].back());
  }

  return expanded_history[0].back();
}

auto part_one(std::vector<std::vector<int>> histories) {
  int result{0};
  for (auto &history : histories) {
    result += next_value(history);
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_09_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_09.txt"};
  utils::Reader reader(input_path);
  auto input = parse_input(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}\n", part_one(input));
  std::cout << std::format("The answer to part two is: {}\n", part_two());

  return 0;
}
