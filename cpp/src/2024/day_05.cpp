#include "../utils/input.hpp"

std::pair<std::vector<std::pair<int, int>>, std::vector<std::vector<int>>>
parse(const std::vector<std::string> &input) {
  auto it{input.begin()};

  std::vector<std::pair<int, int>> rules;
  while (!it->empty()) {
    auto line = *it;
    ++it;
    auto splits = utils::split_string(line, '|');
    assert(splits.size() == 2);
    rules.push_back({std::stoi(splits[0]), std::stoi(splits[1])});
  }
  ++it;
  std::vector<std::vector<int>> orders;
  while (it != input.end()) {

    auto line = *it;
    ++it;
    auto splits = utils::split_string(line, ',');
    std::vector<int> order;
    for (auto split : splits) {
      order.push_back(std::stoi(split));
    }
    orders.push_back(order);
  }

  return {rules, orders};
}

bool is_valid(const std::pair<int, int> &rule, const std::vector<int> &order) {
  auto left = std::find(order.begin(), order.end(), rule.first);
  auto right = std::find(order.begin(), order.end(), rule.second);

  return left == order.end() || right == order.end() || left < right;
}

bool is_valid(const std::vector<std::pair<int, int>> &rules,
              const std::vector<int> &order) {
  return std::all_of(rules.begin(), rules.end(),
                     [&](const auto &rule) { return is_valid(rule, order); });
}

std::vector<int> fix_order(const std::vector<std::pair<int, int>> &rules,
                           std::vector<int> order) {
  bool ordered{true};
  do {
    ordered = true;
    for (const auto &rule : rules) {
      auto left = std::find(order.begin(), order.end(), rule.first);
      auto right = std::find(order.begin(), order.end(), rule.second);
      if (left != order.end() && right != order.end() && left > right) {
        std::iter_swap(left, right);
        ordered = false;
      }
    }
  } while (!ordered);
  return order;
}

auto part_one(const std::vector<std::pair<int, int>> &rules,
              const std::vector<std::vector<int>> &orders) {
  int result{0};
  for (const auto &order : orders) {
    if (is_valid(rules, order)) {
      assert(order.size() % 2 == 1);
      result += order[order.size() / 2];
    }
  }
  return result;
}

auto part_two(const std::vector<std::pair<int, int>> &rules,
              const std::vector<std::vector<int>> &orders) {
  int result{0};
  for (auto order : orders) {
    if (!is_valid(rules, order)) {
      assert(order.size() % 2 == 1);
      order = fix_order(rules, order);
      result += order[order.size() / 2];
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_05_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_05.txt"};
  utils::Reader reader(input_path);
  auto [rules, orders] = parse(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}",
                           part_one(rules, orders))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}",
                           part_two(rules, orders))
            << std::endl;

  return 0;
}
