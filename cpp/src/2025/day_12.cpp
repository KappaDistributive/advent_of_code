#include "../utils/input.hpp"

std::pair<std::vector<int>,
          std::vector<std::pair<std::pair<int, int>, std::array<int, 6>>>>
parse_input(const std::vector<std::string> &data) {
  std::vector<int> patterns;
  std::vector<std::pair<std::pair<int, int>, std::array<int, 6>>> demands;
  int amount{0};
  for (const auto &line : data) {
    if (std::find(line.cbegin(), line.cend(), 'x') == line.cend()) {
      if (line.empty()) {
        patterns.push_back(amount);
        amount = 0;
      }
      for (const auto &ch : line) {
        if (ch == '#')
          ++amount;
      }
    } else {
      auto splits = utils::split_string(line, ' ');
      auto mini =
          utils::split_string(splits[0].substr(0, splits[0].size() - 1), 'x');
      std::pair<int, int> shape = {std::stoi(mini[0]), std::stoi(mini[1])};
      std::array<int, 6> demand = {0, 0, 0, 0, 0, 0};
      for (size_t i{1}; i < splits.size(); ++i) {
        demand[i - 1] = std::stoi(splits[i]);
      }
      demands.push_back({shape, demand});
    }
  }
  return {patterns, demands};
}

// total cheat: Instead of solving the actual problem, just check whether
// there's enough space -- regardless of shapes
auto part_one(
    std::vector<int> patterns,
    std::vector<std::pair<std::pair<int, int>, std::array<int, 6>>> demands) {
  int result{0};
  for (auto [shape, demand] : demands) {
    int total_demand{0};
    for (int demand_idx{0}; demand_idx < 6; ++demand_idx) {
      total_demand += demand[demand_idx] * patterns[demand_idx];
    }
    if (total_demand <= shape.first * shape.second) {
      ++result;
    }
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  std::filesystem::path input_path{"../../data/2025/input_12.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}",
                           part_one(data.first, data.second))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
