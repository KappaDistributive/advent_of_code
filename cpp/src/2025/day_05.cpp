#include "../utils/input.hpp"

std::pair<std::vector<std::pair<int64_t, int64_t>>, std::vector<int64_t>>
parse_input(const std::vector<std::string> &data) {
  std::vector<std::pair<int64_t, int64_t>> ranges;
  std::vector<int64_t> ids;
  bool parsing_ranges = true;

  for (const auto &line : data) {
    if (line.empty()) {
      parsing_ranges = false;
      continue;
    }
    if (parsing_ranges) {
      auto parts = utils::split_string(line, '-');
      ranges.emplace_back(std::stoll(parts[0]), std::stoll(parts[1]));
    } else {
      ids.emplace_back(std::stoll(line));
    }
  }

  return {ranges, ids};
}

auto part_one(const std::vector<std::pair<int64_t, int64_t>> &ranges,
              const std::vector<int64_t> &ids) {
  int result{0};
  for (auto id : ids) {
    for (const auto [low, high] : ranges) {
      if (id >= low && id <= high) {
        result++;
        break;
      }
    }
  }
  return result;
}

auto part_two(std::vector<std::pair<int64_t, int64_t>> ranges) {
  // Merge overlapping ranges
  std::vector<std::pair<int64_t, int64_t>> merged;
  std::sort(ranges.begin(), ranges.end());
  for (const auto &[low, high] : ranges) {
    if (merged.empty() || merged.back().second < low) {
      merged.emplace_back(low, high);
    } else {
      merged.back().second = std::max(merged.back().second, high);
    }
  }
  int64_t result{0};
  for (const auto &[low, high] : merged) {
    result += high - low + 1;
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_05_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_05.txt"};
  utils::Reader reader(input_path);
  auto [ranges, ids] = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}",
                           part_one(ranges, ids))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(ranges))
            << std::endl;

  return EXIT_SUCCESS;
}
