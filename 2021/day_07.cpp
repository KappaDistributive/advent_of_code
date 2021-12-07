#include <limits>

#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<int> positions;

  for (auto line : input) {
    positions.push_back(std::stoi(line));
  }

  return positions;
}

auto total_distance(int target, const std::vector<int>& positions) {
  size_t distance{0};
  for (auto position : positions) {
    distance += static_cast<size_t>(std::abs(target - position));
  }

  return distance;
}

auto part_one(const std::vector<std::string>& input) {
  auto positions = prepare_input(input);
  auto minimum = *std::min_element(positions.begin(), positions.end());
  auto maximum = *std::max_element(positions.begin(), positions.end());

  size_t least_distance{std::numeric_limits<size_t>::max()};

  for (int target{minimum}; target <= maximum; ++target) {
    least_distance = std::min(least_distance, total_distance(target, positions));
  }
  return least_distance;
}

auto part_two(const std::vector<std::string>& input) {
  for (auto line : input) {
    std::cout << line << std::endl;
  }

  return 0;
}

int main() {
  // std::filesystem::path input_path{"../2021/data/input_07_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_07.txt"};
  utils::Reader reader(input_path);
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;

  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
