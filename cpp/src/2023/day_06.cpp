#include "../utils/input.hpp"
#include <ostream>
#include <regex>
#include <string>
#include <utility>
#include <vector>

auto parse(const std::vector<std::string> &input) {
  std::vector<size_t> time;
  auto splits = utils::split_string(input[0], ' ');
  for (auto split : splits) {
    if (split.size() > 0 && split != "Time:")
      time.push_back(std::stoull(split));
  }

  std::vector<size_t> distance;
  splits = utils::split_string(input[1], ' ');
  for (auto split : splits) {
    if (split.size() > 0 && split != "Distance:")
      distance.push_back(std::stoull(split));
  }

  assert(time.size() == distance.size());

  std::vector<std::pair<size_t, size_t>> result;

  for (size_t index{0}; index < time.size(); ++index) {
    result.push_back(std::make_pair(time[index], distance[index]));
  }

  return result;
}

auto winning_combinations(size_t time, size_t distance) {
  std::vector<size_t> charges;
  for (size_t charge{0}; charge <= time; ++charge) {
    if ((time - charge) * charge > distance) {
      charges.push_back(charge);
    }
  }
  return charges;
}

auto part_one(std::vector<std::pair<size_t, size_t>> &input) {
  size_t result{1};
  for (auto [time, distance] : input) {
    result *= winning_combinations(time, distance).size();
  }

  return result;
}

auto part_two(std::vector<std::pair<size_t, size_t>> &input) {
  std::string time_buffer;
  std::string distance_buffer;

  for (auto [time, distance] : input) {
    time_buffer += std::to_string(time);
    distance_buffer += std::to_string(distance);
  }
  size_t time{std::stoull(time_buffer)};
  size_t distance{std::stoull(distance_buffer)};

  return winning_combinations(time, distance).size();
}

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_06_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_06.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
