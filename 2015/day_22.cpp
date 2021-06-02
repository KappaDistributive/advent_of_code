#include <regex>  // NOLINT

#include "../utils/input.hpp"

std::pair<int, int> prepare_input(const std::vector<std::string>& input) {
  std::regex hit_points_regex{"^Hit Points: (\\d+)$"};
  std::regex damage_regex{"^Damage: (\\d+)$"};
  std::smatch matches;
  int hit_points, damage;

  for (auto line : input) {
    if (std::regex_match(line, matches, hit_points_regex)) {
      hit_points = std::stoi(matches[1].str());
    } else if (std::regex_match(line, matches, damage_regex)) {
      damage = std::stoi(matches[1].str());
    } else {
      throw std::runtime_error("Found invalid line in input: " + line);
    }
  }

  return {hit_points, damage};
}

auto part_one(const std::vector<std::string>& input) {
  auto [hit_points, damage] = prepare_input(input);
  return hit_points * damage;
}

auto part_two(const std::vector<std::string>& input) { return ".."; }

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_22.txt"));
  const auto input = reader.get_lines();

  std::cout << "This will take a few minutes..." << std::endl;
  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  std::cout << "Bear with me..." << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

