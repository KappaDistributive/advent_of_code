#include <regex>  // NOLINT

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::regex area_regex{
      "^target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)..(-?\\d+)$"};
  std::smatch matches;

  std::regex_match(input[0], matches, area_regex);
  assertm(matches.size() == 5, "Encountered invalid input");

  return std::make_tuple(
      std::stoi(matches[1].str()), std::stoi(matches[2].str()),
      std::stoi(matches[3].str()), std::stoi(matches[4].str()));
}

auto shoot(int x, int y, int min_x, int max_x, int min_y, int max_y) {
  int current_x{0}, current_y{0}, top_y{0};
  bool hit_target{false};
  while (((max_x >= 0 && current_x <= max_x) ||
          (max_x < 0 && current_x >= min_x)) &&
         (y >= 0 || current_y >= min_y)) {
    current_x += x;
    current_y += y;

    top_y = std::max(top_y, current_y);

    if (min_x <= current_x && current_x <= max_x && min_y <= current_y &&
        current_y <= max_y) {
      hit_target = true;
    }
    if (x > 0) {
      --x;
    } else if (x < 0) {
      ++x;
    }
    --y;
  }

  return std::make_pair(hit_target, top_y);
}

auto part_one(const std::vector<std::string>& input) {
  auto [min_x, max_x, min_y, max_y] = prepare_input(input);
  // std::cout << fmt::format("target area: x={}..{} y={}..{}", min_x, max_x,
  // min_y, max_y) << std::endl;
  int result{0};

  for (int y{-std::max(std::abs(min_y), std::abs(max_y))};
       y <= std::max(std::abs(min_y), std::abs(max_y)); ++y) {
    for (int x{-std::max(std::abs(min_x), std::abs(max_x))};
         x <= std::max(std::abs(min_x), std::abs(max_x)); ++x) {
      result = std::max(result, shoot(x, y, min_x, max_x, min_y, max_y).second);
    }
  }

  return result;
}

auto part_two(const std::vector<std::string>& input) {
  auto [min_x, max_x, min_y, max_y] = prepare_input(input);
  size_t result{0};

  for (int y{-std::max(std::abs(min_y), std::abs(max_y))};
       y <= std::max(std::abs(min_y), std::abs(max_y)); ++y) {
    for (int x{-std::max(std::abs(min_x), std::abs(max_x))};
         x <= std::max(std::abs(min_x), std::abs(max_x)); ++x) {
      if (shoot(x, y, min_x, max_x, min_y, max_y).first) {
        ++result;
      }
    }
  }

  return result;
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_17{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);
  auto answer_two = part_two(input);
  fmt::print("The answer to part two is: {}\n", answer_two);

  return 0;
}
