#include <cassert>

#include "../utils/input.hpp"

const char SAFE_TILE = '.';
const char TRAP_TILE = '^';

std::tuple<char, char, char> get_predeseccors(const std::string& previous_row, size_t index) {
  assert(previous_row.size() > 0);
  assert(index < previous_row.size());

  char left, center, right;
  left = index == 0 ? SAFE_TILE : previous_row[index-1];
  center = previous_row[index];
  right = index + 1 == previous_row.size() ? SAFE_TILE : previous_row[index+1];
  return {left, center, right};
}

char calculate_tile(const std::string& previous_row, size_t index) {
  auto [left, center, right] = get_predeseccors(previous_row, index);
  if (
    (left == TRAP_TILE && center == TRAP_TILE && right == SAFE_TILE) ||
    (left == SAFE_TILE && center == TRAP_TILE && right == TRAP_TILE) ||
    (left == TRAP_TILE && center == SAFE_TILE && right == SAFE_TILE) ||
    (left == SAFE_TILE && center == SAFE_TILE && right == TRAP_TILE)
  ) {
    return TRAP_TILE;
  } else {
    return SAFE_TILE;
  }
}

std::string calculate_row(const std::string& previous_row) {
  std::string row;
  for (size_t index{0}; index < previous_row.size(); index++) {
    row += calculate_tile(previous_row, index);
  }
  return row;
}

int part_one(const std::string& input) {
  std::vector<std::string> rows{{input}};
  while (rows.size() < 40) {
    rows.push_back(calculate_row(rows.back()));
  }
  int safe_tile_counter{0};
  for (auto row: rows) {
    for (auto tile: row) {
      safe_tile_counter += tile == SAFE_TILE;
    }
  }
  return safe_tile_counter;
}

uint64_t part_two(const std::string& input) {
  std::vector<std::string> rows{{input}};
  // It would suffice to calculate a single period and extrapolate from there. But this is still plenty fast.
  while (rows.size() < 400000) {
    rows.push_back(calculate_row(rows.back()));
  }
  uint64_t safe_tile_counter{0};
  for (auto row: rows) {
    for (auto tile: row) {
      safe_tile_counter += tile == SAFE_TILE;
    }
  }
  return safe_tile_counter;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_18.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

