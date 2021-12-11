#include <cassert>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

auto prepare_input(const std::vector<std::string>& input) {
  std::array<int, 100> grid;
  size_t x{0}, y{0};
  for (auto line : input) {
    for (auto energy : line) {
      int energy_level{energy - '0'};
      assert(0 <= energy_level && energy_level < 10);
      grid[y * 10 + (x++ % 10)] = energy_level;
    }
    ++y;
  }

  return grid;
}

auto step(const std::array<int, 100>& grid) {
  auto new_grid = grid;
  std::array<bool, 100> has_flashed{false};
  bool flashing{true};

  for (auto& energy : new_grid) {
    ++energy;
  }
  while (flashing) {
    flashing = false;

    for (size_t y{0}; y < 10; ++y) {
      for (size_t x{0}; x < 10; ++x) {
        if (new_grid[y * 10 + x] > 9 && !has_flashed[y * 10 + x]) {
          has_flashed[y * 10 + x] = true;
          flashing = true;
          for (int offset_y{-1}; offset_y <= 1; ++offset_y) {
            for (int offset_x{-1}; offset_x <= 1; ++offset_x) {
              if (offset_x != 0 || offset_y != 0) {
                if (0 <= static_cast<int>(y) + offset_y &&
                    static_cast<int>(y) + offset_y < 10 &&
                    0 <= static_cast<int>(x) + offset_x &&
                    static_cast<int>(x) + offset_x < 10) {
                  ++new_grid[static_cast<size_t>(y + offset_y) * 10 +
                             static_cast<size_t>(x + offset_x)];
                }
              }
            }
          }
        }
      }
    }
  }

  size_t num_flashes{0};

  for (size_t y{0}; y < 10; ++y) {
    for (size_t x{0}; x < 10; ++x) {
      if (has_flashed[y * 10 + x]) {
        new_grid[y * 10 + x] = 0;
        ++num_flashes;
      }
    }
  }
  return std::make_pair(new_grid, num_flashes);
}

void visualize(const std::array<int, 100>& grid) {
  for (size_t y{0}; y < 10; ++y) {
    for (size_t x{0}; x < 10; ++x) {
      std::cout << grid[y * 10 + x];
    }
    std::cout << '\n';
  }
}

auto part_one(const std::vector<std::string>& input) {
  size_t result{0};
  auto grid = prepare_input(input);
  // visualize(grid);
  for (size_t num_step{1}; num_step <= 100; ++num_step) {
    auto update = step(grid);
    result += std::get<1>(update);
    // fmt::print("\nStep: {}\n", num_step);
    grid = std::get<0>(update);
    // fmt::print("Num flashes: {}\n", std::get<1>(update));
    // visualize(grid);
  }
  return result;
}

auto part_two(const std::vector<std::string>& input) {
  size_t result{0};
  auto grid = prepare_input(input);
  size_t num_flashes{0};
  do {
    auto update = step(grid);
    grid = std::get<0>(update);
    num_flashes = std::get<1>(update);
    ++result;
  } while (num_flashes != 100);
  return result;
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1 && std::string(argv[1]) == "mock") {
    extension = "_mock";
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_11{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);
  auto answer_two = part_two(input);
  fmt::print("The answer to part two is: {}\n", answer_two);

  return 0;
}
