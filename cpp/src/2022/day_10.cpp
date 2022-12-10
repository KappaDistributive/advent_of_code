#include "../utils/input.hpp"

auto registers(const std::vector<std::string> &input) {
  std::vector<int> xs;
  xs.push_back(1);
  for (const auto &line : input) {
    xs.push_back(xs.back());
    fmt::print("{}\t{}\t{}\n", xs.size(), line, xs.back());
    if (line.starts_with("addx")) {
      auto splits = utils::split_string(line, ' ');
      assert(splits.size() == 2);
      assert(splits[0] == "addx");
      xs.push_back(xs.back() + std::stoi(splits[1]));
      fmt::print("{}\t{}\t{}\n", xs.size(), "", xs.back());
    }
  }
  return xs;
}

auto part_one(const std::vector<std::string> &input) {
  auto xs = registers(input);
  int result{0};
  for (auto index : std::vector<int>{{20, 60, 100, 140, 180, 220}}) {
    fmt::print("{} {} {}\n", index, xs[index], index * xs[index]);
    result += index * xs[index - 1];
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) {
  std::string result;
  auto xs = registers(input);
  for (size_t index{0}; index < 240; ++index) {
    int x = index % 40;
    int sprite_pos = xs[index];
    result.push_back((sprite_pos - 1 <= x && x <= sprite_pos + 1) ? '#' : '.');
    if ((index + 1) % 40 == 0) {
      result.push_back('\n');
    }
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_10_mock2.txt"};
  std::filesystem::path input_path{"../../data/2022/input_10.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is:\n{}\n", part_two(input));

  return 0;
}
