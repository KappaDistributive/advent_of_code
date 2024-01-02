#include "../utils/input.hpp"

auto extract_rules(const std::vector<std::string> &input,
                   const std::string &kind) {
  size_t index{0};
  while (!input[index].starts_with(kind)) {
    ++index;
  }
  std::vector<std::tuple<int64_t, int64_t, int64_t>> map;
  while (input[++index] != "") {
    auto splits = utils::split_string(input[index], ' ');
    assert(splits.size() == 3);
    map.push_back(std::make_tuple<int64_t, int64_t, int64_t>(
        std::stoull(splits[0]), std::stoull(splits[1]),
        std::stoull(splits[2])));
  }
  assert(map.size() > 0);
  return map;
}

auto extract_seeds(const std::vector<std::string> &input) {
  auto splits = utils::split_string(input[0], ' ');
  std::vector<int64_t> seeds;
  for (size_t index{1}; index < splits.size(); ++index) {
    seeds.push_back(std::stoull(splits[index]));
  }
  return seeds;
}

auto parse(const std::vector<std::string> &input) {
  auto seeds = extract_seeds(input);
  for (auto seed : seeds) {
    std::cout << seed << std::endl;
  }
  std::vector<std::vector<std::tuple<int64_t, int64_t, int64_t>>> maps;
  maps.push_back(extract_rules(input, "seed-to-soil"));
  maps.push_back(extract_rules(input, "soil-to-fertilizer"));
  maps.push_back(extract_rules(input, "fertilizer-to-water"));
  maps.push_back(extract_rules(input, "water-to-light"));
  maps.push_back(extract_rules(input, "light-to-temperature"));
  maps.push_back(extract_rules(input, "temperature-to-humidity"));
  maps.push_back(extract_rules(input, "humidity-to-location"));

  for (const auto &map : maps) {
    for (const auto &entry : map) {
      fmt::print("{} {} {}\n", std::get<0>(entry), std::get<1>(entry),
                 std::get<2>(entry));
    }
    fmt::print("\n");
  }

  return std::make_pair(seeds, maps);
}

int64_t
transform(int64_t value,
          const std::vector<std::tuple<int64_t, int64_t, int64_t>> &map) {
  for (auto [dest, src, range] : map) {
    if (value >= src && value < src + range) {
      return dest + (value - src);
    }
  }
  return value;
}

auto part_one(
    const std::vector<int64_t> &seeds,
    const std::vector<std::vector<std::tuple<int64_t, int64_t, int64_t>>>
        maps) {
  int64_t answer{std::numeric_limits<int64_t>::max()};
  for (auto value : seeds) {
    for (const auto &map : maps) {
      value = transform(value, map);
    }
    if (value < answer)
      answer = value;
  }
  return answer;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_05_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_05.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  fmt::print("The answer to part one is: {}\n",
             part_one(input.first, input.second));
  fmt::print("The answer to part two is: {}\n", part_two());

  return 0;
}
