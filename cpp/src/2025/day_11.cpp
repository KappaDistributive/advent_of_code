#include "../utils/input.hpp"

std::map<std::string, std::vector<std::string>>
parse_input(const std::vector<std::string> &data) {
  std::map<std::string, std::vector<std::string>> parsed_data;
  for (const auto &line : data) {
    auto splits = utils::split_string(line, ' ');
    parsed_data[splits[0].substr(0, splits[0].size() - 1)] =
        std::vector<std::string>(splits.begin() + 1, splits.end());
  }
  parsed_data["out"] = {};
  return parsed_data;
}

std::map<std::string, int64_t>
num_paths(const std::string dest,
          const std::map<std::string, std::vector<std::string>> &graph) {
  std::map<std::string, int64_t> paths;
  std::map<std::string, int64_t> new_paths;
  for (const auto &[node, _] : graph) {
    new_paths[node] = node == dest ? 1 : 0;
  }

  while (paths != new_paths) {
    paths = new_paths;
    new_paths.clear();
    for (const auto &[node, neighbors] : graph) {
      if (node == dest) {
        new_paths[node] = 1;
      } else {
        int64_t total_paths = 0;
        for (const auto &neighbor : neighbors) {
          total_paths += paths[neighbor];
        }
        new_paths[node] = total_paths;
      }
    }
  }
  return paths;
}

auto part_one(const std::map<std::string, std::vector<std::string>> &graph) {
  return num_paths("out", graph)["you"];
}

auto part_two(const std::map<std::string, std::vector<std::string>> &graph) {
  auto dac = num_paths("dac", graph);
  auto fft = num_paths("fft", graph);
  auto out = num_paths("out", graph);
  return dac["svr"] * fft["dac"] * out["fft"] +
         fft["svr"] * dac["fft"] * out["dac"];
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_11_mock2.txt"};
  std::filesystem::path input_path{"../../data/2025/input_11.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
