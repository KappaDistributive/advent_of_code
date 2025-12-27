#include "../utils/input.hpp"

std::map<std::string, std::vector<std::string>>
parse_input(const std::vector<std::string> &data) {
  std::map<std::string, std::vector<std::string>> parsed_data;
  for (const auto &line : data) {
    auto splits = utils::split_string(line, ' ');
    parsed_data[splits[0].substr(0, splits[0].size() - 1)] =
        std::vector<std::string>(splits.begin() + 1, splits.end());
  }
  return parsed_data;
}

auto part_one(const std::map<std::string, std::vector<std::string>> &graph) {
  size_t result{0};
  std::set<std::vector<std::string>> paths{{{"you"}}};
  while (!paths.empty()) {
    std::set<std::vector<std::string>> next_paths;
    for (const auto &path : paths) {
      const auto &node = path.back();
      if (!graph.contains(node)) {
        continue;
      }
      for (const auto &neighbor : graph.at(node)) {
        if (neighbor == "out") {
          ++result;
        } else {
          auto new_path = path;
          new_path.push_back(neighbor);
          next_paths.insert(new_path);
        }
      }
    }
    paths = next_paths;
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_11_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_11.txt"};
  utils::Reader reader(input_path);
  auto data = parse_input(reader.get_lines());
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
