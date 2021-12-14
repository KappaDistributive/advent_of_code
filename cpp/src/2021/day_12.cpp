#include "../utils/input.hpp"

class Map {
 private:
  const size_t m_curiosity;
  std::set<std::pair<std::string, std::string>> m_adjacency_list;

 public:
  explicit Map(const std::vector<std::string>& input, size_t curiosity = 1)
      : m_curiosity(curiosity) {
    for (auto line : input) {
      auto splits = utils::split_string(line, '-');
      this->m_adjacency_list.insert(std::make_pair(splits[0], splits[1]));
      this->m_adjacency_list.insert(
          std::make_pair(splits[1], splits[0]));  // undirected graph
    }
  }

  bool may_visit(const std::vector<std::string>& path,
                 const std::string& node) const {
    if (std::isupper(node[0])) {
      return true;
    }
    size_t num_visits{0};
    for (auto position : path) {
      if (position == node) {
        ++num_visits;
      }
    }
    return num_visits < this->m_curiosity;
  }

  std::set<std::vector<std::string>> paths(std::string source,
                                           std::string destination) const {
    std::set<std::vector<std::string>> final_paths;
    std::set<std::vector<std::string>> current_paths{{source}};
    bool searching{true};

    while (searching) {
      searching = false;
      std::set<std::vector<std::string>> new_paths;

      for (auto path : current_paths) {
        for (auto [s, d] : this->m_adjacency_list) {
          auto new_path = path;
          if (s == path.back() && this->may_visit(path, d)) {
            new_path.push_back(d);
            if (current_paths.count(new_path) == 0) {
              searching = true;
            }
            new_paths.insert(new_path);
          }
        }
      }

      current_paths = new_paths;

      for (const auto& path : current_paths) {
        if (path.back() == destination) {
          if (final_paths.count(path) == 0) {
            final_paths.insert(path);
          }
        }
      }
    }

    return final_paths;
  }

  friend std::ostream& operator<<(std::ostream& os, const Map& map) {
    for (const auto& [source, destination] : map.m_adjacency_list) {
      os << fmt::format("{} -> {}\n", source, destination);
    }
    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  Map map(input);
  // std::cout << map << std::endl;

  auto all_paths = map.paths("start", "end");

  return all_paths.size();
}

// auto part_two(const std::vector<std::string>& input) {
//   return 2;
// }

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1 && std::string(argv[1]) == "mock") {
    extension = "_mock";
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_12{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);
  // auto answer_two = part_two(input);
  // fmt::print("The answer to part two is: {}\n", answer_two);

  return 0;
}
