#include "../utils/input.hpp"

class Map {
 private:
  const bool m_allow_revisit;
  std::set<std::pair<std::string, std::string>> m_adjacency_list;

 public:
  explicit Map(const std::vector<std::string>& input,
               bool allow_revisit = false)
      : m_allow_revisit(allow_revisit) {
    for (auto line : input) {
      auto splits = utils::split_string(line, '-');
      if (splits[0] != "end" && splits[1] != "start") {
        this->m_adjacency_list.insert(std::make_pair(splits[0], splits[1]));
      }
      if (splits[0] != "start" && splits[1] != "end") {
        this->m_adjacency_list.insert(
            std::make_pair(splits[1], splits[0]));  // undirected graph
      }
    }
  }

  bool may_visit(const std::vector<std::string>& path,
                 const std::string& node) const {
    if (std::isupper(node[0])) {
      return true;
    }

    std::map<std::string, size_t> counts;
    size_t max_visits{1};
    for (auto position : path) {
      if (std::isupper(position[0])) {
        continue;
      } else if (counts.count(position) == 0) {
        counts.insert(std::make_pair(position, 1));
      } else {
        ++counts.at(position);
      }
      max_visits = std::max(max_visits, counts.at(position));
    }

    if (max_visits > 1) {
      return counts.count(node) == 0;
    }

    return counts.count(node) == 0 ||
           (this->m_allow_revisit && counts.at(node) == 1);
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
          if (s == path.back() && this->may_visit(path, d)) {
            auto new_path = path;
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

      std::cout << final_paths.size() << std::endl;
    }

    return final_paths;
  }

  friend std::ostream& operator<<(std::ostream& os, const Map& map) {
    for (const auto& [source, destination] : map.m_adjacency_list) {
      os << std::format("{} -> {}\n", source, destination);
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

auto part_two(const std::vector<std::string>& input) {
  Map map(input, true);
  // std::cout << map << std::endl;
  auto all_paths = map.paths("start", "end");

  return all_paths.size();
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      std::format("../../data/2021/input_12{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
