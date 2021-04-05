#include <algorithm>
#include <cassert>
#include <map>
#include <regex>  // NOLINT
#include <set>

#include "../utils/input.hpp"

class Map {
 private:
  std::regex re;
  std::vector<std::string> cities;
  std::vector<int> adjacency_matrix;

 public:
  std::smatch matches;
  explicit Map(const std::vector<std::string>& input)
      : re("^(\\w+)\\sto\\s(\\w+)\\s=\\s(\\d+)$") {
    // temporary distance storage
    std::map<std::pair<std::string, std::string>, int> distances;
    for (auto line : input) {
      std::regex_match(line,  matches, re);
      assert(matches.size() == 4);
      std::string start{matches[1].str()};
      std::string destination{matches[2].str()};
      int distance{std::stoi(matches[3].str())};

      if (std::find(cities.begin(), cities.end(), start) == cities.end()) {
        cities.push_back(start);
      }
      if (std::find(cities.begin(),
          cities.end(),
          destination) == cities.end()) {
        cities.push_back(destination);
      }
      distances.insert(
        std::make_pair(std::make_pair(start, destination), distance));
    }

    adjacency_matrix.reserve(cities.size() * cities.size());
    for (size_t j{0}; j < cities.size(); j++) {
      for (size_t i{0}; i < cities.size(); i++) {
        try {
          auto distance = distances.at(std::make_pair(cities[i], cities[j]));
          adjacency_matrix[j * cities.size() + i] = distance;
          adjacency_matrix[i * cities.size() + j] = distance;
        }
        catch (const std::out_of_range& e) {
          adjacency_matrix[j * cities.size() + i] = -1;
        }
      }
    }
  }

  std::vector<std::string> get_cities() const {
    return cities;
  }

  int get_distance(std::string start, std::string destination) const {
    try {
      int i = std::find(cities.begin(),
                        cities.end(),
                        start) - cities.begin();
      int j = std::find(cities.begin(),
                        cities.end(),
                        destination) - cities.begin();
      return adjacency_matrix[j * cities.size() + i];
    }
    catch (const std::out_of_range& e) {
      return -1;
    }
  }

  std::vector<std::string> hamiltonian_path(bool shortest = true) {
    std::vector<int> permutation;
    int distance{-1};
    int candidate;
    int next_distance;
    bool valid;

    std::vector<int> indices;
    for (size_t index{0}; index < cities.size(); index++) {
      indices.push_back(index);
    }

    do {
      candidate = 0;
      valid = true;
      for (size_t index{1}; index < indices.size(); index++) {
        next_distance = get_distance(cities[indices[index-1]],
                                     cities[indices[index]]);
        if (next_distance < 0) {
          valid = false;
          break;
        }
        candidate += next_distance;
      }
      if (valid && (distance < 0 ||
                    (shortest && candidate < distance) ||
                    (!shortest && candidate > distance))) {
        distance = candidate;
        permutation = indices;
      }
    } while (std::next_permutation(indices.begin(), indices.end()));

    if (distance >= 0) {
      std::vector<std::string> path;
      for (auto index : permutation) {
        path.push_back(cities[index]);
      }
      return path;
    } else {
      return std::vector<std::string>{};
    }
  }
};


std::ostream& operator<< (std::ostream& os, const Map& map) {
  auto cities = map.get_cities();
  for (auto start : cities) {
    for (auto destination : cities) {
      auto distance = map.get_distance(start, destination);
      if (distance >= 0) {
        std::cout << start << " -- "
                  << distance << " --> "
                  << destination << std::endl;
      }
    }
  }
  return os;
}


int part_one(const std::vector<std::string>& input) {
  Map map(input);
  int result{0};
  auto path = map.hamiltonian_path();
  for (size_t index{1}; index < path.size(); index++) {
    result += map.get_distance(path[index-1], path[index]);
  }
  return result;
}


int part_two(const std::vector<std::string>& input) {
  Map map(input);
  int result{0};
  auto path = map.hamiltonian_path(false);
  for (size_t index{1}; index < path.size(); index++) {
    result += map.get_distance(path[index-1], path[index]);
  }
  return result;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_09.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
