#include <cassert>
#include <map>
#include <set>

#include "../utils/input.hpp"

using Coordinate3D = std::tuple<int, int, int>;

class GameOfCubes3D {
 private:
  std::set<Coordinate3D> active_cubes;

  std::set<Coordinate3D> get_neighbors(const Coordinate3D& cube) const {
    std::set<Coordinate3D> neighbors;
    Coordinate3D neighbor;

    for (int offset_z{-1}; offset_z <= 1; offset_z++) {
      for (int offset_y{-1}; offset_y <= 1; offset_y++) {
        for (int offset_x{-1}; offset_x <= 1; offset_x++) {
          if (std::make_tuple(offset_x, offset_y, offset_z) != std::make_tuple(0, 0, 0)) {
            neighbor = {
              std::get<0>(cube) + offset_x,
              std::get<1>(cube) + offset_y,
              std::get<2>(cube) + offset_z
            };
            neighbors.insert(neighbor);
          }
        }
      }
    }
    assert(neighbors.size() == 26);

    return neighbors;
  }

  size_t num_neighbors(const Coordinate3D& cube) const {
    size_t count{0};
    auto neighbors = get_neighbors(cube);

    for (auto neighbor: neighbors) {
      if (active_cubes.count(neighbor)) {
        count++;
      }
    }
    return count;
  }

  std::map<Coordinate3D, bool> get_populated_Coordinate3Ds() const {
    std::map<Coordinate3D, bool> populated_Coordinate3Ds;
    for (auto cube: active_cubes) {
      populated_Coordinate3Ds.insert(std::make_pair(cube, true));
    }
    std::set<Coordinate3D> neighbors;
    for (auto [cube, _]: populated_Coordinate3Ds) {
      for (auto neighbor: get_neighbors(cube)) {
        neighbors.insert(neighbor);
      }
    }
    for (auto neighbor: neighbors) {
      if (populated_Coordinate3Ds.count(neighbor) == 0) {
        populated_Coordinate3Ds.insert(std::make_pair(neighbor, false));
      }
    }
    return populated_Coordinate3Ds;
  }

 public:
  explicit GameOfCubes3D(const std::vector<std::string>& input) {
    for (size_t y{0}; y < input.size(); y++) {
      for (size_t x{0}; x < input[y].size(); x++) {
        switch (input[y][x]) {
          case '#':
            active_cubes.insert({x, y, 0});
            break;
          case '.':
            break;
          default:
            throw std::invalid_argument("Invalid cube: " + std::to_string(input[y][x]));
            break;
        }
      }
    }
  }

  void step() {
    std::map<Coordinate3D, bool> updates;
    auto populated_Coordinate3Ds = get_populated_Coordinate3Ds();
    size_t count;

    for (auto [Coordinate3D, active]: populated_Coordinate3Ds) {
      count = num_neighbors(Coordinate3D);
      if (active && !(count == 2 || count == 3)) {
        updates.insert(std::make_pair(Coordinate3D, false));
      } else if (!active && count == 3) {
        updates.insert(std::make_pair(Coordinate3D, true));
      }
    }

    for (auto [Coordinate3D, active]: updates) {
      if (active) {
        active_cubes.insert(Coordinate3D);
      } else {
        active_cubes.erase(Coordinate3D);
      }
    }
  }

  bool get(const Coordinate3D& Coordinate3D) const {
    return active_cubes.count(Coordinate3D);
  }

  bool get(int x, int y, int z) const {
    Coordinate3D Coordinate3D(x, y, z);
    return get(Coordinate3D);
  }

  size_t num_active_cubes() const {
    return active_cubes.size();
  }
};

using Coordinate4D = std::tuple<int, int, int, int>;

class GameOfCubes4D {
 private:
  std::set<Coordinate4D> active_cubes;

  std::set<Coordinate4D> get_neighbors(const Coordinate4D& cube) const {
    std::set<Coordinate4D> neighbors;
    Coordinate4D neighbor;

    for (int offset_w{-1}; offset_w <= 1; offset_w++) {
      for (int offset_z{-1}; offset_z <= 1; offset_z++) {
        for (int offset_y{-1}; offset_y <= 1; offset_y++) {
          for (int offset_x{-1}; offset_x <= 1; offset_x++) {
            if (std::make_tuple(offset_x, offset_y, offset_z, offset_w) != std::make_tuple(0, 0, 0, 0)) {
              neighbor = {
                std::get<0>(cube) + offset_x,
                std::get<1>(cube) + offset_y,
                std::get<2>(cube) + offset_z,
                std::get<3>(cube) + offset_w
              };
              neighbors.insert(neighbor);
            }
          }
        }
      }
    }
    assert(neighbors.size() == 80);

    return neighbors;
  }

  size_t num_neighbors(const Coordinate4D& cube) const {
    size_t count{0};
    auto neighbors = get_neighbors(cube);

    for (auto neighbor: neighbors) {
      if (active_cubes.count(neighbor)) {
        count++;
      }
    }
    return count;
  }

  std::map<Coordinate4D, bool> get_populated_Coordinate4Ds() const {
    std::map<Coordinate4D, bool> populated_Coordinate4Ds;
    for (auto cube: active_cubes) {
      populated_Coordinate4Ds.insert(std::make_pair(cube, true));
    }
    std::set<Coordinate4D> neighbors;
    for (auto [cube, _]: populated_Coordinate4Ds) {
      for (auto neighbor: get_neighbors(cube)) {
        neighbors.insert(neighbor);
      }
    }
    for (auto neighbor: neighbors) {
      if (populated_Coordinate4Ds.count(neighbor) == 0) {
        populated_Coordinate4Ds.insert(std::make_pair(neighbor, false));
      }
    }
    return populated_Coordinate4Ds;
  }

 public:
  explicit GameOfCubes4D(const std::vector<std::string>& input) {
    for (size_t y{0}; y < input.size(); y++) {
      for (size_t x{0}; x < input[y].size(); x++) {
        switch (input[y][x]) {
          case '#':
            active_cubes.insert({x, y, 0, 0});
            break;
          case '.':
            break;
          default:
            throw std::invalid_argument("Invalid cube: " + std::to_string(input[y][x]));
            break;
        }
      }
    }
  }

  void step() {
    std::map<Coordinate4D, bool> updates;
    auto populated_Coordinate4Ds = get_populated_Coordinate4Ds();
    size_t count;

    for (auto [Coordinate4D, active]: populated_Coordinate4Ds) {
      count = num_neighbors(Coordinate4D);
      if (active && !(count == 2 || count == 3)) {
        updates.insert(std::make_pair(Coordinate4D, false));
      } else if (!active && count == 3) {
        updates.insert(std::make_pair(Coordinate4D, true));
      }
    }

    for (auto [Coordinate4D, active]: updates) {
      if (active) {
        active_cubes.insert(Coordinate4D);
      } else {
        active_cubes.erase(Coordinate4D);
      }
    }
  }

  bool get(const Coordinate4D& Coordinate4D) const {
    return active_cubes.count(Coordinate4D);
  }

  bool get(int x, int y, int z, int w) const {
    Coordinate4D Coordinate4D(x, y, z, w);
    return get(Coordinate4D);
  }

  size_t num_active_cubes() const {
    return active_cubes.size();
  }
};

int part_one(const std::vector<std::string>& input) {
  GameOfCubes3D game(input);
  // std::cout << game.num_active_cubes() << std::endl;
  for (size_t index{0}; index < 6; index++) {
    game.step();
    // std::cout << game.num_active_cubes() << std::endl;
  }
  return game.num_active_cubes();
}

int part_two(const std::vector<std::string>& input) {
  GameOfCubes4D game(input);
  // std::cout << game.num_active_cubes() << std::endl;
  for (size_t index{0}; index < 6; index++) {
    game.step();
    // std::cout << game.num_active_cubes() << std::endl;
  }
  return game.num_active_cubes();
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_17.txt"));
  const std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

