#include "../../includes/md5.hpp"
#include "../utils/input.hpp"
#include "../utils/geometry.hpp"

using utils::geometry::Direction;
using utils::geometry::ALL_DIRECTIONS;

class Path {
 private:
  std::string open_door_codes;
  std::string passcode;
  std::vector<Direction> path;
  std::pair<size_t, size_t> position;

 public:
  Path() = default;

  explicit Path(const std::string& passcode)
      : open_door_codes("bcdef"),
        passcode(passcode),
        path(),
        position({0, 0}) {}

  explicit Path(const Path& other)
      : open_door_codes(other.open_door_codes),
        passcode(other.passcode),
        path(other.path),
        position(other.position) {}

  Path& operator=(const Path& other) {
    this->open_door_codes = other.open_door_codes;
    this->passcode = other.passcode;
    this->path = other.path;
    this->position = other.position;

    return *this;
  }

  std::pair<size_t, size_t> get_position() const { return this->position; }

  std::string path_to_string(const std::vector<Direction>& path,
                             size_t offset = 0) const {
    if (offset >= path.size()) {
      return "";
    }
    std::string path_str;
    switch (path[offset]) {
      case Direction::Up:
        path_str += 'U';
        break;
      case Direction::Down:
        path_str += 'D';
        break;
      case Direction::Left:
        path_str += 'L';
        break;
      case Direction::Right:
        path_str += 'R';
        break;
    }
    return path_str + path_to_string(path, ++offset);
  }

  std::string path_to_string() const { return path_to_string(this->path); }

  bool valid_move(const Direction& direction) {
    if ((this->position.first == 0 && direction == Direction::Left) ||
        (this->position.second == 0 && direction == Direction::Up) ||
        (this->position.first == 3 && direction == Direction::Right) ||
        (this->position.second == 3 && direction == Direction::Down)) {
      return false;
    }

    auto potential_path = this->path;
    auto path_str = path_to_string(potential_path);
    bool open_door;

    switch (direction) {
      case Direction::Up:
        open_door =
            open_door_codes.find(md5(this->passcode + path_to_string())[0]) !=
            std::string::npos;
        break;
      case Direction::Down:
        open_door =
            open_door_codes.find(md5(this->passcode + path_to_string())[1]) !=
            std::string::npos;
        break;
      case Direction::Left:
        open_door =
            open_door_codes.find(md5(this->passcode + path_to_string())[2]) !=
            std::string::npos;
        break;
      case Direction::Right:
        open_door =
            open_door_codes.find(md5(this->passcode + path_to_string())[3]) !=
            std::string::npos;
        break;
    }

    return open_door;
  }

  bool step(const Direction& direction) {
    if (!valid_move(direction)) {
      return false;
    }
    this->path.push_back(direction);
    switch (direction) {
      case Direction::Up:
        this->position.second--;
        break;
      case Direction::Down:
        this->position.second++;
        break;
      case Direction::Right:
        this->position.first++;
        break;
      case Direction::Left:
        this->position.first--;
        break;
    }

    return true;
  }
};

std::string part_one(const std::string& input) {
  std::string shortest_path;
  std::vector<Path> paths{{Path(input)}};
  bool searching{true};
  bool stuck{false};
  std::vector<Path> new_paths;
  Path path{paths[0]};
  while (searching) {
    new_paths = {};
    stuck = true;
    for (auto& path : paths) {
      if (searching) {
        for (auto direction : ALL_DIRECTIONS) {
          if (path.valid_move(direction)) {
            stuck = false;
            Path new_path(path);
            new_path.step(direction);
            new_paths.push_back(new_path);
            if (new_path.get_position() == std::pair<size_t, size_t>({3, 3})) {
              assert(shortest_path == "");
              shortest_path = new_path.path_to_string();
              searching = false;
              break;
            }
          }
        }
      }
    }
    if (stuck) {
      throw std::runtime_error("I am stuck!");
    }
    paths = new_paths;
  }
  return shortest_path;
}

size_t part_two(const std::string& input) {
  std::string longest_path;
  std::vector<Path> paths{{Path(input)}};
  bool searching{true};
  bool stuck{false};
  std::vector<Path> new_paths;
  std::vector<Path> finished_paths;
  while (searching) {
    new_paths = {};
    stuck = true;
    for (auto& path : paths) {
      if (searching) {
        for (auto& direction : ALL_DIRECTIONS) {
          if (path.valid_move(direction)) {
            stuck = false;
            Path new_path(path);
            new_path.step(direction);
            if (new_path.get_position() == std::pair<size_t, size_t>({3, 3})) {
              finished_paths.push_back(new_path);
            } else {
              new_paths.push_back(new_path);
            }
          }
        }
      }
    }
    if (stuck) {
      searching = false;
    }
    paths = new_paths;
  }

  for (auto& path : finished_paths) {
    if (path.path_to_string().size() > longest_path.size()) {
      longest_path = path.path_to_string();
    }
  }
  return longest_path.size();
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2016/input_17.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

