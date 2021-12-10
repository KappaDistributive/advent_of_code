#include <cassert>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using size_t = std::size_t;
using Position = utils::geometry::Point<size_t, 2>;

class Maze {
 private:
  std::map<Position, size_t> m_locations;
  std::vector<bool> m_maze;
  size_t m_width;
  size_t m_height;

 public:
  explicit Maze(const std::vector<std::string>& input) {
    assert(input.size() > 0);
    this->m_height = input.size();
    this->m_width = input[0].size();

    size_t x{0}, y{0};
    for (auto line : input) {
      x = 0;
      assert(line.size() == this->m_width);
      for (auto position : line) {
        this->m_maze.push_back(position == '#' ? false : true);
        if ('0' <= position && position <= '9') {
          this->m_locations.insert(
              std::make_pair(Position{std::array<size_t, 2>{x, y}},
                             static_cast<size_t>(position - '0')));
        }
        ++x;
      }
      ++y;
    }
  }

  std::vector<Position> shortest_path(Position origin,
                                      Position destination) const;

  char operator[](Position position) const {
    auto coordinates = position.coordinates();
    size_t x{coordinates[0]}, y{coordinates[1]};
    if (this->m_locations.count(position) > 0) {
      return '0' + this->m_locations.at(position);
    }

    if (y * this->m_width + x < this->m_maze.size()) {
      return this->m_maze[y * this->m_width + x] ? '.' : '#';
    }
    return '#';
  }

  friend std::ostream& operator<<(std::ostream& os, const Maze& maze) {
    for (size_t y{0}; y < maze.m_height; ++y) {
      for (size_t x{0}; x < maze.m_width; ++x) {
        os << maze[Position{std::array<size_t, 2>{x, y}}];
      }
      os << '\n';
    }

    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  for (auto line : input) {
    std::cout << line << std::endl;
  }
  Maze maze{input};
  std::cout << std::endl;
  std::cout << maze << std::endl;
  return 0;
}

// auto part_two(const std::vector<std::string>& input) {
//   return 1;
// }

int main() {
  // std::filesystem::path input_path{"../../data/2016/input_24_mock.txt"};
  std::filesystem::path input_path{"../../data/2016/input_24.txt"};
  utils::Reader reader{input_path};
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

