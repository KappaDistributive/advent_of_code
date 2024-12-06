#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Pipe {
private:
  const char m_type;

public:
  Pipe(char type) : m_type{type} {}
  bool accessible(const Point &direction) const {
    if (direction == Point{{0, -1}}) { // north
      return this->m_type == '|' || this->m_type == '7' || this->m_type == 'F';
    } else if (direction == Point{{1, 0}}) { // east
      return this->m_type == '-' || this->m_type == 'J' || this->m_type == '7';
    } else if (direction == Point{{0, 1}}) { // south
      return this->m_type == '|' || this->m_type == 'J' || this->m_type == 'L';
    } else if (direction == Point{{-1, 0}}) { // west
      return this->m_type == '-' || this->m_type == 'F' || this->m_type == 'L';
    }
    return false;
  }
};

class Maze {
private:
  const std::vector<std::string> m_data;
  size_t m_width, m_height;
  Point m_start;
  Point m_position;
  char m_start_symbol;
  Point m_direction;
  std::vector<Point> m_loop;

  char infer_start_symbol() {
    std::vector<Pipe> neighbors{
        Pipe{this->at(this->m_start + Point{{0, -1}})}, // 0: north
        Pipe{this->at(this->m_start + Point{{1, 0}})},  // 1: east
        Pipe{this->at(this->m_start + Point{{0, 1}})},  // 2: south
        Pipe{this->at(this->m_start + Point{{-1, 0}})}, // 3: west
    };

    if (neighbors[0].accessible(Point{{0, -1}}) &&
        neighbors[2].accessible(Point{{0, 1}})) {
      return '|';
    } else if (neighbors[1].accessible(Point{{1, 0}}) &&
               neighbors[3].accessible(Point{{-1, 0}})) {
      return '-';
    } else if (neighbors[0].accessible(Point{{0, -1}}) &&
               neighbors[1].accessible(Point{{1, 0}})) {
      return 'L';
    } else if (neighbors[3].accessible(Point{{-1, 0}}) &&
               neighbors[0].accessible(Point{{0, -1}})) {
      return 'J';
    } else if (neighbors[2].accessible(Point{{0, 1}}) &&
               neighbors[1].accessible(Point{{1, 0}})) {
      return 'F';
    } else if (neighbors[3].accessible(Point{{-1, 0}}) &&
               neighbors[2].accessible(Point{{0, 1}})) {
      return '7';
    }
    assert(false);
    return ' ';
  }

  std::array<Point, 2> infer_direction(const char symbol) {
    switch (symbol) {
    case '|':
      return {Point{{0, -1}}, Point{{0, 1}}};
      break;
    case '-':
      return {Point{{1, 0}}, Point{{-1, 0}}};
      break;
    case 'L':
      return {Point{{0, -1}}, Point{{1, 0}}};
      break;
    case 'J':
      return {Point{{0, -1}}, Point{{-1, 0}}};
      break;
    case '7':
      return {Point{{0, 1}}, Point{{-1, 0}}};
      break;
    case 'F':
      return {Point{{1, 0}}, Point{{0, 1}}};
      break;
    }
    assert(false);
    return {Point{{0, 0}}, Point{{0, 0}}};
  }

public:
  Maze(const std::vector<std::string> &input) : m_data{input} {
    this->m_height = input.size();
    assert(this->m_height > 0);
    this->m_width = input[0].size();
    for (size_t y{0}; y < this->m_height; ++y) {
      for (size_t x{0}; x < this->m_width; ++x) {
        if (this->m_data[y][x] == 'S') {
          this->m_start = Point{{static_cast<int>(x), static_cast<int>(y)}};
        }
      }
    }

    this->m_start_symbol = this->infer_start_symbol();
    this->m_position = this->m_start;
    this->m_direction = this->infer_direction(this->m_start_symbol)[0];
    std::cout << "Start symbol: " << this->m_start_symbol << std::endl;
    std::cout << "Direction: " << this->m_direction << std::endl;
  }

  char at(const Point &position) const {
    if (position == this->m_start) {
      return this->m_start_symbol;
    }
    if (position[0] < 0 || position[0] >= static_cast<int>(this->m_width) ||
        position[1] < 0 || position[1] >= static_cast<int>(this->m_height)) {
      return ' ';
    }
    return this->m_data[position[1]][position[0]];
  }

  void step() {
    Point next = this->m_position + this->m_direction;
    std::cout << "Inferring direction for: " << this->at(next) << std::endl;
    auto possible_directions = this->infer_direction(this->at(next));
    if (possible_directions[0] == -this->m_direction) {
      this->m_direction = possible_directions[1];
    } else {
      this->m_direction = possible_directions[0];
    }
    this->m_position = next;
  }

  Point start_position() const { return this->m_start; }

  Point position() const { return this->m_position; }

  friend std::ostream &operator<<(std::ostream &os, const Maze &maze) {
    for (size_t y{0}; y < maze.m_height; ++y) {
      for (size_t x{0}; x < maze.m_width; ++x) {
        Point pos{{static_cast<int>(x), static_cast<int>(y)}};
        if (pos == maze.m_position) {
          os << "\033[3m";
        }
        if (pos == maze.start_position()) {
          os << "\033[1;96m";
        }
        os << maze.at(pos) << "\033[0m";
      }
      os << std::endl;
    }
    return os;
  }
};

std::vector<Point> trace(Maze maze) {
  std::vector<Point> loop{};
  do {
    loop.push_back(maze.position());
    maze.step();
  } while (maze.position() != maze.start_position());
  return loop;
}

auto part_one(const std::vector<std::string> &input) {
  Maze maze(input);
  auto loop = trace(maze);
  return loop.size() / 2;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_10_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_10.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
