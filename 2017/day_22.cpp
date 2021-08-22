#include <map>

#include "../utils/input.hpp"

#define assertm(exp, msg) assert(((void)msg, exp))

typedef std::pair<int64_t, int64_t> coordinate;

enum class Direction : int { up, right, down, left };

std::ostream& operator<<(std::ostream& os, coordinate position) {
  os << '(' << position.first << ", " << position.second << ')';
  return os;
}

class Grid {
 private:
  std::map<coordinate, char> m_grid;
  coordinate m_carrier;
  Direction m_direction;
  size_t m_total_infections;
  bool m_resistant;

  auto border() const {
    coordinate top_left{0, 0}, bottom_right{0, 0};

    for (auto [key, _] : this->m_grid) {
      top_left.first = std::min(top_left.first, key.first);
      top_left.second = std::min(top_left.second, key.second);
      bottom_right.first = std::max(bottom_right.first, key.first);
      bottom_right.second = std::max(bottom_right.second, key.second);
    }

    --top_left.first;
    --top_left.second;
    ++bottom_right.first;
    ++bottom_right.second;

    return std::make_pair(top_left, bottom_right);
  }

  void turn(bool clockwise) {
    if (clockwise) {
      switch (m_direction) {
        case Direction::up:
          m_direction = Direction::right;
          break;
        case Direction::right:
          m_direction = Direction::down;
          break;
        case Direction::down:
          m_direction = Direction::left;
          break;
        case Direction::left:
          m_direction = Direction::up;
          break;
      }
    } else {
      switch (m_direction) {
        case Direction::up:
          m_direction = Direction::left;
          break;
        case Direction::right:
          m_direction = Direction::up;
          break;
        case Direction::down:
          m_direction = Direction::right;
          break;
        case Direction::left:
          m_direction = Direction::down;
          break;
      }
    }
  }

 public:
  explicit Grid(const std::vector<std::string>& input, bool resistant = false)
      : m_direction(Direction::up),
        m_total_infections(0),
        m_resistant(resistant) {
    for (size_t y{0}; y < input.size(); ++y) {
      for (size_t x{0}; x < input[y].size(); ++x) {
        this->m_grid.insert(std::make_pair(std::make_pair(x, y), input[y][x]));
      }
    }

    this->m_carrier.second = input.size() / 2;
    this->m_carrier.first = input[this->m_carrier.second].size() / 2;
  }

  char operator[](coordinate position) const {
    if (this->m_grid.count(position) > 0) {
      return this->m_grid.at(position);
    }
    return '.';
  }

  void step() {
    turn(this->operator[](this->m_carrier) == '#');

    if (this->operator[](this->m_carrier) == '#') {
      this->m_grid.insert_or_assign(this->m_carrier, '.');
    } else {
      ++this->m_total_infections;
      this->m_grid.insert_or_assign(this->m_carrier, '#');
    }

    switch (this->m_direction) {
      case Direction::up:
        --this->m_carrier.second;
        break;
      case Direction::right:
        ++this->m_carrier.first;
        break;
      case Direction::down:
        ++this->m_carrier.second;
        break;
      case Direction::left:
        --this->m_carrier.first;
        break;
    }
  }

  size_t total_infections() const { return this->m_total_infections; }

  friend std::ostream& operator<<(std::ostream& os, const Grid& grid) {
    auto [top_left, bottom_right] = grid.border();

    for (int64_t y{top_left.second}; y <= bottom_right.second; ++y) {
      for (int64_t x{top_left.first}; x <= bottom_right.first; ++x) {
        os << grid[std::make_pair(x, y)];
        if (y == grid.m_carrier.second && x + 1 == grid.m_carrier.first) {
          os << '[';
        } else if (y == grid.m_carrier.second && x == grid.m_carrier.first) {
          os << ']';
        } else if (x < bottom_right.first) {
          os << ' ';
        }
      }
      os << '\n';
    }

    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  Grid grid(input);
  for (size_t step{0}; step < 10000; ++step) {
    grid.step();
    // std::cout << grid << std::endl;
  }
  std::cout << grid << std::endl;
  return grid.total_infections();
}

auto part_two(const std::vector<std::string>& input) { return -2; }

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_22.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
}

