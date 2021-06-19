#include "../utils/input.hpp"

typedef std::pair<int, int> Point;

std::ostream& operator<<(std::ostream& os, const Point& point) {
  os << "(" << point.first << ", " << point.second << ")";
  return os;
}

enum class Direction : size_t {
  north,
  east,
  south,
  west,
};

std::ostream& operator<<(std::ostream& os, Direction direction) {
  switch (direction) {
    case Direction::north:
      os << "North";
      break;
    case Direction::east:
      os << "East";
      break;
    case Direction::south:
      os << "South";
      break;
    case Direction::west:
      os << "West";
      break;
    default:
      throw std::runtime_error("This should never happen.");
      break;
  }

  return os;
}

class Node {
 private:
  Point m_pos;
  size_t m_capacity, m_used;

 public:
  Node(Point pos, size_t capacity, size_t used)
      : m_pos(pos), m_capacity(capacity), m_used(used) {}

  size_t available() const noexcept { return this->m_capacity - this->m_used; }

  size_t capacity() const noexcept { return this->m_capacity; }

  size_t used() const noexcept { return this->m_used; }

  size_t percentage_used() const {
    return static_cast<size_t>(
        std::round(100.f * static_cast<float>(this->m_used) /
                   static_cast<float>(this->m_capacity)));
  }

  friend std::ostream& operator<<(std::ostream& os, const Node& node) noexcept {
    os << "/dev/grid/node-x" << node.m_pos.first << "-y" << node.m_pos.second
       << "\t" << node.m_capacity << "T"
       << "\t" << node.m_used << "T"
       << "\t" << node.available() << "T"
       << "\t" << node.percentage_used() << "%";

    return os;
  }
};

class Grid {
 private:
  std::map<Point, Node> m_grid;

 public:
  explicit Grid(std::map<Point, Node>&& grid) noexcept
      : m_grid(std::move(grid)) {}

  std::pair<Point, Point> border() const noexcept {
    Point min{std::numeric_limits<int>::max(), std::numeric_limits<int>::max()};
    Point max{std::numeric_limits<int>::min(), std::numeric_limits<int>::min()};

    for (auto [pos, _] : this->m_grid) {
      min.first = std::min(min.first, pos.first);
      min.second = std::min(min.second, pos.second);
      max.first = std::max(max.first, pos.first);
      max.second = std::max(max.second, pos.second);
    }

    return std::make_pair(min, max);
  }

  friend std::ostream& operator<<(std::ostream& os, const Grid& grid) {
    auto [min, max] = grid.border();
    for (int y{min.second}; y <= max.second; ++y) {
      for (int x{min.first}; x <= max.first; ++x) {
        size_t capacity{0}, used{0};
        if (grid.m_grid.count({x, y}) > 0) {
          capacity = grid.m_grid.at({x, y}).capacity();
          used = grid.m_grid.at({x, y}).used();
        }

        os << "(" << std::setw(3) << used << "T/" << std::setw(3) << capacity
           << "T)";
        if (x < max.first) {
          os << " -- ";
        }
      }
      os << "\n";
      if (y < max.second) {
        for (int x{0}; x <= (max.first - min.first); ++x) {
          if (x >= 1) {
            os << "              |";
          } else {
            os << "     |";
          }
        }
        os << "\n";
      }
    }

    return os;
  }
};

/*
 * root@ebhq-gridcenter# df -h
 * Filesystem              Size  Used  Avail  Use%
 * /dev/grid/node-x0-y0     89T   67T    22T   75%
 * /dev/grid/node-x0-y1     91T   72T    19T   79%
 */
std::map<Point, Node> prepare_input(const std::vector<std::string>& input) {
  std::map<Point, Node> nodes;
  std::regex fs_regex{
      "^/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T.*$"};
  std::smatch matches;
  for (auto line : input) {
    if (std::regex_match(line, matches, fs_regex)) {
      size_t x = std::stoi(matches[1]);
      size_t y = std::stoi(matches[2]);
      size_t capacity = std::stol(matches[3]);
      size_t used = std::stol(matches[4]);
      nodes.insert({{x, y}, Node({x, y}, capacity, used)});
    }
  }

  return nodes;
}

auto part_one(const std::vector<std::string>& input) {
  auto nodes = prepare_input(input);
  size_t viable_counter{0};
  int x_max{0}, y_max{0};

  for (auto [pos, node] : nodes) {
    x_max = std::max(x_max, pos.first);
    y_max = std::max(y_max, pos.second);
  }

  for (int y_a{0}; y_a <= y_max; y_a++) {
    for (int y_b{0}; y_b <= y_max; y_b++) {
      for (int x_a{0}; x_a <= x_max; x_a++) {
        for (int x_b{0}; x_b <= x_max; x_b++) {
          if (x_a == x_b && y_a == y_b) {
            continue;
          }
          auto a = nodes.at({x_a, y_a});
          auto b = nodes.at({x_b, y_b});
          if (a.used() > 0 && a.used() <= b.available()) {
            viable_counter++;
          }
        }
      }
    }
  }

  return viable_counter;
}

auto part_two(const std::vector<std::string>& input) {
  Grid grid(prepare_input(input));
  auto [min, max] = grid.border();
  std::cout << min << " -- " << max << std::endl;
  std::cout << grid << std::endl;

  return -1;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_22.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

