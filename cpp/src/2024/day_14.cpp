#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Robot {
private:
  Point m_position;
  Point m_velocity;

public:
  Robot(const std::string &description) {
    auto numbers = utils::extract_numbers<int>(description);
    assert(numbers.size() == 4);
    this->m_position = Point{{numbers[0], numbers[1]}};
    this->m_velocity = Point{{numbers[2], numbers[3]}};
  }

  Point position() const { return this->m_position; }

  void step(int width, int height) {
    this->m_position += this->m_velocity;
    while (this->m_position[0] < 0) {
      this->m_position[0] += width;
    }
    this->m_position[0] %= width;
    while (this->m_position[1] < 0) {
      this->m_position[1] += height;
    }
    this->m_position[1] %= height;
  }

  friend std::ostream &operator<<(std::ostream &os, const Robot &robot) {
    os << std::format("Position: ({},{}), Velocity: ({},{})",
                      robot.m_position[0], robot.m_position[1],
                      robot.m_velocity[0], robot.m_velocity[1]);
    return os;
  }
};

class Grid {
private:
  std::vector<Robot> m_robots;
  int m_width, m_height;

public:
  Grid(const std::vector<std::string> &input, int width, int height)
      : m_width(width), m_height(height) {
    for (const auto &line : input) {
      this->m_robots.push_back(Robot(line));
    }
  }

  void step() {
    for (auto &robot : this->m_robots) {
      robot.step(this->m_width, this->m_height);
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Grid &grid) {
    for (int y{0}; y < grid.m_height; ++y) {
      for (int x{0}; x < grid.m_width; ++x) {
        Point pos{{x, y}};
        int count{0};
        for (const auto &robot : grid.m_robots) {
          if (robot.position() == pos) {
            ++count;
          }
        }
        if (count == 0) {
          os << '.';
        } else {
          os << count;
        }
      }
      os << '\n';
    }

    return os;
  }

  int64_t safety_factor() const {
    Point mid_point{{this->m_width / 2, this->m_height / 2}};
    std::cout << "Mid point: " << mid_point << std::endl;
    int64_t a{0}, b{0}, c{0}, d{0};
    for (const auto &robot : this->m_robots) {
      if (robot.position()[0] < mid_point[0] &&
          robot.position()[1] < mid_point[1]) {
        ++a;
      } else if (robot.position()[0] > mid_point[0] &&
                 robot.position()[1] < mid_point[1]) {
        ++b;
      } else if (robot.position()[0] < mid_point[0] &&
                 robot.position()[1] > mid_point[1]) {
        ++c;
      } else if (robot.position()[0] > mid_point[0] &&
                 robot.position()[1] > mid_point[1]) {
        ++d;
      }
    }
    return a * b * c * d;
  }
};

auto part_one(Grid grid) {
  for (int step{1}; step <= 100; ++step) {
    grid.step();
  }
  return grid.safety_factor();
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_14_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_14.txt"};
  utils::Reader reader(input_path);
  // auto grid = Grid(reader.get_lines(), 11, 7);
  auto grid = Grid(reader.get_lines(), 101, 103);

  std::cout << std::format("The answer to part one is: {}", part_one(grid))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
