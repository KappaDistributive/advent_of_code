#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Game {
private:
  const Point m_a;
  const Point m_b;
  const Point m_prize;
  const int m_cost_a{3};
  const int m_cost_b{1};

public:
  Game(Point a, Point b, Point prize) : m_a(a), m_b(b), m_prize(prize) {}

  Point solve() const {
    int minimal_cost{std::numeric_limits<int>::max()};
    Point solution;
    for (int a{0}; a <= 100; ++a) {
      for (int b{0}; b <= 100; ++b) {
        Point attempt{{a, b}};
        if (a * this->m_a[0] + b * this->m_b[0] == this->m_prize[0] &&
            a * this->m_a[1] + b * this->m_b[1] == this->m_prize[1]) {
          int cost = this->cost(attempt);
          if (cost < minimal_cost) {
            minimal_cost = cost;
            solution = attempt;
          }
        }
      }
    }
    return solution;
  }

  int cost(Point moves) const {
    return moves[0] * this->m_cost_a + moves[1] * this->m_cost_b;
  }

  friend std::ostream &operator<<(std::ostream &os, const Game &game) {
    os << std::format("A: ({},{}), B: ({},{}), Prize: ({},{})", game.m_a[0],
                      game.m_a[1], game.m_b[0], game.m_b[1], game.m_prize[0],
                      game.m_prize[1]);
    return os;
  }
};

std::vector<Game> parse(const std::vector<std::string> &input) {
  std::vector<Game> result;
  std::array<Point, 3> data;
  for (size_t index{0}; index < input.size(); ++index) {
    auto splits = utils::split_string(input[index], ' ');
    switch (index % 4) {
    case 0:
      data[0] = Point{{std::stoi(splits[2].substr(2, splits[1].size() - 4)),
                       std::stoi(splits[3].substr(2))}};
      break;
    case 1:
      data[1] = Point{{std::stoi(splits[2].substr(2, splits[1].size() - 4)),
                       std::stoi(splits[3].substr(2))}};
      break;
    case 2:
      data[2] = Point{{std::stoi(splits[1].substr(2, splits[1].size() - 3)),
                       std::stoi(splits[2].substr(2))}};
      break;
    case 3:
      result.push_back(Game(data[0], data[1], data[2]));
      break;
    }
  }
  result.push_back(Game(data[0], data[1], data[2]));
  return result;
}

auto part_one(const std::vector<Game> &games) {
  int64_t result{0};
  for (const auto &game : games) {
    auto solution = game.solve();
    result += game.cost(solution);
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_13_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_13.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
