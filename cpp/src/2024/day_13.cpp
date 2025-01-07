#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int64_t, 2>;

class Game {
private:
  const Point m_a;
  const Point m_b;
  Point m_prize;
  const int m_cost_a{3};
  const int m_cost_b{1};

public:
  Game(Point a, Point b, Point prize) : m_a(a), m_b(b), m_prize(prize) {}

  Point solve() const {
    // Gauss elimination
    int64_t a{0}, b{0};
    b = (this->m_prize[1] * this->m_a[0] - this->m_prize[0] * this->m_a[1]) /
        (this->m_b[1] * this->m_a[0] - this->m_b[0] * this->m_a[1]);
    a = (this->m_prize[0] - b * this->m_b[0]) / this->m_a[0];

    if (a * this->m_a[0] + b * this->m_b[0] == this->m_prize[0] &&
        a * this->m_a[1] + b * this->m_b[1] == this->m_prize[1]) {
      return Point{{a, b}};
    }
    return Point();
  }

  int64_t cost(Point moves) const {
    return moves[0] * this->m_cost_a + moves[1] * this->m_cost_b;
  }

  void part_two() { this->m_prize += Point{{10000000000000, 10000000000000}}; }

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

auto part_two(const std::vector<Game> &games) {
  int64_t result{0};
  for (auto game : games) {
    game.part_two();
    auto solution = game.solve();
    result += game.cost(solution);
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_13_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_13.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
