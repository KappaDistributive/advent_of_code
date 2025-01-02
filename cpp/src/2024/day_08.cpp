#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Map {
private:
  int m_width, m_height;
  std::map<Point, char> m_antennas;

public:
  Map(const std::vector<std::string> &input)
      : m_width{static_cast<int>(input[0].size())},
        m_height{static_cast<int>(input.size())} {
    for (int y{0}; y < m_height; ++y) {
      for (int x{0}; x < m_width; ++x) {
        if (input[y][x] != '.') {
          m_antennas[Point{{x, y}}] = input[y][x];
        }
      }
    }
  }

  char at(const Point &p) const {
    if (m_antennas.contains(p)) {
      return m_antennas.at(p);
    }
    return '.';
  }

  bool in_bounds(const Point &p) const {
    return p[0] >= 0 && p[0] < this->m_width && p[1] >= 0 &&
           p[1] < this->m_height;
  }

  std::map<Point, char> antennas() const { return m_antennas; }

  int width() const { return m_width; }

  int height() const { return m_height; }

  friend std::ostream &operator<<(std::ostream &os, const Map &map) {
    for (int y{0}; y < map.m_height; ++y) {
      for (int x{0}; x < map.m_width; ++x) {
        os << map.at(Point{{x, y}});
      }
      os << std::endl;
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Map map(input);
  std::set<Point> antinodes;
  const auto antennas = map.antennas();
  for (auto [base_pos, base_freq] : antennas) {
    for (auto [pos, freq] : antennas) {
      if (base_pos == pos || base_freq != freq) {
        continue;
      }
      Point target = base_pos - pos + base_pos;
      if (map.in_bounds(target)) {
        antinodes.insert(target);
      }
    }
  }
  return antinodes.size();
}

auto part_two(const std::vector<std::string> &input) {
  Map map(input);
  std::set<Point> antinodes;
  const auto antennas = map.antennas();
  for (auto [base_pos, base_freq] : antennas) {
    for (auto [pos, freq] : antennas) {
      if (base_pos == pos || base_freq != freq) {
        continue;
      }
      const Point diff = base_pos - pos;
      Point target = base_pos;
      while (map.in_bounds(target)) {
        antinodes.insert(target);
        target += diff;
      }
    }
  }
  return antinodes.size();
  ;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_08.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
