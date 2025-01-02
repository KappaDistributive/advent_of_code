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

      if (target[0] >= 0 && target[0] < map.width() && target[1] >= 0 &&
          target[1] < map.height()) {
        // std::cout << std::format("Found resonance for {} at ({},{})", freq,
        // target[0], target[1]) << std::endl;
        antinodes.insert(target);
      }
    }
  }
  return antinodes.size();
}

auto part_two() { return 0; }

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_08.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
