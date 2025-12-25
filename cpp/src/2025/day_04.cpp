#include "../utils/input.hpp"

class Map {
private:
  std::vector<std::string> m_data;
  std::size_t m_width, m_height;

public:
  Map(const std::vector<std::string> &data)
      : m_data(data), m_width(data.empty() ? 0 : data[0].size()),
        m_height(data.size()) {}
  ~Map() = default;

  size_t neighbors_count(int x, int y) const {
    size_t count = 0;
    for (int dy = -1; dy <= 1; ++dy) {
      for (int dx = -1; dx <= 1; ++dx) {
        if ((dx == 0 && dy == 0) || (dx < 0 && x == 0) || (dy < 0 && y == 0))
          continue;
        int nx = x + dx;
        int ny = y + dy;
        if (nx < static_cast<int>(this->m_width) &&
            ny < static_cast<int>(this->m_height) &&
            this->m_data[ny][nx] == '@') {
          ++count;
        }
      }
    }
    return count;
  }

  size_t width() const { return m_width; }
  size_t height() const { return m_height; }
  char at(int x, int y) const {
    if (x < 0 || x >= static_cast<int>(m_width) || y < 0 ||
        y >= static_cast<int>(m_height)) {
      return '.';
    }
    return this->m_data[y][x];
  }

  size_t rolls() const {
    size_t count = 0;
    for (auto &line : m_data) {
      for (auto &ch : line) {
        if (ch == '@') {
          ++count;
        }
      }
    }
    return count;
  }

  void clear(int x, int y) {
    if (x >= 0 && x < static_cast<int>(m_width) && y >= 0 &&
        y < static_cast<int>(m_height)) {
      this->m_data[y][x] = 'x';
    }
  }

  bool accessible(int x, int y) const {
    return this->at(x, y) == '@' && this->neighbors_count(x, y) < 4;
  }

  std::vector<std::pair<int, int>> get_accessible_positions() const {
    std::vector<std::pair<int, int>> positions;
    for (int y = 0; y < static_cast<int>(m_height); ++y) {
      for (int x = 0; x < static_cast<int>(m_width); ++x) {
        if (this->accessible(x, y)) {
          positions.emplace_back(x, y);
        }
      }
    }
    return positions;
  }

  friend std::ostream &operator<<(std::ostream &os, const Map &map) {
    for (const auto &line : map.m_data) {
      os << line << std::endl;
    }
    return os;
  }
};

auto part_one(const std::vector<std::string> &data) {
  Map map(data);
  return map.get_accessible_positions().size();
}

auto part_two(const std::vector<std::string> &data) {
  Map map(data);
  size_t before{map.rolls()};
  auto ps = map.get_accessible_positions();
  while (ps.size() > 0) {
    for (const auto &[x, y] : ps) {
      map.clear(x, y);
    }
    ps = map.get_accessible_positions();
  }
  return before - map.rolls();
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_04_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_04.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(data))
            << std::endl;

  return EXIT_SUCCESS;
}
