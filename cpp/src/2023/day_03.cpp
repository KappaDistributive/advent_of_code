#include "../utils/input.hpp"
#include <cassert>
#include <cstddef>
#include <ostream>
#include <utility>
#include <vector>

struct Coord {
  int x;
  int y;

  bool operator==(const Coord &other) const {
    return this->x == other.x && this->y == other.y;
  }
};

class Engine {

public:
  size_t m_width, m_height;
  std::string m_data;
  std::vector<std::pair<Coord, Coord>> m_numeric_spans;

  Engine(const std::vector<std::string> &description) {
    this->m_width = description[0].size();
    this->m_height = description.size();
    for (size_t y{0}; y < this->m_height; ++y) {
      for (size_t x{0}; x < this->m_width; ++x) {
        this->m_data.push_back(description[y][x]);
      }
    }
    bool in_number{false};
    std::pair<size_t, size_t> span;
    for (size_t y{0}; y < this->m_height; ++y) {
      for (size_t x{0}; x <= this->m_width; ++x) {
        auto symbol = this->at(x, y);
        if (!in_number && '0' <= symbol && symbol <= '9') {
          in_number = true;
          span.first = y * this->m_width + x;
        } else if (in_number &&
                   (x == this->m_width || !('0' <= symbol && symbol <= '9'))) {
          span.second = y * this->m_width + x - 1;
          int y = span.first / this->m_width;
          int x_start = span.first % this->m_width;
          int x_end = span.second % this->m_width;

          this->m_numeric_spans.push_back(
              std::make_pair(Coord{x_start, y}, Coord{x_end, y}));
          in_number = false;
        }
      }
    }
  }

  char at(const int x, const int y) const {
    if (x < 0 || x >= static_cast<int>(this->m_width) || y < 0 ||
        y >= static_cast<int>(this->m_height)) {
      return '.';
    }
    return this->m_data[y * this->m_width + x];
  }

  std::vector<std::pair<Coord, Coord>> spans_in_reach(const int x,
                                                      const int y) const {
    std::vector<std::pair<Coord, Coord>> result;
    for (int offset_y{-1}; offset_y <= 1; ++offset_y) {
      for (int offset_x{-1}; offset_x <= 1; ++offset_x) {
        Coord pos{x + offset_x, y + offset_y};
        for (auto [left, right] : this->m_numeric_spans) {
          if (pos.x >= left.x && pos.x <= right.x && pos.y == left.y) {
            if (std::find(result.cbegin(), result.cend(),
                          std::make_pair(left, right)) == result.cend()) {
              result.push_back(std::make_pair(left, right));
            }
          }
        }
      }
    }
    return result;
  }

  size_t span_to_number(const Coord left, const Coord right) const {
    assert(left.x <= right.x && left.y == right.y);
    size_t number{0};
    for (int x{left.x}; x <= right.x; ++x) {
      char symbol = this->at(x, left.y);
      assert('0' <= symbol && symbol <= '9');
      number = number * 10 + static_cast<size_t>(symbol - '0');
    }
    return number;
  }

  friend std::ostream &operator<<(std::ostream &os, const Engine engine) {
    for (size_t y{0}; y < engine.m_height; ++y) {
      for (size_t x{0}; x < engine.m_width; ++x) {
        os << engine.at(x, y);
      }
      os << '\n';
    }
    return os;
  }
};

bool is_symbol(char s) { return s != '.' && !('0' <= s && s <= '9'); }

auto part_one(Engine engine) {
  std::vector<std::pair<Coord, Coord>> spans_in_reach;
  for (int y{0}; y < static_cast<int>(engine.m_height); ++y) {
    for (int x{0}; x < static_cast<int>(engine.m_width); ++x) {
      if (!is_symbol(engine.at(x, y))) {
        continue;
      }
      for (auto span : engine.spans_in_reach(x, y)) {
        spans_in_reach.push_back(span);
      }
    }
  }

  size_t result{0};
  for (auto [left, right] : spans_in_reach) {
    result += engine.span_to_number(left, right);
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_03_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_03.txt"};
  utils::Reader reader(input_path);
  auto engine = Engine(reader.get_lines());

  fmt::print("The answer to part one is: {}\n", part_one(engine));
  fmt::print("The answer to part two is: {}\n", part_two());

  return 0;
}
