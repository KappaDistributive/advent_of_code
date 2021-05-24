#include <algorithm>
#include <cassert>
#include <map>
#include <limits>
#include <regex>  // NOLINT

#include "../utils/input.hpp"

#ifdef DEBUG
#define DBGVAR(os, var) \
  (os) << "DBG: " << __FILE__ << "(" << __LINE__ << ") "\
       << #var << " = [" << (var) << "]" << std::endl
#else
#define DBGVAR(os, var) do {}while(0)
#endif


typedef std::pair<size_t, size_t> Point;


std::ostream&
operator<<(std::ostream& os, const Point& point) {
  os << "(" << point.first << ", " << point.second << ")";

  return os;
}

enum class Tile: size_t {
  clay,
  sand,
  source,
  water,
};

std::ostream&
operator<<(std::ostream& os, const Tile& tile) {
  switch (tile) {
    case Tile::clay:   os << '#'; break;
    case Tile::sand:   os << '.'; break;
    case Tile::source: os << '+'; break;
    case Tile::water:  os << '~'; break;
    default: throw std::runtime_error("This should never happen."); break;
  }

  return os;
}


Tile from_char(char character) {
  switch (character) {
    case '#': return Tile::clay;   break;
    case '.': return Tile::sand;   break;
    case '+': return Tile::source; break;
    case '~': return Tile::water;  break;
    default: throw std::invalid_argument("Unknown tile: " + std::string{character}); break;
  }
}


class Slice {
 private:
  std::map<Point, Tile> m_tiles;

  std::pair<Point, Point> border() const noexcept {
    Point min{std::numeric_limits<size_t>::max(), std::numeric_limits<size_t>::max()};
    Point max{0, 0};

    for (auto [point, tile] : m_tiles) {
      if (tile == Tile::clay || tile == Tile::source) {
        min.first = std::min(min.first, point.first);
        min.second = std::min(min.second, point.second);
        max.first = std::max(max.first, point.first);
        max.second = std::max(max.second, point.second);
      }
    }

    return {min, max};
  }

 public:
  explicit Slice(const std::vector<std::string>& input) {
    m_tiles.insert({{500, 0}, Tile::source});

    std::regex vertical_regex{"x=(\\d+), y=(\\d+)\\.\\.(\\d+)"};
    std::regex horizontal_regex{"y=(\\d+), x=(\\d+)\\.\\.(\\d+)"};
    std::smatch matches;
    for (auto line : input) {
      std::cout << line << std::endl;
      std::regex_match(line, matches, vertical_regex);
      if (matches.size() == 4) {
        size_t x{std::stoul(matches[1].str())};
        size_t y_min{std::stoul(matches[2].str())};
        size_t y_max{std::stoul(matches[3].str())};
        for (size_t y{y_min}; y <= y_max; ++y) {
          m_tiles.insert({{x, y}, Tile::clay});
        }
      } else {
        std::regex_match(line, matches, horizontal_regex);
        assert(matches.size() == 4);
        size_t y{std::stoul(matches[1].str())};
        size_t x_min{std::stoul(matches[2].str())};
        size_t x_max{std::stoul(matches[3].str())};
        for (size_t x{x_min}; x <= x_max; ++x) {
          m_tiles.insert({{x, y}, Tile::clay});
        }
      }
    }
  }

  Tile
  operator[](const Point& point) const noexcept {
    if (this->m_tiles.count(point) > 0) {
      return this->m_tiles.at(point);
    }
    return Tile::sand;
  }

  friend std::ostream&
  operator<<(std::ostream& os, const Slice& slice) {
    auto [min, max] = slice.border();
    if (min.first > 0) min.first--;
    if (min.second > 0) min.second--;
    max.first++;
    max.second++;

    for (size_t y{min.second}; y <= max.second; ++y) {
      for (size_t x{min.first}; x <= max.first; ++x) {
        os << slice[Point(x, y)];
      }
      if (y < max.second) os << "\n";
    }

    return os;
  }
};

auto
part_one(const std::vector<std::string>& input) {
  Slice slice(input);
  std::cout << slice << std::endl;
  return 1;
}


// auto
// part_two(const std::vector<std::string>& input) {
//   return 2;
// }


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_17_mock.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two =  part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

