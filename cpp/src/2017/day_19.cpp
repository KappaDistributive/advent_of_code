#include <array>

#include "../utils/input.hpp"

typedef std::pair<size_t, size_t> coordinate;

std::ostream& operator<<(std::ostream& os, const coordinate coord) {
  os << "(" << coord.first << ", " << coord.second << ")";
  return os;
}

enum class Direction : int { north, east, south, west };

constexpr std::ostream& operator<<(std::ostream& os,
                                   const Direction direction) {
  switch (direction) {
    case Direction::north:
      os << '^';
      break;
    case Direction::east:
      os << '>';
      break;
    case Direction::south:
      os << 'v';
      break;
    case Direction::west:
      os << '<';
      break;
  }

  return os;
}

constexpr Direction operator-(const Direction direction) {
  switch (direction) {
    case Direction::north:
      return Direction::south;
      break;
    case Direction::east:
      return Direction::west;
      break;
    case Direction::south:
      return Direction::north;
      break;
    case Direction::west:
      return Direction::east;
      break;
  }
}

constexpr std::array<Direction, 4> all_directions{
    {Direction::north, Direction::east, Direction::south, Direction::west}};

class Grid {
 private:
  coordinate m_starting_position;
  coordinate m_position;
  std::vector<char> m_grid;
  std::vector<char> m_encounters;
  size_t m_width;
  size_t m_height;
  Direction m_direction;
  size_t m_num_steps;

  auto find_starting_position(const std::vector<std::string>& input) const {
    coordinate starting_position{0, 0};
    assertm(input.size() > 0 && input[0].size() > 0, "Invalid input.");
    for (size_t x{0}; x < input[0].size(); ++x) {
      if (input[0][x] == '|') {
        assertm(starting_position.first == 0,
                "Found multiple starting points.");
        starting_position.first = x;
      }
    }
    return starting_position;
  }

  auto find_dimensions(const std::vector<std::string>& input) const {
    std::pair<size_t, size_t> dimensions{0, 0};
    assertm(input.size() > 0 && input[0].size() > 0, "Invalid input.");
    dimensions.first = input[0].size();
    for (auto it{input.begin()}; it != input.end(); ++it) {
      auto line = *it;
      assertm(line.size() == dimensions.first, "Invalid input.");
      utils::replace_all_substrings(&line, " ", "");
      if (line.size() > 0) {
        ++dimensions.second;
      } else {
        assertm(std::next(it) == input.end(),
                "Input contains an empty line other than its final line");
      }
    }
    return dimensions;
  }

 public:
  explicit Grid(const std::vector<std::string>& input)
      : m_direction(Direction::south), m_num_steps(1) {
    m_starting_position = find_starting_position(input);
    m_position = m_starting_position;
    auto dimensions = find_dimensions(input);
    m_width = dimensions.first;
    m_height = dimensions.second;
    m_grid.reserve(m_height * m_width);
    for (size_t y{0}; y < m_height; ++y) {
      for (size_t x{0}; x < m_width; ++x) {
        m_grid[y * m_width + x] = input[y][x];
      }
    }
  }

  std::optional<coordinate> move(Direction direction) {
    bool failed{false};
    auto position = m_position;
    switch (direction) {
      case Direction::north:
        if (position.second > 0) {
          --position.second;
        } else {
          failed = true;
        }
        break;
      case Direction::east:
        ++position.first;
        break;
      case Direction::south:
        ++position.second;
        break;
      case Direction::west:
        if (position.first > 0) {
          --position.first;
        } else {
          failed = true;
        }
        break;
    }

    if (failed || position.first >= m_width || position.second >= m_height ||
        m_grid[position.second * m_width + position.first] == ' ') {
      return std::nullopt;
    }
    return position;
  }

  auto move() { return move(m_direction); }

  auto step() {
    auto position = move();
    bool moved = false;
    if (position.has_value()) {
      m_position = position.value();
      moved = true;
    } else {
      for (auto direction : all_directions) {
        if (direction == m_direction || direction == -m_direction) {
          continue;
        }
        auto position = move(direction);
        if (position.has_value()) {
          m_direction = direction;
          m_position = position.value();
          moved = true;
          break;
        }
      }
    }

    if (moved &&
        'A' <= m_grid[m_position.second * m_width + m_position.first] &&
        m_grid[m_position.second * m_width + m_position.first] <= 'Z') {
      m_encounters.push_back(
          m_grid[m_position.second * m_width + m_position.first]);
    }

    if (moved) {
      ++m_num_steps;
    }

    return moved;
  }

  std::vector<char> encounters() const { return m_encounters; }

  auto total_steps() const { return m_num_steps; }

  friend std::ostream& operator<<(std::ostream& os, const Grid& grid) {
    os << "Encounters: ";
    for (auto it = grid.m_encounters.begin(); it != grid.m_encounters.end();
         ++it) {
      os << *it;
      if (std::next(it) != grid.m_encounters.end()) {
        os << ", ";
      }
    }
    os << '\n';

    for (size_t y{0}; y < grid.m_height; ++y) {
      for (size_t x{0}; x < grid.m_width; ++x) {
        if (y == grid.m_position.second && x == grid.m_position.first) {
          os << grid.m_direction;
        } else {
          os << grid.m_grid[y * grid.m_width + x];
        }
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  Grid grid(input);
  do {
    // std::cout << grid << std::endl;
  } while (grid.step());
  std::string result;

  auto encounters = grid.encounters();
  for (auto encounter : encounters) {
    result += encounter;
  }
  return result;
}

auto part_two(const std::vector<std::string>& input) {
  Grid grid(input);
  do {
    // std::cout << grid << std::endl;
  } while (grid.step());
  return grid.total_steps();
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_19.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
}

