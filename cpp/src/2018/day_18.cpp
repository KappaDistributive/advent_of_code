#include <algorithm>
#include <cassert>

#include "../utils/input.hpp"

#ifdef DEBUG
#define DBGVAR(os, var)                                                    \
  (os) << "DBG: " << __FILE__ << "(" << __LINE__ << ") " << #var << " = [" \
       << (var) << "]" << std::endl
#else
#define DBGVAR(os, var) \
  do {                  \
  } while (0)
#endif

struct Position {
  int x;
  int y;

  Position operator+(Position rhs) {
    Position result{this->x + rhs.x, this->y + rhs.y};
    return result;
  }
};

std::ostream& operator<<(std::ostream& os, const Position& position) {
  os << '(' << position.x << ", " << position.y << ')';
  return os;
}

class LumberArea {
 private:
  size_t m_width, m_height;
  std::vector<char> m_grid;

 public:
  explicit LumberArea(const std::vector<std::string>& input) : m_height(0) {
    this->m_width = input[0].size();
    for (auto line : input) {
      if (line.size() == 0) {
        continue;
      }
      ++this->m_height;
      assert(line.size() == this->m_width);
      for (auto acre : line) {
        assert(acre == '.' || acre == '|' || acre == '#');
        this->m_grid.push_back(acre);
      }
    }
  }

  bool is_in_bounds(Position position) const noexcept {
    return position.x >= 0 && position.x < static_cast<int>(this->m_width) && position.y >= 0 && position.y < static_cast<int>(this->m_height);
  }

  char& operator[](Position position) {
    if (!this->is_in_bounds(position)) {
      throw std::out_of_range("Out of range.");
    }
    return m_grid[position.y * this->m_width + position.x];
  }

  char operator[](Position position) const {
    if (!this->is_in_bounds(position)) {
      throw std::out_of_range("Out of range.");
    }
    return m_grid[position.y * this->m_width + position.x];
  }

  std::vector<char> adjacent_acres(Position position) const noexcept {
    std::vector<char> neighbors;

    for (int offset_y{-1}; offset_y <= 1; ++offset_y) {
      for (int offset_x{-1}; offset_x <= 1; ++offset_x) {
        if (offset_x == 0 && offset_y == 0) {
          continue;
        }
        try {
          neighbors.push_back(
              this->operator[](position + Position{offset_x, offset_y}));
        } catch (const std::out_of_range& e) {
          neighbors.push_back('.');
        }
      }
    }

    assert(neighbors.size() == 8);

    return neighbors;
  }

  void step() {
    std::vector<char> grid;
    for (int y{0}; y < static_cast<int>(this->m_height); ++y) {
      for (int x{0}; x < static_cast<int>(this->m_width); ++x) {
        Position position{x, y};
        auto neigbhors = this->adjacent_acres(position);
        // std::cerr << position << std::endl;
        // for (auto neighbor : neigbhors) {
        //   std::cerr << "`" << neighbor << "` ";
        // }
        // std::cerr << std::endl;
        size_t num_open{0}, num_tree{0}, num_lumberyard{0};
        for (auto neighbor : neigbhors) {
          switch (neighbor) {
            case '.':  // open
              ++num_open;
              break;
            case '|':  // tree
              ++num_tree;
              break;
            case '#':  // lumberyard
              ++num_lumberyard;
              break;
            default:
              std::cerr << "Encountered neighbor: `" << neighbor << "`"
                        << std::endl;
              throw std::runtime_error("This should never happen!");
              break;
          }
        }
        switch (this->operator[](position)) {
          case '.':  // open
            grid.push_back(num_tree >= 3 ? '|' : '.');
            break;
          case '|':  // tree
            grid.push_back(num_lumberyard >= 3 ? '#' : '|');
            break;
          case '#':  // lumberyard
            grid.push_back((num_lumberyard > 0 && num_tree > 0) ? '#' : '.');
            break;
          default:
            throw std::runtime_error("This should never happen!");
            break;
        }
      }
    }

    assert(this->m_grid.size() == grid.size());
    this->m_grid = grid;
  }

  std::vector<char> grid() const noexcept {
    return this->m_grid;
  }

  size_t resource_value() const noexcept {
    size_t num_tree{0}, num_lumberyard{0};
    for (auto acre : this->m_grid) {
      switch (acre) {
        case '|':
          ++num_tree;
          break;
        case '#':
          ++num_lumberyard;
          break;
        default:
          break;
      }
    }

    return num_tree * num_lumberyard;
  }

  friend std::ostream& operator<<(std::ostream& os, const LumberArea& area) {
    for (int y{0}; y < static_cast<int>(area.m_height); ++y) {
      for (int x{0}; x < static_cast<int>(area.m_width); ++x) {
        os << area[Position{x, y}];
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  LumberArea area(input);
  std::cout << area << std::endl;
  for (size_t step{1}; step <= 10; ++step) {
    area.step();
  }
  std::cout << area << std::endl;
  return area.resource_value();
}

auto part_two(const std::vector<std::string>& input) {
  LumberArea area(input);
  std::vector<std::pair<std::vector<char>, size_t>> history;
  history.push_back(std::make_pair(area.grid(), area.resource_value()));
  size_t duplicate_offset{0};
  size_t cycle_length{0};
  while (true) {
    area.step();
    auto grid = area.grid();
    auto score = area.resource_value();
    auto find_result = std::find(history.begin(), history.end(), std::make_pair(grid, score));
    if (find_result == history.end()) {
      history.push_back(std::make_pair(area.grid(), area.resource_value()));
    } else {
      duplicate_offset = find_result - history.begin();
      cycle_length = history.size() - duplicate_offset;
      break;
    }
  }
  return std::get<1>(history[duplicate_offset + ((1000000000 - duplicate_offset) % cycle_length)]);
}


int main() {
  // utils::Reader reader(std::filesystem::path("../../data/2018/input_18_mock.txt"));
  utils::Reader reader(std::filesystem::path("../../data/2018/input_18.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

