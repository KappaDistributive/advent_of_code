#include <array>
#include <optional>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using utils::geometry::Direction;

#define RESET "\033[0m"
#define GREEN "\033[32m"

std::pair<int, int> operator+(std::pair<int, int> lhs,
                              std::pair<int, int> rhs) {
  return std::make_pair(lhs.first + rhs.first, lhs.second + rhs.second);
}

void operator+=(std::pair<int, int> &position,
                const std::pair<int, int> &other) {
  position.first += other.first;
  position.second += other.second;
}

std::ostream &operator<<(std::ostream &os,
                         const std::pair<int, int> &position) {
  os << "(" << position.first << ", " << position.second << ")";
  return os;
}

template <size_t _height, size_t _width> class SpiralGrid {
private:
  const size_t height, width;
  const std::pair<size_t, size_t> center;
  std::array<int, _height * _width> *data;
  std::pair<int, int> position;
  Direction direction;
  std::vector<std::pair<int, int>> path;
  size_t path_index;

public:
  explicit SpiralGrid(bool part_two = false)
      : height(_height), width(_width), center({_height / 2, _width / 2}),
        data(new std::array<int, _height * _width>()), position({0, 0}),
        direction(Direction::Down), path({{0, 0}}), path_index(0) {
    build_spiral(part_two);
  }

  void reset() {
    this->position = {0, 0};
    this->direction = Direction::Down;
  }

  int *operator[](std::pair<int, int> position) {
    size_t index{(center.second - position.second) * this->width +
                 center.first + position.first};
    if (index < this->data->size()) {
      return &(this->data->operator[](index));
    } else {
      return nullptr;
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const SpiralGrid &grid) {
    size_t padding{0};
    size_t current_length;
    for (auto entry : *grid.data) {
      current_length = std::to_string(entry).size() + 1;
      if (current_length > padding) {
        padding = current_length;
      }
    }
    for (size_t y{0}; y < grid.height; y++) {
      for (size_t x{0}; x < grid.width; x++) {
        if (y == grid.height / 2 && x == grid.height / 2) {
          os << GREEN;
        }
        os << std::setw(padding) << grid.data->operator[](y * grid.width + x)
           << std::setw(1);
        if (y == grid.height / 2 && x == grid.height / 2) {
          os << RESET;
        }
      }
      os << "\n";
    }
    return os;
  }

  std::pair<size_t, size_t> get_center() const { return this->center; }

  std::pair<int, int> step() {
    // update direction
    switch (this->direction) {
    case Direction::Up:
      if (this->operator[](position + std::make_pair(-1, 0)) == nullptr ||
          *this->operator[](position + std::make_pair(-1, 0)) == 0) {
        this->direction = Direction::Left;
      }
      break;
    case Direction::Right:
      if (this->operator[](position + std::make_pair(0, 1)) == nullptr ||
          *this->operator[](position + std::make_pair(0, 1)) == 0) {
        this->direction = Direction::Up;
      }
      break;
    case Direction::Down:
      if (this->operator[](position + std::make_pair(1, 0)) == nullptr ||
          *this->operator[](position + std::make_pair(1, 0)) == 0) {
        this->direction = Direction::Right;
      }
      break;
    case Direction::Left:
      if (this->operator[](position + std::make_pair(0, -1)) == nullptr ||
          *this->operator[](position + std::make_pair(0, -1)) == 0) {
        this->direction = Direction::Down;
      }
      break;
    }

    // take a step
    switch (direction) {
    case Direction::Up:
      this->position += std::make_pair(0, 1);
      break;
    case Direction::Right:
      this->position += std::make_pair(1, 0);
      break;
    case Direction::Down:
      this->position += std::make_pair(0, -1);
      break;
    case Direction::Left:
      this->position += std::make_pair(-1, 0);
      break;
    }
    this->path.push_back(this->position);
    return this->position;
  }

  std::optional<std::pair<int, int>> find(const int &value) {
    std::pair<int, int> position;
    for (int y{0}; y < static_cast<int>(this->height); y++) {
      for (int x{0}; x < static_cast<int>(this->width); x++) {
        position =
            std::make_pair(x - this->center.first, y - this->center.second);
        if (*this->operator[](position) == value) {
          return position;
        }
      }
    }
    return std::nullopt;
  }

  std::vector<std::pair<int, int>> get_path() const { return this->path; }

  // only works for uneven _height, _width
  void build_spiral(bool part_two = false) {
    int value{1};
    while (this->operator[](this->position) != nullptr) {
      if (part_two && this->position != std::make_pair(0, 0)) {
        value = 0;
        for (int offset_y{-1}; offset_y <= 1; offset_y++) {
          for (int offset_x{-1}; offset_x <= 1; offset_x++) {
            if (offset_y != 0 || offset_x != 0) {
              auto pointer = this->operator[](
                  this->position + std::make_pair(offset_x, offset_y));
              if (pointer != nullptr) {
                value += *pointer;
              }
            }
            *this->operator[](this->position) = value;
          }
        }
      } else {
        if (this->operator[](this->position) != nullptr) {
          *(this->operator[](this->position)) = value;
        }
        value++;
      }
      step();
    }
  }
};

int part_one(const int &input) {
  SpiralGrid<1001, 1001> grid;
  auto position = grid.find(input).value();
  return abs(position.first) + abs(position.second);
}

int part_two(const int &input) {
  SpiralGrid<1001, 1001> grid(true);
  auto path = grid.get_path();
  int value;
  for (auto position : path) {
    value = *grid[position];
    if (value > input) {
      break;
    }
  }
  return value;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_03.txt"));
  auto input = std::stoi(reader.get_lines()[0]);

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
