#include "../utils/input.hpp"

enum Command { rotate_row, rotate_column, rectangle };

template <size_t _width, size_t _height>
class Display {
 private:
  size_t width, height;
  std::array<bool, _width * _height> content;

 public:
  Display() : width(_width), height(_height) {
    content = std::array<bool, _width * _height>();
  }

  void rotate_row(const size_t& row_index, const int& rotation) {
    if (row_index >= height) {
      throw std::out_of_range("row_index is out of range.");
    }
    std::vector<bool> row;
    row.reserve(this->width);

    for (size_t index{0}; index < this->width; index++) {
      row.push_back(this->operator[](std::make_pair(index, row_index)));
    }
    row = utils::rotate_vector(row, rotation);
    for (size_t index{0}; index < this->width; index++) {
      this->operator[](std::make_pair(index, row_index)) = row[index];
    }
  }

  void set_rectangle(const size_t& width, const size_t& height) {
    for (size_t y{0}; y < height; y++) {
      for (size_t x{0}; x < width; x++) {
        this->operator[]({x, y}) = 1;
      }
    }
  }

  void rotate_column(const size_t& column_index, const int& rotation) {
    if (column_index >= width) {
      throw std::out_of_range("column_index is out of range.");
    }
    std::vector<bool> column;
    column.reserve(this->height);

    for (size_t index{0}; index < this->height; index++) {
      column.push_back(this->operator[](std::make_pair(column_index, index)));
    }
    column = utils::rotate_vector(column, rotation);
    for (size_t index{0}; index < this->height; index++) {
      this->operator[](std::make_pair(column_index, index)) = column[index];
    }
  }

  int brightness() const {
    int count{0};
    for (size_t y{0}; y < this->height; y++) {
      for (size_t x{0}; x < this->width; x++) {
        count += this->content[y * this->width + x];
      }
    }
    return count;
  }

  bool& operator[](const std::pair<size_t, size_t>& position) {
    if (position.first >= width) {
      throw std::out_of_range("First coordinate is out of bounds.");
    } else if (position.second >= height) {
      throw std::out_of_range("Second coordinate is out of bounds.");
    }
    return this->content[position.second * this->width + position.first];
  }

  std::string str() const {
    std::string representation;
    for (size_t y{0}; y < this->height; y++) {
      for (size_t x{0}; x < this->width; x++) {
        representation.push_back(this->content[y * this->width + x] ? '#'
                                                                    : '.');
      }
      representation.push_back('\n');
    }
    return representation;
  }

  friend std::ostream& operator<<(std::ostream& os, const Display& display) {
    os << display.str();
    return os;
  }
};

std::vector<std::tuple<Command, int, int>> prepare_input(
    const std::vector<std::string>& input) {
  std::vector<std::tuple<Command, int, int>> commands;
  std::regex rectangle_regex{"^rect\\s(\\d+)x(\\d+)$"};
  std::regex rotation_regex{
      "^rotate\\s(column|row)\\s(?:x|y)=(\\d+)\\sby\\s(\\d+)$"};  // NOLINT
  std::smatch matches;
  for (auto line : input) {
    std::regex_match(line, matches, rectangle_regex);
    if (matches.size() == 3) {
      commands.push_back({rectangle, std::stoi(matches[1].str()),
                          std::stoi(matches[2].str())});
    } else {
      std::regex_match(line, matches, rotation_regex);
      if (matches.size() == 4 && matches[1].str() == "column") {
        commands.push_back({rotate_column, std::stoi(matches[2].str()),
                            std::stoi(matches[3].str())});
      } else if (matches.size() == 4 && matches[1].str() == "row") {
        commands.push_back({rotate_row, std::stoi(matches[2].str()),
                            std::stoi(matches[3].str())});
      } else {
        throw std::invalid_argument("Unknown command: " + line);
      }
    }
  }
  return commands;
}

int part_one(const std::vector<std::string>& input) {
  bool verbose{false};
  Display<50, 6> display;
  auto commands = prepare_input(input);

  for (auto [op, a, b] : commands) {
    if (op == rectangle) {
      display.set_rectangle(a, b);
    } else if (op == rotate_column) {
      display.rotate_column(a, b);
    } else if (op == rotate_row) {
      display.rotate_row(a, b);
    } else {
      throw std::invalid_argument("");
    }

    if (verbose) {
      std::cout << display << std::endl;
    }
  }
  return display.brightness();
}

std::string part_two(const std::vector<std::string>& input) {
  bool verbose{false};
  Display<50, 6> display;
  auto commands = prepare_input(input);

  for (auto [op, a, b] : commands) {
    if (op == rectangle) {
      display.set_rectangle(a, b);
    } else if (op == rotate_column) {
      display.rotate_column(a, b);
    } else if (op == rotate_row) {
      display.rotate_row(a, b);
    } else {
      throw std::invalid_argument("");
    }

    if (verbose) {
      std::cout << display << std::endl;
    }
  }
  return "\n" + display.str();
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2016/input_08.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
