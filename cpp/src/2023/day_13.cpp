#include "../utils/input.hpp"
#include <cassert>

class Pattern {
private:
  size_t m_width, m_height;
  std::string m_buffer;

public:
  Pattern(const std::vector<std::string> &input)
      : m_width(input[0].size()), m_height(input.size()) {
    m_buffer = std::string(input[0].size() * input.size(), ' ');
    size_t index{0};
    for (const auto &line : input) {
      assert(line.size() > 0);
      for (const auto &character : line) {
        this->m_buffer[index++] = character;
      }
    }
  }

  char at(size_t x, size_t y) const {
    return this->m_buffer[y * this->m_width + x];
  }

  bool has_horizontal_symmetry_at(size_t index) const {
    if (index >= this->m_height - 1) {
      return false;
    }
    int index_low = index, index_high = index + 1;
    ;
    while (index_low >= 0 && index_high < static_cast<int>(this->m_height)) {
      for (size_t x{0}; x < this->m_width; ++x) {
        if (this->at(x, index_low) != this->at(x, index_high)) {
          return false;
        }
      }
      --index_low;
      ++index_high;
    }
    return true;
  }

  bool has_vertical_symmetry_at(size_t index) const {
    if (index >= this->m_width - 1) {
      return false;
    }
    int index_left = index, index_right = index + 1;
    while (index_left >= 0 && index_right < static_cast<int>(this->m_width)) {
      for (size_t y{0}; y < this->m_height; ++y) {
        if (this->at(index_left, y) != this->at(index_right, y)) {
          return false;
        }
      }
      --index_left;
      ++index_right;
    }
    return true;
  }

  bool has_symmetry_at(size_t index, bool horizontal) const {
    if (horizontal) {
      return this->has_horizontal_symmetry_at(index);
    }
    return this->has_vertical_symmetry_at(index);
  }

  size_t width() const { return this->m_width; }

  size_t height() const { return this->m_height; }

  friend std::ostream &operator<<(std::ostream &os, const Pattern &pattern) {
    for (size_t y{0}; y < pattern.m_height; ++y) {
      for (size_t x{0}; x < pattern.m_width; ++x) {
        os << pattern.at(x, y);
      }
      if (y + 1 < pattern.m_height)
        os << '\n';
    }
    return os;
  }
};

std::vector<Pattern> parse(const std::vector<std::string> &input) {
  std::vector<Pattern> patterns;
  std::vector<std::string> buffer;
  for (auto line : input) {
    if (line.size() == 0) {
      patterns.push_back(Pattern{buffer});
      buffer = {};
      continue;
    }
    buffer.push_back(line);
  }
  if (buffer.size() != 0) {
    patterns.push_back(Pattern{buffer});
  }

  return patterns;
}

auto part_one(const std::vector<std::string> &input) {
  size_t result{0};
  auto patterns = parse(input);
  for (const auto &pattern : patterns) {
    std::cout << pattern << std::endl;
    std::cout << ("### Vertical symmetry ###") << std::endl;
    for (size_t index{0}; index < pattern.width() - 1; ++index) {
      std::cout << index << ": "
                << (pattern.has_symmetry_at(index, false) ? '.' : 'x')
                << std::endl;
      if (pattern.has_vertical_symmetry_at(index)) {
        result += index + 1;
      }
    }
    for (size_t index{0}; index < pattern.height() - 1; ++index) {
      if (pattern.has_horizontal_symmetry_at(index)) {
        result += 100 * (index + 1);
      }
    }
  }
  return result;
}

auto part_two() { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2023/input_13_mock.txt"};
  std::filesystem::path input_path{"../../data/2023/input_13.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
