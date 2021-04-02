#include <map>
#include <regex>  // NOLINT

#include "../utils/input.hpp"


class Firewall {
 private:
  int m_position;
  int m_time;
  std::vector<size_t> m_scanner_ranges;

 public:
  explicit Firewall(const std::vector<std::string>& input)
    : m_position(-1),
      m_time(0) {
    std::regex re{"^(\\d+):\\s(\\d+)$"};
    std::smatch matches;

    for (auto line : input) {
      std::regex_match(line, matches, re);
      size_t index = std::stoul(matches[1].str());
      while (this->m_scanner_ranges.size() < index + 1) {
        this->m_scanner_ranges.push_back(0);
      }
      this->m_scanner_ranges[index] = std::stoul(matches[2].str());
    }
  }

  void step() {
    this->m_time++;
  }

  size_t move() {
    this->m_position++;
    if (this->m_position < this->size() &&
        this->getScannerLayer(this->m_position) == 0) {
      return this->m_position * this->m_scanner_ranges[this->m_position];
    }

    return 0;
  }

  size_t size() const {
    return this->m_scanner_ranges.size();
  }

  int getPosition() const {
    return this->m_position;
  }

  size_t getScannerLayer(const size_t& scanner) const {
    auto period_length = 2 * (this->m_scanner_ranges[scanner] - 1);
    auto step = m_time % period_length;

    if (step < this->m_scanner_ranges[scanner]) {
      return step;
    } else {
      return this->m_scanner_ranges[scanner] + 1 - step;
    }
  }

  friend std::ostream& operator<<(std::ostream& os, const Firewall& firewall) {
    // print time
    if (firewall.m_time >= 0) {
      std::cout << "Picosecond: " << firewall.m_time << std::endl;
    } else {
      std::cout << "Initial state:" << std::endl;
    }
    // print index
    for (int depth{-1}; depth < static_cast<int>(firewall.size()); depth++) {
      std::cout << (depth < 0 ? " " : "  ") << depth << "  ";
    }
    std::cout << std::endl;

    auto max_range = *std::max_element(firewall.m_scanner_ranges.begin(),
                                       firewall.m_scanner_ranges.end());

    // print layers
    for (size_t layer{0}; layer <= max_range; layer++) {
      for (int depth{-1}; depth < static_cast<int>(firewall.size()); depth++) {
        if (depth < 0) {
          auto is_at_package = firewall.m_position == depth;
          if (layer == 0) {
            std::cout << " " << (is_at_package ? "(" : "[");
            std::cout << " ";
            std::cout << (is_at_package ? ")" : "]") << " ";
          } else {
            std::cout << "     ";
          }
        } else {
          auto is_at_package = layer == 0 && firewall.m_position == depth;
          auto is_in_range = layer < firewall.m_scanner_ranges[depth];
          bool is_at_scanner = firewall.getScannerLayer(depth) == layer;
          if (is_in_range) {
            std::cout << " " << (is_at_package ? "(" : "[");
            std::cout << (is_at_scanner ? "S" : " ");
            std::cout << (is_at_package ? ")" : "]") << " ";
          } else if (layer == 0) {
            std::cout << (is_at_package ? " (.) " : " ... ");
          } else {
            std::cout << "     ";
          }
        }
      }
      std::cout << std::endl;
    }

    return os;
  }
};


size_t part_one(const std::vector<std::string>& input) {
  Firewall firewall(input);
  size_t severity{0};

  std::cout << firewall << std::endl;
  for (size_t step{0}; step < firewall.size(); step++) {
    severity += firewall.move();
    std::cout << firewall << std::endl;
    firewall.step();
    std::cout << firewall << std::endl;
  }

  return severity;
}

size_t part_two(const std::vector<std::string>& input) {
  return 37;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_13.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

