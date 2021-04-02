#include <map>
#include <regex>  // NOLINT

#include "../utils/input.hpp"


class Firewall {
 private:
  int m_position;
  int m_time;
  bool m_moved;
  std::vector<size_t> m_scanner_ranges;

 public:
  explicit Firewall(const std::vector<std::string>& input)
    : m_position(-1),
      m_time(0),
      m_moved(false) {
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
    this->m_moved = false;
  }

  std::pair<size_t, bool> move() {
    size_t severity{0};
    bool got_caught{false};
    this->m_moved = true;
    this->m_position++;
    if (this->m_position < this->size() &&
        this->getScannerLayer(this->m_position) == 0) {
      severity = this->m_position * this->m_scanner_ranges[this->m_position];
      got_caught = true;
    }

    return {severity, got_caught};
  }

  void setTime(size_t time) {
    this->m_time = time;
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
      return 2 * this->m_scanner_ranges[scanner] - 2 - step;
    }
  }

  friend std::ostream& operator<<(std::ostream& os, const Firewall& firewall) {
    // print time
    if (firewall.m_moved) {
      std::cout << "Picosecond: " << firewall.m_time << std::endl;
    }
    // print index
    for (size_t depth{0}; depth < firewall.size(); depth++) {
      std::cout << "  " << depth << "  ";
    }
    std::cout << std::endl;

    auto max_range = *std::max_element(firewall.m_scanner_ranges.begin(),
                                       firewall.m_scanner_ranges.end());

    // print layers
    for (size_t layer{0}; layer <= max_range; layer++) {
      for (size_t depth{0}; depth < firewall.size(); depth++) {
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
      std::cout << std::endl;
    }

    return os;
  }
};

std::pair<size_t, bool> run(
           Firewall firewall,
           size_t delay = 0,
           bool part_two = false,
           bool verbose = false) {
  firewall.setTime(delay);

  size_t severity{0};
  bool got_caught{false};
  for (size_t step{0}; step < firewall.size(); step++) {
    auto update = firewall.move();
    severity += std::get<0>(update);
    got_caught = got_caught || std::get<1>(update);

    // early stopping
    if (part_two && got_caught) {
      return {0, got_caught};
    }

    if (verbose)
      std::cout << firewall << std::endl;
    firewall.step();
    if (verbose)
      std::cout << firewall << std::endl;
  }

  return {severity, got_caught};
}


size_t part_one(const std::vector<std::string>& input) {
  Firewall firewall(input);
  return std::get<0>(run(firewall));
}

size_t part_two(const std::vector<std::string>& input) {
  size_t delay{0};
  Firewall firewall(input);
  while (std::get<1>(run(firewall, delay, true))) {
    delay++;
  }
  return delay;
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

