#include <map>
#include <regex>  // NOLINT

#include "../utils/input.hpp"


class Firewall {
 private:
  struct Scanner {
    size_t time;
    size_t range;
  };
  size_t getScannerDepth(const size_t& index) const {
    if (m_scanners.count(index) == 0) {
      return 0;
    } else {
      auto scanner = m_scanners.at(index);
      size_t period_length = 2 * (scanner.range - 1);
      auto step = scanner.time % period_length;
      if (step < scanner.range) {
        return step;
      } else {
        return scanner.range + 1 - step;
      }
    }
  }
  std::map<size_t, Scanner> m_scanners;
  std::pair<int, int> m_position;

 public:
  explicit Firewall(const std::vector<std::string>& input)
    : m_position({-1, 0}) {
    std::regex re{"^(\\d+):\\s(\\d+)$"};
    std::smatch matches;

    for (auto line : input) {
      std::regex_match(line, matches, re);
      size_t index = std::stoul(matches[1].str());
      m_scanners.insert({index, Scanner{0, std::stoul(matches[2].str())}});
    }

    for (auto [index, scanner] : m_scanners) {
      std::cout << index << ": " << scanner << std::endl;
    }
  }

  size_t step() {
    std::get<0>(m_position)++;
    size_t severity{0};
    for (auto& [index, scanner] : this->m_scanners) {
      if (index == m_position.first &&
          this->getScannerDepth(index) == m_position.second) {
        severity = index * scanner.range;
      }
      scanner.time++;
    }

    return severity;
  }

  size_t size() const {
    if (this->m_scanners.size() == 0) {
      return 0;
    }

    size_t max_index{0};
    for (auto [index, _] : this->m_scanners) {
      max_index = std::max(max_index, index);
    }

    return max_index + 1;
  }

  friend std::ostream& operator<<(std::ostream& os, const Scanner& scanner) {
    os << "Scanner<range: " << scanner.range
       << ", time: " << scanner.time
       << " position: " << scanner.time % scanner.range
       << ">";

    return os;
  }

  friend std::ostream& operator<<(std::ostream& os, const Firewall& firewall) {
    size_t max_index{0};
    size_t max_range{0};
    for (auto [index, scanner] : firewall.m_scanners) {
      max_index = std::max(max_index, index);
      max_range = std::max(max_range, scanner.range);
    }

    for (size_t index{0}; index <= max_index; index++) {
      std::cout << "  " << index << "  ";
    }
    std::cout << std::endl;

    for (size_t depth{0}; depth <= max_range; depth++) {
      for (size_t index{0}; index <= max_index; index++) {
        auto is_at_package = firewall.m_position.first == index &&
                             firewall.m_position.second == depth;
        auto is_in_range = firewall.m_scanners.count(index) > 0 &&
                           firewall.m_scanners.at(index).range > depth;
        bool is_at_scanner = firewall.m_scanners.count(index) > 0 &&
                             firewall.getScannerDepth(index) == depth;
      if (is_in_range) {
          std::cout << " " << (is_at_package ? "(" : "[");
          std::cout << (is_at_scanner ? "S" : " ");
          std::cout << (is_at_package ? ")" : "]") << " ";
        } else if (depth == 0) {
          std::cout << " ... ";
        } else {
          std::cout << "     ";
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
    severity += firewall.step();
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

