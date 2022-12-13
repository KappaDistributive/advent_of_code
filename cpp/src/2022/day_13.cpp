#include <algorithm>
#include <queue>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

struct Packet {
  std::optional<int> value;
  std::vector<Packet> elements;

  Packet (const std::string& input) {
    if (input[0] == '[') {
      assert (input[input.size() - 1] == ']');
      if (input.size() == 2) {
        return;
      }
      std::string buffer{};
      int level{0};

      for (size_t index{1}; index < input.size(); ++index) {
        if (level == 0 && (input[index] == ',' || index + 1 == input.size())) {
          this->elements.push_back(Packet{buffer});
          buffer = "";
          ++index;
        }
        buffer.push_back(input[index]);
        if (input[index] == '[') {
          ++level;
        } else if (input[index] == ']') {
          --level;
        } 
      }
    } else {
      auto delim = std::find(input.cbegin(), input.cend(), ',') - input.cbegin();
      this->value = std::stoi(input.substr(0, delim));
    }
  }

  std::weak_ordering operator<=>(const Packet& other) const {
    // std::cout << "Compare " << *this << " vs. " << other << std::endl;
    if (this->value.has_value() && other.value.has_value()) {
      // std::cout << "Compare " << this->value.value() << " vs. " << other.value.value() << std::endl;
      return this->value.value() <=> other.value.value();
    } else if (this->value.has_value() && !other.value.has_value()) {
      return Packet{fmt::format("[{}]", this->value.value())} <=> other;
    } else if (!this->value.has_value() && other.value.has_value()) {
      return this->operator<=>(Packet{fmt::format("[{}]", other.value.value())});
    }
    auto lhs_it{this->elements.cbegin()};
    auto rhs_it{other.elements.cbegin()};
    while (lhs_it != this->elements.cend() && rhs_it != other.elements.cend()) {
      Packet lhs = *lhs_it;
      Packet rhs = *rhs_it;
      std::weak_ordering comp = lhs.operator<=>(rhs);
      if (comp != std::weak_ordering::equivalent) {
        return comp;
      }
      ++lhs_it;
      ++rhs_it;
    }

    if (lhs_it == this->elements.cend() && rhs_it == other.elements.cend()) {
      return std::weak_ordering::equivalent;
    } else if (lhs_it == this->elements.cend()) {
      return std::weak_ordering::less;
    }
    return std::weak_ordering::greater;
  }

  friend std::ostream& operator<<(std::ostream& os, const Packet& packet) {
    if (packet.value.has_value()) {
      os << packet.value.value();
    } else {
      os << '[';
      for (auto it{packet.elements.cbegin()}; it != packet.elements.cend(); ++it) {
        os << *it;
        if (std::next(it) != packet.elements.cend()) {
          os << ',';
        }
      }
      os << ']';
    }
    return os;
  }
};

std::vector<std::pair<Packet, Packet>> parse_input(const std::vector<std::string> &input) {
  std::vector<std::pair<Packet, Packet>> result;
  Packet packet{"[]"};
  bool first{true};
  for (const auto &line: input) {
    if (line.size() > 0) {
      if(first) {
        first = false;
        packet = Packet{line};
      } else {
        first = true;
        result.push_back(std::make_pair(packet, Packet{line}));
      }
    } else {
      first = true;
    }
  }
  return result;
}


auto part_one(const std::vector<std::string> &input) {
  auto packets = parse_input(input);
  size_t index{0};
  size_t result{0};
  for (const auto &[lhs, rhs] : packets) {
    ++index;
    auto comp = lhs <=> rhs;
    std::cout << lhs;
    if (comp == std::weak_ordering::less) {
      std::cout << " < ";
      result += index;
    } else if (comp == std::weak_ordering::equivalent) {
      std::cout << " == ";
    } else {
      std::cout << " > ";
    }
    std::cout << rhs << std::endl;
  }
  return result;
}

auto part_two(const std::vector<std::string> &input) {
  return 2;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_13_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_13.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
