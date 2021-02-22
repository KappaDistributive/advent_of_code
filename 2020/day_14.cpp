#include <bitset>
#include <cassert>
#include <map>
#include <regex>

#include "../utils/input.hpp"

class Mask {
 private:
  std::bitset<36> data;
  std::bitset<36> specified;

 public:
  Mask() : data(0), specified(0) {
  }

  explicit Mask(const std::string& input) {
    assert(input.size() == 36);
    for (size_t index{0}; index < input.size(); index++) {
      switch (input[35 - index]) {
        case '1':
          data[index] = 1;
          specified[index] = 1;
          break;
        case '0':
          data[index] = 0;
          specified[index] = 1;
          break;
        case 'X':
          data[index] = 0;
          specified[index] = 0;
          break;
        default:
          throw std::invalid_argument("Invalid mask: " + input);
          break;
      }
    }
  }

  bool is_specified(size_t bit) const {
    assert(bit < 36);
    return specified[bit];
  }

  bool get_bit(size_t bit) const {
    assert(bit < 36);
    return data[bit];
  }

  std::string str() {
    std::string result;
    for (size_t index{0}; index < 36; index++) {
      if (specified[index]) {
        result = (data[index] ? '1' : '0') + result;
      } else {
        result = 'X' + result;
      }
    }
    return result;
  }
};

class Value {
 protected:
  std::bitset<36> data;

 public:
  Value() = default;

  explicit Value(const std::string& input) {
    data = std::stoull(input);
  }

  void apply(const Mask& mask) {
    for (size_t index{0}; index < 36; index++) {
      if (mask.is_specified(index)) {
        data[index] = mask.get_bit(index);
      }
    }
  }

  void set_bit(size_t index, bool value = true) {
    data[index] = value;
  }

  operator uint64_t() const {
    uint64_t value{0};
    for (size_t index{0}; index < 36; index++) {
      if (data[index]) {
        value += (1ull << index);
      }
    }
    return value;
  }

  std::string str() const {
    std::string result{""};
    for (size_t index{0}; index < 36; index++) {
      result = (data[index] ? '1' : '0') + result;
    }
    return result;
  }

  bool operator<(const Value& other) const {
    for (size_t index{0}; index < 36; index++) {
      if (this->data[35 - index] < other.data[35 - index]) {
        return true;
      } else if (this->data[35 - index] > other.data[35 - index]) {
        return false;
      }
    }
    return false;
  }
};

class Address: public Value {
 public:
  Address() = default;

  explicit Address(const std::string& input) {
    data = std::stoull(input);
  }

  std::vector<Address> apply(const Mask& mask) const {
    std::vector<Address> addresses;
    Address template_address = *this;
    std::vector<size_t> floating_bits;

    for (size_t index{0}; index < 36; index++) {
      if (mask.is_specified(index) && mask.get_bit(index)) {
        template_address.set_bit(index);
      } else if (!mask.is_specified(index)) {
        floating_bits.push_back(index);
      }
    }

    for (uint64_t noise{0}; noise < (1ull << floating_bits.size()); noise++) {
      Address copy = template_address;
      for (size_t index{0}; index < floating_bits.size(); index++) {
        copy.set_bit(floating_bits[index], (noise & (1ull << index)) > 0);
      }
      addresses.push_back(copy);
    }

    if (addresses.size() == 0) {
      addresses.push_back(template_address);
    }

    return addresses;
  }
};

uint64_t part_one(const std::vector<std::string>& input) {
  Mask mask;
  Value value;
  Address address;
  std::map<Address, Value> memory;
  std::regex regex_mask{"^mask\\s=\\s([X01]{36})$"};
  std::regex regex_memory{"^mem\\[(\\d+)\\]\\s=\\s(\\d+)$"};
  std::smatch matches;

  for (auto line: input) {
    if (std::regex_match(line, matches, regex_mask)) {
      assert(matches.size() == 2);
      mask = Mask(matches[1].str());
    } else {
      assert(std::regex_match(line, matches, regex_memory));
      assert(matches.size() == 3);
      address = Address(matches[1].str());
      value = Value(matches[2].str());
      value.apply(mask);
      try {
        memory.at(address) = value;
      } catch (const std::out_of_range& e) {
        memory.insert(std::make_pair(address, value));
      }
    }
  }
  uint64_t result{0};
  for (auto [_, value]: memory) {
    result += static_cast<uint64_t>(value);
  }
  return result;
}

uint64_t part_two(const std::vector<std::string>& input) {
  Mask mask;
  Value value;
  std::vector<Address> addresses;
  std::map<Address, Value> memory;
  std::regex regex_mask{"^mask\\s=\\s([X01]{36})$"};
  std::regex regex_memory{"^mem\\[(\\d+)\\]\\s=\\s(\\d+)$"};
  std::smatch matches;

  for (auto line: input) {
    if (std::regex_match(line, matches, regex_mask)) {
      assert(matches.size() == 2);
      mask = Mask(matches[1].str());
    } else {
      assert(std::regex_match(line, matches, regex_memory));
      assert(matches.size() == 3);
      value = Value(matches[2].str());
      addresses = Address(matches[1].str()).apply(mask);
      for (auto address: addresses) {
        try {
          memory.at(address) = value;
        } catch (const std::out_of_range& e) {
          memory.insert(std::make_pair(address, value));
        }
      }
    }
  }
  uint64_t result{0};
  for (auto [_, value]: memory) {
    result += static_cast<uint64_t>(value);
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_14.txt"));
  const std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

