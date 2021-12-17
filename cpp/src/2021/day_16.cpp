#include <deque>
#include <limits>
#include <variant>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

size_t decode(std::deque<bool>& bits, size_t num_bits) {
  size_t result{0};

  for (size_t index{0}; index < num_bits; ++index) {
    result <<= 1;
    result |= bits.front();
    bits.pop_front();
  }

  return result;
}

std::deque<bool> decode(std::string hexcode) {
  std::deque<bool> result;

  for (auto character : hexcode) {
    int nibble{0};
    assertm(('0' <= character && character <= '9') ||
                ('A' <= character && character <= 'F'),
            "Encountered invalid hexcode.");
    if (character <= '9') {
      nibble = character - '0';
    } else {
      nibble = 10 + character - 'A';
    }

    for (size_t index{0}; index < 4; ++index) {
      result.push_back(nibble & (8 >> index));
    }
  }

  return result;
}

auto split(std::deque<bool>& bits, size_t num_bits) {
  std::deque<bool> result{};
  std::copy_n(std::cbegin(bits), num_bits, std::back_inserter(result));
  bits.erase(std::cbegin(bits), std::cbegin(bits) + num_bits);

  return result;
}

struct Packet {
  using SubPackets = std::vector<std::unique_ptr<const Packet>>;
  size_t version;
  size_t type_id;
  std::variant<size_t, SubPackets> payload;

 public:
  static Packet fromBits(std::deque<bool>& bits) {
    size_t version = decode(bits, 3);
    size_t type_id = decode(bits, 3);
    Packet result{};
    result.version = version;
    result.type_id = type_id;

    if (type_id == 4) {  // literal value
      size_t payload{0};
      while (bits.front()) {
        bits.pop_front();
        payload <<= 4;
        payload += decode(bits, 4);
      }
      bits.pop_front();
      payload <<= 4;
      payload += decode(bits, 4);

      result.payload = payload;
    } else {  // operator
      auto length_type_id = decode(bits, 1);
      SubPackets sub_packets;

      if (length_type_id == 0) {
        size_t total_length = decode(bits, 15);
        auto sub_bits = split(bits, total_length);
        while (!sub_bits.empty()) {
          sub_packets.emplace_back(
              std::make_unique<const Packet>(Packet::fromBits(sub_bits)));
        }
      } else {
        assertm(length_type_id == 1, "Encountered invalid length type ID.");
        size_t num_sub_packets = decode(bits, 11);
        for (size_t index{0}; index < num_sub_packets; ++index) {
          sub_packets.emplace_back(
              std::make_unique<const Packet>(Packet::fromBits(bits)));
        }
      }
      result.payload = std::move(sub_packets);
    }

    return result;
  }

  size_t version_sum() const {
    size_t result{version};

    if (std::holds_alternative<SubPackets>(payload)) {
      for (const auto& sub_packet : std::get<SubPackets>(payload)) {
        result += sub_packet->version_sum();
      }
    }

    return result;
  }

  size_t value() const {
    if (type_id == 4) {
      return std::get<size_t>(payload);
    }
    const auto& sub_packets = std::get<SubPackets>(payload);

    size_t result{0};
    if (type_id == 0) {
      result = 0;
      for (const auto& sub_packet : sub_packets) {
        result += sub_packet->value();
      }
    } else if (type_id == 1) {
      result = 1;
      for (const auto& sub_packet : sub_packets) {
        result *= sub_packet->value();
      }
    } else if (type_id == 2) {
      result = std::numeric_limits<size_t>::max();
      for (const auto& sub_packet : sub_packets) {
        result = std::min(result, sub_packet->value());
      }
    } else if (type_id == 3) {
      result = std::numeric_limits<size_t>::min();
      for (const auto& sub_packet : sub_packets) {
        result = std::max(result, sub_packet->value());
      }
    } else if (type_id == 5) {
      result = sub_packets[0]->value() > sub_packets[1]->value() ? 1 : 0;
    } else if (type_id == 6) {
      result = sub_packets[0]->value() < sub_packets[1]->value() ? 1 : 0;
    } else if (type_id == 7) {
      result = sub_packets[0]->value() == sub_packets[1]->value() ? 1 : 0;
    } else {
      throw std::runtime_error(fmt::format("Invalid type ID: {}", type_id));
    }

    return result;
  }
};

std::ostream& operator<<(std::ostream& os, const Packet& packet) {
  os << fmt::format("version: {} type ID: {} payload: (", packet.version,
                    packet.type_id);

  if (std::holds_alternative<size_t>(packet.payload)) {
    os << fmt::format(" {} )", std::get<0>(packet.payload));
  } else {
    for (auto it{std::get<1>(packet.payload).begin()};
         it != std::get<1>(packet.payload).end(); ++it) {
      os << ' ' << **it;
      if (std::next(it) != std::get<1>(packet.payload).end()) {
        os << ", ";
      }
    }
    os << ')';
  }
  return os;
}

auto part_one(const std::vector<std::string>& input) {
  auto bits = decode(input[0]);
  Packet packet = Packet::fromBits(bits);

  // std::cout << packet << std::endl;
  return packet.version_sum();
}

auto part_two(const std::vector<std::string>& input) {
  auto bits = decode(input[0]);
  Packet packet = Packet::fromBits(bits);

  // std::cout << packet << std::endl;
  return packet.value();
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension.assign(argv[1]);
    extension = "_" + extension;
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_16{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);
  auto answer_two = part_two(input);
  fmt::print("The answer to part two is: {}\n", answer_two);

  return 0;
}
