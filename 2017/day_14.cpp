#include <bitset>
#include <cassert>
#include <sstream>

#include "../utils/input.hpp"
#include "../utils/tree.hpp"

std::vector<int> formKnot(const std::vector<int>& chain,
                          const size_t& start,
                          const size_t& knot_length) {
  assert(start < chain.size());
  assert(knot_length <= chain.size());
  auto knot = utils::rotate_vector(chain, -start);

  for (size_t index{0}; index < knot_length / 2; index++) {
    auto memory = knot[index];
    knot[index] = knot[knot_length - index - 1];
    knot[knot_length - index - 1] = memory;
  }
  return utils::rotate_vector(knot, start);
}

std::tuple<std::vector<int>, size_t, size_t>
knotHashRound(const std::vector<int>& lengths,
              std::vector<int> chain,
              size_t index,
              size_t skip_size) {

  for (auto length : lengths) {
    chain = formKnot(chain, index, length);
    assert(chain.size() == 256);
    index = (index + length + skip_size) % chain.size();
    skip_size++;
  }

  return {chain, index, skip_size};
}


std::vector<int> sparseHash(const std::string& input) {
  static const std::vector<int> additional_lengths{{17, 31, 73, 47, 23}};
  std::vector<int> lengths;
  for (auto entry : input) {
    lengths.push_back(static_cast<int>(entry));
  }
  for (auto length : additional_lengths) {
    lengths.push_back(length);
  }

  std::vector<int> chain;
  for (size_t i{0}; i< 256; i++) {
    chain.push_back(i);
  }

  size_t index{0}, skip_size{0};
  for (size_t round{0}; round < 63; round++) {
    auto update = knotHashRound(lengths, chain, index, skip_size);
    chain = std::get<0>(update);
    index = std::get<1>(update);
    skip_size = std::get<2>(update);
  }

  return std::get<0>(knotHashRound(lengths, chain, index, skip_size));
}

std::string denseHash(const std::string& input) {
  auto sparse_hash = sparseHash(input);
  std::vector<int> raw_hash;

  for (size_t block_index{0}; block_index < 16; block_index++) {
    int entry;
    for (size_t index{0}; index < 16; index++) {
      if (index == 0) {
        entry = sparse_hash[block_index * 16 + index];
      } else {
        entry ^= sparse_hash[block_index * 16 + index];
      }
    }
    raw_hash.push_back(entry);
  }

  std::stringstream ss;
  ss << std::hex;
  for (auto entry : raw_hash) {
    ss << entry;
  }

  return ss.str();
}

std::bitset<128> knotHashToBits(std::string hash) {

  std::string lower = (std::string(32 - hash.size(), '0') +
                       hash).substr(0, 16);
  std::string upper = (std::string(32 - hash.size(), '0') +
                       hash).substr(16, 16);

  std::bitset<64> lower_bit(std::stoull(lower, nullptr, 16));
  std::bitset<64> upper_bit(std::stoull(upper, nullptr, 16));

  std::bitset<128> result(0);
  for (size_t index{0}; index < 64; index++) {
    result[index] = upper_bit[index];
  }
  for (size_t index{0}; index < 64; index++) {
    result[64 + index] = lower_bit[index];
  }

  return result;
}

size_t part_one(const std::string& input) {
  size_t result{0};
  for (auto layer{0}; layer < 128; layer++) {
    auto row  = knotHashToBits(denseHash(input + "-" + std::to_string(layer)));
    for (size_t bit{0}; bit < 128; bit++) {
      result += row[bit];
    }
  }
  return result;
}


std::string part_two(const std::string& input) {
  return denseHash(input);
}


int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_14.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
