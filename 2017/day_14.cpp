#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <set>
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
    ss << std::setfill('0') << std::setw(2) << entry;
  }

  return ss.str();
}

std::bitset<128> knotHashToBits(std::string hash) {
  assert(hash.size() == 32);
  std::string lower = hash.substr(0, 16);
  std::string upper = hash.substr(16, 16);

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


std::pair<size_t, size_t>
findNewRegion(const std::array<int, 128*128>& ownership) {
  for (size_t y{0}; y < 128; y++) {
    for (size_t x{0}; x < 128; x++) {
      if (ownership[y * 128 + x] == -1) {
        return {x, y};
      }
    }
  }

  throw std::runtime_error("No new region left!");
}


void fillRegion(const std::array<bool, 128*128>& disk,
                size_t x,
                size_t y,
                std::array<int, 128*128>* ownership) {
  assert(disk[y * 128 + x] == true);
  int owner = *std::max_element(ownership->begin(), ownership->end());
  owner++;
  std::cout << "Adding owner: " << owner << std::endl;
  std::set<std::pair<size_t, size_t>> points{{{x, y}}};
  bool expand{true};
  while (expand) {
    std::set<std::pair<size_t, size_t>> new_points;
    for (auto point : points) {
      for (int y{-1}; y <= 1; y++) {
        for (int x{-1}; x <= 1; x++) {
          if (!(x== 0 && y == 0) && (x == 0 || y == 0)) {
            std::pair<int, int> candidate;
            candidate.first = std::min(
              std::max(0, static_cast<int>(point.first) + x), 127);
            candidate.second = std::min(
              std::max(0, static_cast<int>(point.second) + y), 127);

            if (disk[candidate.second * 128 + candidate.first]) {
              new_points.insert({static_cast<size_t>(candidate.first),
                                 static_cast<size_t>(candidate.second)});
            }
          }
        }
      }
    }
    size_t num_points{points.size()};
    for (auto p : new_points) {
      points.insert(p);
    }
    expand = points.size() > num_points;
  }

  for (auto p : points) {
    ownership->operator[](p.second * 128 + p.first) = owner;
  }
}

int countRegions(const std::array<bool, 128*128>& disk) {
  std::array<int, 128*128> ownership;
  ownership.fill(-1);

  // set ownership for empty parts of disk
  for (size_t y{0}; y < 128; y++) {
    for (size_t x{0}; x < 128; x++) {
      if (!disk[y * 128 + x]) {
        ownership[y * 128 + x] = 0;
      }
    }
  }

  while (*std::min_element(ownership.begin(), ownership.end()) < 0) {
    auto [x, y] = findNewRegion(ownership);
    fillRegion(disk, x, y, &ownership);
  }

  return *std::max_element(ownership.begin(), ownership.end());
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


int part_two(const std::string& input) {
  std::array<bool, 128*128> disk;

  for (auto layer{0}; layer < 128; layer++) {
    auto row  = knotHashToBits(denseHash(input + "-" + std::to_string(layer)));
    for (auto x{0}; x < 128; x++) {
      disk[layer * 128 + x] = row[127 - x];
    }
  }

  return countRegions(disk);
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
