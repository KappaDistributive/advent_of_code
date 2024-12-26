#pragma once

#include <deque>
#include <iostream>
#include <iterator>
#include <list>
#include <stack>
#include <type_traits>
#include <vector>

#include "input.hpp"

namespace utils {
namespace combinatorics {

template <typename T> class Powerset {
private:
  const std::vector<T> m_data;

  struct Iterator {
    size_t m_mask;
    const std::vector<T> &m_ref;
    explicit Iterator(const std::vector<T> &ref, size_t mask)
        : m_mask(mask), m_ref(ref) {}

    Iterator &operator++() {
      if (m_mask < utils::pow<size_t>(2, this->m_ref.size())) {
        m_mask++;
      }
      return *this;
    }

    std::vector<T> operator*() const {
      std::vector<T> subvector;
      for (size_t index{0}; index < this->m_ref.size(); ++index) {
        if ((1 << index) & this->m_mask) {
          subvector.push_back(this->m_ref[index]);
        }
      }

      return subvector;
    }

    friend bool operator==(const Iterator &lhs, const Iterator &rhs) {
      return lhs.m_mask == rhs.m_mask && lhs.m_ref == rhs.m_ref;
    }

    friend bool operator!=(const Iterator &lhs, const Iterator &rhs) {
      return !(lhs == rhs);
    }
  };

public:
  explicit Powerset(const std::vector<T> &data) : m_data(data) {}

  Iterator begin() const { return Iterator(this->m_data, 0); }

  Iterator end() const {
    return Iterator(this->m_data, utils::pow<size_t>(2, this->m_data.size()));
  }
};

// Produces all possible combinations of `size` numbers of
// elements assigned to values from 0 to `max_size`i, i.e.
// 0        0 0
// 1        0 0
// ...
// max_size 0 0
// 0        1 0
// 1        1 0
// 2        1 0
// ...
std::vector<std::vector<int>> all_combinations(const size_t size, const int max_size) {
  std::vector<std::vector<int>> result;
  std::vector<int> sequence(size, 0);
  result.push_back(sequence);
  while (!std::all_of(sequence.begin(), sequence.end(),
                      [max_size](int i) { return i == max_size; })) {
    for (size_t index{0}; index < sequence.size(); ++index) {
      if (sequence[index] < max_size) {
        sequence[index] += 1;
        break;
      } else {
        sequence[index] = 0;
      }
    }
    result.push_back(sequence);
  }
  return result;
}

template <typename T>
std::vector<std::vector<T>> all_combinations(const size_t size,
                                             const std::vector<T> &elements) {
  assert(!elements.empty());
  auto combs = all_combinations(size, elements.size() - 1);
  std::vector<std::vector<T>> result;
  for (const auto &comb : combs) {
    std::vector<T> temp;
    for (const auto &c : comb) {
      temp.push_back(elements[c]);
    }
    result.push_back(temp);
  }
  return result;
}

} // namespace combinatorics
} // namespace utils
