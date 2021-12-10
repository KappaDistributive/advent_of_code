#pragma once

#include <deque>
#include <list>
#include <iostream>
#include <iterator>
#include <vector>
#include <stack>
#include <type_traits>

#include "input.hpp"

namespace utils {
namespace combinatorics {

template<typename T>
class Powerset {
 private:
  const std::vector<T> m_data;

  struct Iterator {
    size_t m_mask;
    const std::vector<T>& m_ref;
    explicit Iterator(const std::vector<T>& ref, size_t mask)
      : m_mask(mask),
        m_ref(ref) {
    }

    Iterator& operator++() {
      if (m_mask < utils::pow<size_t>(2, this->m_ref.size())) {
        m_mask++;
      }
      return *this;
    }

    std::vector<T>
    operator*() const {
      std::vector<T> subvector;
      for (size_t index{0}; index < this->m_ref.size(); ++index) {
        if ((1 << index) & this->m_mask) {
          subvector.push_back(this->m_ref[index]);
        }
      }

      return subvector;
    }

    friend bool
    operator==(const Iterator& lhs, const Iterator& rhs) {
      return lhs.m_mask == rhs.m_mask && lhs.m_ref == rhs.m_ref;
    }

    friend bool
    operator!=(const Iterator& lhs, const Iterator& rhs) {
      return !(lhs == rhs);
    }
  };

 public:
  explicit Powerset(const std::vector<T>& data)
    : m_data(data) {
  }

  Iterator begin() const {
    return Iterator(this->m_data, 0);
  }

  Iterator end() const {
    return Iterator(this->m_data, utils::pow<size_t>(2, this->m_data.size()));
  }
};

}  // namespace combinatorics
}  // namespace utils

