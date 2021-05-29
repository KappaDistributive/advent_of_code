#include <algorithm>
#include <numeric>
#include <optional>
#include <set>

#include "../utils/input.hpp"


std::vector<size_t>
prepare_input(const std::vector<std::string>& input) {
  std::vector<size_t> weights;
  for (auto line : input) {
    weights.push_back(std::stoull(line));
  }
  std::reverse(weights.begin(), weights.end());

  return weights;
}

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


bool
is_valid(const std::vector<size_t>& candidate,
         const std::vector<size_t>& weights,
         size_t target_weight) {
  std::vector<size_t> remaining_weights;
  for (auto weight : weights) {
    if (std::find(candidate.begin(), candidate.end(), weight) == candidate.end()) {
      remaining_weights.push_back(weight);
    }
  }

  Powerset powerset{remaining_weights};
  for (auto subvector : powerset) {
    if (std::accumulate(subvector.begin(), subvector.end(), size_t{0}) == target_weight) {
      return true;
    }
  }
  return false;
}


size_t
quantum_entanglement(const std::vector<size_t> group) {
  return std::accumulate(group.begin(), group.end(), size_t{1}, std::multiplies<size_t>());
}


auto
part_one(const std::vector<std::string>& input) {
  auto weights = prepare_input(input);
  size_t target_weight = std::accumulate(weights.begin(), weights.end(), size_t{0}) / 3;
  Powerset powerset(weights);
  std::set<std::vector<size_t>> candidates;
  size_t max_size{weights.size()};

  for (auto subvector : powerset) {
    if (subvector.size() > max_size ||
        std::accumulate(subvector.begin(), subvector.end(), size_t{0}) != target_weight) {
      continue;
    }
    if (is_valid(subvector, weights, target_weight)) {
      if (subvector.size() < max_size) {
        candidates = std::set<std::vector<size_t>>{{subvector}};
        max_size = subvector.size();
      } else {
        candidates.insert(subvector);
      }
      std::cout << max_size << ": " << candidates.size() << std::endl;
    }
  }

  size_t min_entanglement{quantum_entanglement(weights)};
  for (auto candidate : candidates) {
    if (candidate.size() > max_size) {
      continue;
    }
    auto entanglement = quantum_entanglement(candidate);
    if (entanglement < min_entanglement) {
      min_entanglement = entanglement;
    }
  }

  return min_entanglement;
}


// auto
// part_two(const std::vector<std::string>& input) {
//   CPU cpu(input);
//   cpu.set_memory('a', 1);
//   cpu.run();
//   return cpu.get_memory().at('b');
// }


int
main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_24.txt"));
  const auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}

