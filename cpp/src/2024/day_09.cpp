#include "../utils/input.hpp"

class Disk {
private:
  std::vector<int64_t> m_data;

public:
  Disk(const std::vector<int64_t> &input) {
    bool is_file{true};
    int64_t current_id{0};
    for (auto entry : input) {
      for (int64_t i{0}; i < entry; ++i) {
        m_data.push_back(is_file % 2 != 0 ? current_id : -1);
      }
      is_file = !is_file;
      if (is_file)
        ++current_id;
    }
  }

  void bit_compact() {
    int64_t left{0}, right{0};
    for (int64_t index{0}; index < static_cast<int64_t>(this->m_data.size());
         ++index) {
      if (this->m_data[index] == -1) {
        left = index;
        break;
      }
    }
    for (int64_t index{static_cast<int64_t>(this->m_data.size()) - 1};
         index > 0; --index) {
      if (this->m_data[index] != -1) {
        right = index;
        break;
      }
    }
    while (left < right) {
      this->m_data[left] = this->m_data[right];
      this->m_data[right] = -1;
      while (left < static_cast<int64_t>(this->m_data.size()) &&
             this->m_data[left] != -1)
        ++left;
      while (right > 0 && this->m_data[right] == -1)
        --right;
    }
  }

  std::pair<int64_t, int64_t> find_file(int64_t max_index) {
    int64_t index{0}, size{0};
    for (int64_t i{max_index}; i >= 0; --i) {
      if (this->m_data[i] != -1) {
        index = i;
        size = 0;
        while (i > 0 && this->m_data[i] == this->m_data[index]) {
          ++size;
          --i;
        }
        break;
      }
    }
    return {index, size};
  }

  int64_t gap_size(int64_t index) {
    int64_t size{0};
    while (index < static_cast<int64_t>(this->m_data.size()) &&
           this->m_data[index] == -1) {
      ++size;
      ++index;
    }
    return size;
  }

  int64_t gap(int64_t min_size) {
    int64_t index{0};
    while (index < static_cast<int64_t>(this->m_data.size()) &&
           gap_size(index) < min_size) {
      index += gap_size(index) + 1;
    }
    return index;
  }

  void file_compact() {
    int64_t right_index{static_cast<int64_t>(this->m_data.size()) - 1};
    do {
      auto [index, size] = find_file(right_index);
      right_index = index;
      auto gap_index = gap(size);

      if (gap_index < index) {
        for (int64_t i{0}; i < size; ++i) {
          this->m_data[gap_index + i] = this->m_data[index - i];
          this->m_data[index - i] = -1;
        }
      }
      right_index -= size;
    } while (right_index != 0 && this->m_data[right_index] != 0);
  }

  int64_t checksum() const {
    int64_t result{0};
    for (int64_t pos{0}; pos < static_cast<int64_t>(this->m_data.size());
         ++pos) {
      if (this->m_data[pos] != -1) {
        result += pos * this->m_data[pos];
      }
    }
    return result;
  }

  friend std::ostream &operator<<(std::ostream &os, const Disk &disk) {
    for (auto entry : disk.m_data) {
      os << (entry == -1 ? "." : std::to_string(entry)) << ' ';
    }
    return os;
  }
};

auto part_one(const std::vector<int64_t> &input) {
  Disk disk(input);
  disk.bit_compact();
  return disk.checksum();
}

auto part_two(const std::vector<int64_t> &input) {
  Disk disk(input);
  disk.file_compact();
  return disk.checksum();
}

std::vector<int64_t> parse(const std::vector<std::string> &input) {
  assert(input.size() == 1);
  std::vector<int64_t> result;
  for (auto c : input[0]) {
    result.push_back(c - '0');
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2024/input_09_mock.txt"};
  std::filesystem::path input_path{"../../data/2024/input_09.txt"};
  utils::Reader reader(input_path);
  auto input = parse(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input))
            << std::endl;

  return 0;
}
