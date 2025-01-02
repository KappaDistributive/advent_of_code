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

  void defragment() {
    int64_t left{0}, right{0};
    for (int64_t index{0}; index < static_cast<int64_t>(this->m_data.size()); ++index) {
      if (this->m_data[index] == -1) {
        left = index;
        break;
      }
    }
    for (int64_t index{static_cast<int64_t>(this->m_data.size()) - 1}; index > 0;
         --index) {
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

  int64_t checksum() const {
    int64_t result{0};
    for (int64_t pos{0};
         pos < static_cast<int64_t>(this->m_data.size()) && this->m_data[pos] >= 0;
         ++pos) {
      result += pos * this->m_data[pos];
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
  // std::cout << disk << std::endl;
  disk.defragment();
  // std::cout << disk << std::endl;
  return disk.checksum();
}

auto part_two() { return 0; }

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
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return 0;
}
