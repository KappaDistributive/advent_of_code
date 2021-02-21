#include <cassert>

#include "../utils/input.hpp"

class Password {
 private:
  std::string password;

  void inc(size_t offset) {
    bool carry{false};

    if (offset == password.size()) {
      password = "a" + password;
    } else {
      if (password[password.size() - 1 - offset] == 'z') {
        password[password.size() - 1 - offset] = 'a';
        carry = true;
      } else {
        password[password.size() - 1 - offset]++;
      }
    }

    if (carry) {
      inc(offset+1);
    }
  }

  bool has_increasing_triplet() const {
    if (password.size() < 3) {
      return false;
    }
    for (size_t index{2}; index < password.size(); index++) {
      assert('a' <= password[index-2] && password[index-2] <= 'z');
      assert('a' <= password[index-1] && password[index-1] <= 'z');
      assert('a' <= password[ index ] && password[ index ] <= 'z');
      if (password[index-2] +1 == password[index-1] && password[index-1]+1 == password[index]) {
        return true;
      }
    }
    return false;
  }

  bool has_ambiguous_letter() const {
    for (auto character: password) {
      if (character == 'i' || character == 'o' || character == 'l') {
        return true;
      }
    }
    return false;
  }

  bool has_two_pairs() const {
    for (size_t index_left{0}; index_left + 3 < password.size(); index_left++) {
      for (size_t index_right{index_left+2}; index_right + 1 < password.size(); index_right++) {
        if (password[index_left] == password[index_left+1] && password[index_right] == password[index_right+1]) {
          return true;
        }
      }
    }
    return false;
  }

 public:
  explicit Password(const std::string& password) : password(password) {
  }

  void increment() {
    inc(0);
  }

  bool is_valid() const {
    return (has_increasing_triplet() && !has_ambiguous_letter() && has_two_pairs());
  }

  std::string str() const {
    return password;
  }
};

std::string part_one(const std::string& input) {
  Password password{input};
  while (!password.is_valid()) {
    password.increment();
  }
  return password.str();
}

std::string part_two(const std::string& input) {
  Password password{part_one(input)};
  password.increment();
  while (!password.is_valid()) {
    password.increment();
  }
  return password.str();
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_11.txt"));
  auto input = reader.get_lines()[0];

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

