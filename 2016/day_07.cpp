#include "../utils/input.hpp"

bool is_abba(const std::string& candidate) {
  for (size_t index{0}; index + 3 < candidate.size(); index++) {
    if (candidate[index] != candidate[index + 1] &&
        candidate[index] == candidate[index + 3] &&
        candidate[index + 1] == candidate[index + 2]) {
      return true;
    }
  }
  return false;
}

std::set<std::string> get_abas(const std::string& candidate) {
  std::set<std::string> abas;
  for (size_t index{0}; index + 2 < candidate.size(); index++) {
    if (candidate[index] != candidate[index + 1] &&
        candidate[index] == candidate[index + 2]) {
      abas.insert(candidate.substr(index, 3));
    }
  }
  return abas;
}

int part_one(const std::vector<std::string>& input) {
  std::regex re_outside{"\\]\\w+|\\w+\\["};
  std::regex re_inside{"\\[\\w+\\]"};
  std::smatch matches;
  int result{0};
  bool is_valid;

  for (auto line : input) {
    is_valid = true;
    for (auto it = std::sregex_iterator(line.begin(), line.end(), re_inside);
         it != std::sregex_iterator(); it++) {
      if (is_abba(it->str())) {
        is_valid = false;
        break;
      }
    }
    if (is_valid) {
      is_valid = false;
      for (auto it = std::sregex_iterator(line.begin(), line.end(), re_outside);
           it != std::sregex_iterator(); it++) {
        if (is_abba(it->str())) {
          is_valid = true;
          break;
        }
      }
    }
    if (is_valid) {
      result++;
    }
  }

  return result;
}

int part_two(const std::vector<std::string>& input) {
  std::regex re_outside{"\\]\\w+|\\w+\\["};
  std::regex re_inside{"\\[\\w+\\]"};
  std::smatch matches;
  int result{0};

  for (auto line : input) {
    std::set<std::string> abas;
    for (auto it = std::sregex_iterator(line.begin(), line.end(), re_outside);
         it != std::sregex_iterator(); it++) {
      abas.merge(get_abas(it->str()));
    }
    for (auto it = std::sregex_iterator(line.begin(), line.end(), re_inside);
         it != std::sregex_iterator(); it++) {
      std::string candidate = it->str();
      for (std::string aba : abas) {
        std::string bab;
        bab.push_back(aba[1]);
        bab.push_back(aba[0]);
        bab.push_back(aba[1]);
        if (candidate.find(bab) != std::string::npos) {
          result++;
          break;
        }
      }
    }
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_07.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
  return 0;
}

