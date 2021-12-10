#include "../utils/input.hpp"

class Entry {
 private:
  std::string raw;
  std::vector<int> codes;
  std::string decoded;

  std::vector<int> expand(std::string entry) {
    std::vector<int> result;
    for (size_t index{0}; index < entry.size(); index++) {
      if (entry[index] != '\\') {
        result.push_back(entry[index]);
      } else if (index + 1 < entry.size() && entry[index + 1] != 'x') {
        result.push_back(entry[index + 1]);
        index++;
      } else if (index + 3 < entry.size()) {
        result.push_back(std::stoi(entry.substr(index + 2, 2), 0, 16));
        index += 3;
      } else {
        throw std::runtime_error("This should never happen.");
      }
    }

    return result;
  }

  std::string decode(std::string raw) {
    std::string result{"\""};
    for (auto character : raw) {
      if (character == '\\') {
        result += "\\\\";
      } else if (character == '\"') {
        result += "\\\"";
      } else {
        result += character;
      }
    }
    result += "\"";
    return result;
  }

 public:
  explicit Entry(std::string entry) : raw(entry) {
    codes = expand(raw.substr(1, raw.size() - 2));
    decoded = decode(raw);
  }

  std::string get_raw() const { return raw; }

  std::vector<int> get_codes() const { return codes; }

  std::string get_decoded() const { return decoded; }
};

std::ostream& operator<<(std::ostream& os, const Entry& entry) {
  os << "Raw     : " << entry.get_raw() << "\n"
     << "Expanded: [ ";
  for (auto code : entry.get_codes()) {
    os << code << ",";
  }
  os << " ]\n";
  os << "Decoded : " << entry.get_decoded() << std::endl;

  return os;
}

std::vector<Entry> parse_input(std::vector<std::string> input) {
  std::vector<Entry> result;

  for (auto entry : input) {
    result.push_back(Entry(entry));
  }

  return result;
}

int part_one(std::vector<std::string> input) {
  auto entries = parse_input(input);
  int result{0};

  for (auto entry : entries) {
    result += static_cast<int>(entry.get_raw().size()) -
              static_cast<int>(entry.get_codes().size());
  }
  return result;
}

int part_two(std::vector<std::string> input) {
  auto entries = parse_input(input);
  int result{0};

  for (auto entry : entries) {
    result += static_cast<int>(entry.get_decoded().size()) -
              static_cast<int>(entry.get_raw().size());
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2015/input_08.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

