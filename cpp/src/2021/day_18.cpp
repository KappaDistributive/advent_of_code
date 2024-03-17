#include <variant>

#include "../utils/input.hpp"

class SnailfishNumber {
 private:
  struct Node {
    std::optional<std::weak_ptr<Node>> parent = std::nullopt;
    std::variant<std::shared_ptr<Node>, int> left = nullptr;
    std::variant<std::shared_ptr<Node>, int> right = nullptr;

    friend std::ostream& operator<<(std::ostream& os,
                                    std::shared_ptr<Node> node) {
      os << '[';

      if (std::holds_alternative<int>(node->left)) {
        os << std::get<int>(node->left);
      } else {
        if (std::get<std::shared_ptr<Node>>(node->left) != nullptr) {
          os << std::get<std::shared_ptr<Node>>(node->left);
        } else {
          os << "n/a";
        }
      }

      os << ", ";

      if (std::holds_alternative<int>(node->right)) {
        os << std::get<int>(node->right);
      } else {
        if (std::get<std::shared_ptr<Node>>(node->right) != nullptr) {
          os << std::get<std::shared_ptr<Node>>(node->right);
        } else {
          os << "n/a";
        }
      }

      os << ']';

      return os;
    }
  };

  std::shared_ptr<Node> m_root;

  static std::optional<std::pair<std::string_view, std::string_view>>
  split_pair(std::string_view description) {
    int depth{0};
    for (auto it{description.begin()}; it != description.end(); ++it) {
      switch (*it) {
        case '[':
          ++depth;
          break;
        case ']':
          --depth;
          break;
        case ',':
          if (depth == 1) {
            return std::make_pair(
                std::string_view(description.begin() + 1,
                                 it - description.begin() - 1),
                std::string_view(it + 1, description.end() - it - 2));
          }
          break;
        default:
          break;
      }
    }

    return std::nullopt;
  }

  std::shared_ptr<Node> create_node(
      std::string_view left, std::string_view right,
      std::optional<std::shared_ptr<Node>> parent = std::nullopt) {
    auto node = std::make_shared<Node>();
    node->parent = parent;

    // left child
    if (std::all_of(left.begin(), left.end(), ::isdigit)) {
      node->left = std::stoi(std::string{left});
    } else {
      auto splits = SnailfishNumber::split_pair(left);
      if (splits.has_value()) {
        auto [left_, right_] = splits.value();
        node->left = create_node(left_, right_, node);
      } else {
        throw std::runtime_error(std::format("Failed to parse: {}\n", left));
      }
    }

    // right child
    if (std::all_of(right.begin(), right.end(), ::isdigit)) {
      node->right = std::stoi(std::string{right});
    } else {
      auto splits = SnailfishNumber::split_pair(right);
      if (splits.has_value()) {
        auto [left_, right_] = splits.value();
        node->right = create_node(left_, right_, node);
      } else {
        throw std::runtime_error(std::format("Failed to parse: {}\n", right));
      }
    }

    return node;
  }

 public:
  explicit SnailfishNumber(const std::string& description) {
    auto splits = this->split_pair(description);
    if (splits.has_value()) {
      auto [left, right] = splits.value();
      this->m_root = create_node(left, right);
    } else {
      throw std::runtime_error(std::format(
          "Failed to create SnailfishNumber from: {}\n", description));
    }
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  const SnailfishNumber& number) {
    os << number.m_root;

    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  for (auto line : input) {
    SnailfishNumber number{line};
    std::cout << number << std::endl;
  }
  return 0;
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      std::format("../../data/2021/input_18{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << std::format("The answer to part one is: {}", answer_one) << std::endl;
  // auto answer_two = part_two(input);
  // std::cout << std::format("The answer to part two is: {}", answer_two) << std::endl;

  return 0;
}
