#include <cassert>
#include <optional>

#include "../utils/input.hpp"
#include "../utils/tree.hpp"

struct Section {
  size_t start;
  size_t end;

  bool operator==(const Section& other) const {
    return this->start == other.start && this->end == other.end;
  }
  bool operator!=(const Section& other) const {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream& os, const Section& section) {
  os << "Section<start: " << section.start
     << ", end: " << section.end << ">";

  return os;
}

using Node = utils::tree::Node<Section>;
using Tree = utils::tree::Tree<Node>;

std::optional<Section> next_group(const std::string& input,
                                  const size_t& start,
                                  const size_t& end
                                ) {
  bool in_garbage{false};
  bool is_escaped{false};
  size_t level{0};
  size_t group_start;

  for (size_t index{start}; index <= end; index++) {
    auto character = input[index];
    if (is_escaped) {
      is_escaped = false;
    } else if (character == '!') {
      is_escaped = true;
    } else if (in_garbage) {
      if (character == '>') {
        in_garbage = false;
      }
    } else if (character == '<') {
      in_garbage = true;
    } else if (character == '{') {
      if (level == 0) {
        group_start = index;
      }
      level++;
    } else if (character == '}') {
      level--;
      if (level == 0) {
        return Section{group_start, index};
      }
    }
  }
  return std::nullopt;
}

std::vector<Section> top_level_groups(const std::string& input,
                                      const size_t& start,
                                      const size_t& end
                                     ) {
  std::vector<Section> groups;
  size_t index = start;
  std::optional<Section> group = std::nullopt;
  do {
    group = next_group(input, index, end);
    if (group.has_value()) {
      groups.push_back(group.value());
      index = group.value().end + 1;
    }
  } while (group.has_value());

  return groups;
}

void next_level(const std::string& input, Node* node) {
  auto section = node->getData();
  assert(input[section.start] == '{');
  assert(input[section.end] == '}');

  auto groups = top_level_groups(input, section.start+1, section.end-1);

  for (auto group : groups) {
    auto child = node->addChild(Node{group});
    next_level(input, child);
  }
}

Tree prepare_input(const std::string& input) {
  Tree tree(Node{Section{0, input.size() - 1}});
  auto roots = tree.findByData(Node{Section{0, input.size() - 1}});
  assert(roots.size() == 1);
  next_level(input, roots[0]);
  return tree;
}

size_t calculateScore(Node* node, size_t level = 0) {
  size_t score{level + 1};
  for (auto child : node->getChildren()) {
    score += calculateScore(child, level + 1);
  }

  return score;
}

int part_one(const std::string& input) {
  bool verbose{false};
  auto tree = prepare_input(input);

  if (verbose) {
    for (auto node : tree.getRoot()) {
      std::cout << node->getData() << std::endl;
    }
  }

  return calculateScore(&tree.getRoot());
}

int part_two(const std::string& input) {
  size_t num_garbage{0};
  bool in_garbage{false};
  bool is_escaped{false};

  for (auto character : input) {
    if (is_escaped) {
      is_escaped = false;
    } else if (character == '!') {
      is_escaped = true;
    } else if (in_garbage) {
      if (character == '>') {
        in_garbage = false;
      } else {
        num_garbage++;
      }
    } else if (character == '<') {
      in_garbage = true;
    }
  }

  return num_garbage;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_09.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
