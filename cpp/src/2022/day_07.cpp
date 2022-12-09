#include <numeric>
#include <queue>

#include "../utils/input.hpp"

struct Directory;

struct INode {
  std::string name;

  INode(std::string name) : name(name) {}
  virtual ~INode() = default;

  virtual size_t total_size() const noexcept = 0;

  virtual std::string str(size_t depth = 0) const noexcept = 0;

  virtual void
  visit_dirs(const std::function<void(const Directory &)> &visitor) const = 0;
};

struct File : public INode {
  size_t size;

  File(std::string name, size_t size) : INode(name), size(size) {}

  size_t total_size() const noexcept override { return this->size; }

  std::string str(size_t depth = 0) const noexcept override {
    return std::string(depth * 2, ' ') +
           fmt::format("- {} (file, size={})\n", this->name, this->size);
  }

  void visit_dirs(
      const std::function<void(const Directory &)> &visitor) const override {}
};

struct Directory : public INode {
  std::vector<std::unique_ptr<INode>> nodes{};

  Directory(std::string name) : INode(name) {}

  size_t total_size() const noexcept override {
    return std::transform_reduce(
        std::cbegin(nodes), std::cend(nodes), size_t{0}, std::plus<>(),
        [](const auto &node) { return node->total_size(); });
  }

  std::string str(size_t depth = 0) const noexcept override {
    std::string result =
        std::string(depth * 2, ' ') + fmt::format("- {} (dir)\n", this->name);
    for (const auto &node : this->nodes) {
      result += node->str(depth + 1);
    }
    return result;
  }

  void visit_dirs(
      const std::function<void(const Directory &)> &visitor) const override {
    visitor(*this);
    for (const auto &node : this->nodes) {
      node->visit_dirs(visitor);
    }
  }
};

std::unique_ptr<Directory> parse_directory(std::queue<std::string> &input,
                                           std::string name) {
  auto result = std::make_unique<Directory>(name);

  while (input.front()[0] == '$')
    input.pop();

  std::vector<std::pair<std::string, size_t>> subdirs;
  while (!input.empty() && input.front()[0] != '$') {
    if (input.front().starts_with("dir ")) {
      // directory
      subdirs.emplace_back(
          std::make_pair(input.front().substr(4), result->nodes.size()));
      result->nodes.emplace_back(nullptr);
    } else {
      // file
      auto splits = utils::split_string(input.front(), ' ');
      assert(splits.size() == 2);
      result->nodes.emplace_back(
          std::make_unique<File>(splits[1], std::stoull(splits[0])));
    }
    input.pop();
  }

  for (auto [subdir_name, index] : subdirs) {
    result->nodes[index] = parse_directory(input, subdir_name);
  }

  return result;
}

auto part_one(std::queue<std::string> input) {
  auto root = parse_directory(input, "/");
  fmt::print("{}", root->str());
  size_t result{0};
  root->visit_dirs([&result](const Directory &dir) {
    if (dir.total_size() <= 100000)
      result += dir.total_size();
  });
  return result;
}

auto part_two(std::queue<std::string> input) {
  const size_t disk_size{70000000};
  const size_t required_size{30000000};
  auto root = parse_directory(input, "/");
  size_t required_diff = root->total_size() + required_size - disk_size;

  size_t result{root->total_size()};
  root->visit_dirs([&result, required_diff](const Directory &dir) {
    if (dir.total_size() >= required_diff)
      result = std::min(result, dir.total_size());
  });

  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_07_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_07.txt"};
  utils::Reader reader(input_path);
  auto lines = reader.get_lines();
  std::queue<std::string> input;
  for (auto it{lines.begin()}; it != lines.end(); ++it) {
    input.push(*it);
  }

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
