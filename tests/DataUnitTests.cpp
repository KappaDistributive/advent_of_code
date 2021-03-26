#include <gtest/gtest.h>

#include "../utils/data.hpp"

namespace {
TEST(Node, Instantiation) {
  std::string want{"one"};
  utils::Node<std::string> node("one");
  auto got = node.getData();

  EXPECT_EQ(want, got);
}

TEST(Node, SetData) {
  std::string want{"two"};
  utils::Node<std::string> node("one");
  node.setData("two");
  auto got = node.getData();

  EXPECT_EQ(want, got);
}

TEST(Node, AddChild) {
  utils::Node<std::string> node("one");
  size_t want{0};
  auto got = node.addChild(utils::Node<std::string>("two"));

  EXPECT_EQ(want, got);
}

TEST(Node, GetChild) {
  utils::Node<std::string> node("one");
  std::string want{"two"};
  node.addChild(utils::Node<std::string>("two"));
  auto got = node.getChild(0).getData();

  EXPECT_EQ(want, got);
}

}  // namespace
