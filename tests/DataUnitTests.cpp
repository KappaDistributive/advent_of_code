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

TEST(Node, Equality_test0) {
  utils::Node<std::string> node("one");

  EXPECT_TRUE(node == node);
  EXPECT_FALSE(node != node);
}

TEST(Node, Equality_test1) {
  utils::Node<std::string> lhs("one");
  utils::Node<std::string> rhs("one");

  EXPECT_TRUE(lhs == rhs);
  EXPECT_FALSE(lhs != rhs);
}

TEST(Node, Equality_test2) {
  utils::Node<std::string> lhs("lhs");
  utils::Node<std::string> rhs("rhs");

  EXPECT_FALSE(lhs == rhs);
  EXPECT_TRUE(lhs != rhs);
}

TEST(Node, Equality_test3) {
  utils::Node<std::string> lhs("lhs");
  lhs.addChild(utils::Node<std::string>("child"));
  utils::Node<std::string> rhs("rhs");

  EXPECT_FALSE(lhs == rhs);
  EXPECT_TRUE(lhs != rhs);
}

TEST(Node, Equality_test4) {
  utils::Node<std::string> lhs("one");
  lhs.addChild(utils::Node<std::string>("child"));
  utils::Node<std::string> rhs("one");
  rhs.addChild(utils::Node<std::string>("child"));

  EXPECT_TRUE(lhs == rhs);
  EXPECT_FALSE(lhs != rhs);
}

TEST(Node, Equality_test5) {
  utils::Node<std::string> lhs("one");
  lhs.addChild(utils::Node<std::string>("child"));
  utils::Node<std::string> rhs("one");
  rhs.addChild(utils::Node<std::string>("missfit"));

  EXPECT_FALSE(lhs == rhs);
  EXPECT_TRUE(lhs != rhs);
}

}  // namespace
