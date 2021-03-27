#include <gtest/gtest.h>
#include <sstream>

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
  utils::Node<std::string> want("two");
  auto got = *node.addChild(want);

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

TEST(Node, Print_test0) {
  utils::Node<std::string> node("one");
  std::string want{"Node<data: one; #children: 0>"};
  std::stringstream ss;
  ss << node;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(Node, Print_test1) {
  utils::Node<int> node(-1);
  node.addChild(utils::Node<int>(1));
  std::string want{"Node<data: -1; #children: 1>"};
  std::stringstream ss;
  ss << node;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Dereference) {
  using Node = utils::Node<int>;
  Node node(1);
  auto it = node.begin();
  auto want = node.getData();
  auto got = (*it)->getData();

  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Successor_test0) {
  using Node = utils::Node<int>;
  Node node(1);
  Node child(2);
  node.addChild(child);
  auto it = node.begin();
  auto want = child.getData();
  auto got = (*++it)->getData();

  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Successor_test1) {
  using Node = utils::Node<int>;
  Node node(1);
  auto child = node.addChild(Node(2));
  child->addChild(Node(3));
  child = node.addChild(Node(4));
  child->addChild(Node(5));

  auto want = std::vector<int>{1, 2, 3, 4, 5};
  std::vector<int> got;
  for (auto it = node.begin(); it != node.end(); it++) {
    got.push_back((*it)->getData());
  }
  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Successor_test2) {
  using Node = utils::Node<int>;
  Node node(1);
  auto child = node.addChild(Node(2));
  child->addChild(Node(3));
  child = node.addChild(Node(4));
  child->addChild(Node(5));

  auto want = std::vector<int>{1, 2, 3, 4, 5};
  std::vector<int> got;
  for (auto node_value : node) {
    got.push_back(node_value->getData());
  }
  EXPECT_EQ(want, got);
}

TEST(Tree, Initialization) {
  using Node = utils::Node<int>;
  Node want{1};
  utils::Tree<Node> tree(want);
  auto got = tree.getRoot();

  EXPECT_EQ(want, got);
}

TEST(Tree, Equality_test0) {
  using Node = utils::Node<int>;
  utils::Tree<Node> tree(utils::Node<int>{1});

  EXPECT_TRUE(tree == tree);
  EXPECT_FALSE(tree != tree);
}

TEST(Tree, Equality_test1) {
  using Node = utils::Node<int>;
  utils::Tree<Node> tree(utils::Node<int>{1});
  utils::Tree<Node> other(utils::Node<int>{1});

  EXPECT_TRUE(tree == other);
  EXPECT_FALSE(tree != other);
}

TEST(Tree, Equality_test2) {
  using Node = utils::Node<int>;
  utils::Tree<Node> tree(utils::Node<int>{1});
  tree.getRoot().addChild(utils::Node<int>{2});
  utils::Tree<Node> other(utils::Node<int>{1});

  EXPECT_FALSE(tree == other);
  EXPECT_TRUE(tree != other);
}

}  // namespace
