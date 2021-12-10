#include <gtest/gtest.h>
#include <sstream>

#include "../src/utils/tree.hpp"

namespace  {
using utils::tree::Node;
using utils::tree::Tree;

TEST(Node, Instantiation) {
  std::string want{"one"};
  Node<std::string> node("one");
  auto got = node.getData();

  EXPECT_EQ(want, got);
}

TEST(Node, SetData) {
  std::string want{"two"};
  Node<std::string> node("one");
  node.setData("two");
  auto got = node.getData();

  EXPECT_EQ(want, got);
}

TEST(Node, AddChild) {
  Node<std::string> node("one");
  Node<std::string> want("two");
  auto got = *node.addChild(want);

  EXPECT_EQ(want, got);
}

TEST(Node, GetChild) {
  Node<std::string> node("one");
  std::string want{"two"};
  node.addChild(Node<std::string>("two"));
  auto got = node.getChild(0).getData();

  EXPECT_EQ(want, got);
}

TEST(Node, Size_test0) {
  Node<std::string> node("one");
  size_t want{1};
  auto got = node.size();

  EXPECT_EQ(want, got);
}

TEST(Node, Size_test1) {
  /*
  *           (1)
  *          /    \
  *        (2)     (4)
  *       /     /   |   \
  *     (3)   (5)  (6)  (7)
  */
  using Node = Node<int>;
  Node node(1);
  auto node_two = node.addChild(Node(2));
  node_two->addChild(Node(3));
  auto node_four = node.addChild(Node(4));
  node_four->addChild(Node(5));
  node_four->addChild(Node(6));
  node_four->addChild(Node(7));

  size_t want{7};
  auto got = node.size();

  EXPECT_EQ(want, got);
}

TEST(Node, Equality_test0) {
  Node<std::string> node("one");

  EXPECT_TRUE(node == node);
  EXPECT_FALSE(node != node);
}

TEST(Node, Equality_test1) {
  Node<std::string> lhs("one");
  Node<std::string> rhs("one");

  EXPECT_TRUE(lhs == rhs);
  EXPECT_FALSE(lhs != rhs);
}

TEST(Node, Equality_test2) {
  Node<std::string> lhs("lhs");
  Node<std::string> rhs("rhs");

  EXPECT_FALSE(lhs == rhs);
  EXPECT_TRUE(lhs != rhs);
}

TEST(Node, Equality_test3) {
  Node<std::string> lhs("lhs");
  lhs.addChild(Node<std::string>("child"));
  Node<std::string> rhs("rhs");

  EXPECT_FALSE(lhs == rhs);
  EXPECT_TRUE(lhs != rhs);
}

TEST(Node, Equality_test4) {
  Node<std::string> lhs("one");
  lhs.addChild(Node<std::string>("child"));
  Node<std::string> rhs("one");
  rhs.addChild(Node<std::string>("child"));

  EXPECT_TRUE(lhs == rhs);
  EXPECT_FALSE(lhs != rhs);
}

TEST(Node, Equality_test5) {
  Node<std::string> lhs("one");
  lhs.addChild(Node<std::string>("child"));
  Node<std::string> rhs("one");
  rhs.addChild(Node<std::string>("missfit"));

  EXPECT_FALSE(lhs == rhs);
  EXPECT_TRUE(lhs != rhs);
}

TEST(Node, Print_test0) {
  Node<std::string> node("one");
  std::string want{"Node<data: one; #children: 0>"};
  std::stringstream ss;
  ss << node;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(Node, Print_test1) {
  Node<int> node(-1);
  node.addChild(Node<int>(1));
  std::string want{"Node<data: -1; #children: 1>"};
  std::stringstream ss;
  ss << node;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Dereference) {
  using Node = Node<int>;
  Node node(1);
  auto it = node.begin();
  auto want = node.getData();
  auto got = (*it)->getData();

  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Successor_test0) {
  using Node = Node<int>;
  Node node(1);
  Node child(2);
  node.addChild(child);
  auto it = node.begin();
  auto want = child.getData();
  auto got = (*++it)->getData();

  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Successor_test1) {
  /*
  *           (1)
  *          /    \
  *        (2)     (4)
  *       /     /   |   \
  *     (3)   (5)  (6)  (7)
  */
  using Node = Node<int>;
  Node node(1);
  auto node_two = node.addChild(Node(2));
  node_two->addChild(Node(3));
  auto node_four = node.addChild(Node(4));
  node_four->addChild(Node(5));
  node_four->addChild(Node(6));
  node_four->addChild(Node(7));

  auto want = std::vector<int>{1, 2, 3, 4, 5, 6, 7};

  std::vector<int> got;
  for (auto it = node.begin(); it != node.end(); it++) {
    got.push_back((*it)->getData());
  }
  EXPECT_EQ(want, got);
}

TEST(NodeIterator, Successor_test2) {
  /*
  *           (1)
  *          /    \
  *        (2)     (4)
  *       /     /   |   \
  *     (3)   (5)  (6)  (7)
  */
  using Node = Node<int>;
  Node node(1);
  auto node_two = node.addChild(Node(2));
  node_two->addChild(Node(3));
  auto node_four = node.addChild(Node(4));
  node_four->addChild(Node(5));
  node_four->addChild(Node(6));
  node_four->addChild(Node(7));

  auto want = std::vector<int>{1, 2, 3, 4, 5, 6, 7};

  std::vector<int> got;
  for (auto node_value : node) {
    got.push_back(node_value->getData());
  }
  EXPECT_EQ(want, got);
}

TEST(Tree, Initialization) {
  using Node = Node<int>;
  Node want{1};
  Tree<Node> tree(want);
  auto got = tree.getRoot();

  EXPECT_EQ(want, got);
}

TEST(Tree, Equality_test0) {
  using Node = Node<int>;
  Tree<Node> tree(Node{1});

  EXPECT_TRUE(tree == tree);
  EXPECT_FALSE(tree != tree);
}

TEST(Tree, Equality_test1) {
  using Node = Node<int>;
  Tree<Node> tree(Node{1});
  Tree<Node> other(Node{1});

  EXPECT_TRUE(tree == other);
  EXPECT_FALSE(tree != other);
}

TEST(Tree, Equality_test2) {
  using Node = Node<int>;
  Tree<Node> tree(Node{1});
  tree.getRoot().addChild(Node{2});
  Tree<Node> other(Node{1});

  EXPECT_FALSE(tree == other);
  EXPECT_TRUE(tree != other);
}

TEST(Tree, FindByData_test0) {
  using Node = Node<std::string>;
  Tree<Node> tree(Node("root"));

  std::vector<Node*> want{&tree.getRoot()};
  auto got = tree.findByData(Node("root"));

  EXPECT_EQ(want, got);
}

TEST(Tree, FindByData_test1) {
  /*
  *           (3)
  *          /    \
  *        (2)     (4)
  *       /     /   |   \
  *     (3)   (3)  (6)  (7)
  */
  using Node = Node<int>;
  Tree<Node> tree(Node(3));
  auto match_0 = &tree.getRoot();
  auto node_two = match_0->addChild(Node(2));
  auto match_1 = node_two->addChild(Node(3));
  auto node_four = match_0->addChild(Node(4));
  auto match_2 = node_four->addChild(Node(3));
  node_four->addChild(Node(6));
  node_four->addChild(Node(7));

  std::vector<Node*> want = {match_0, match_1, match_2};
  auto got = tree.findByData(Node(3));

  EXPECT_EQ(want, got);
}
}  // namespace
