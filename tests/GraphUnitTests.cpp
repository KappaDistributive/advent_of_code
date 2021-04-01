#include <gtest/gtest.h>
#include <sstream>
#include <string>

#include "../utils/graph.hpp"

namespace  {
using utils::graph::Graph;
using utils::graph::Node;

TEST(Node, Copy) {
  Node<int> want(1);
  Node got(want);

  EXPECT_EQ(want, got);
}

TEST(Node, Equality_test_0) {
  EXPECT_EQ(Node<int>(3), Node<int>(3));
}

TEST(Node, Equality_test_1) {
  EXPECT_EQ(Node<std::string>("node"), Node<std::string>("node"));
}

TEST(Node, Inequality_test_0) {
  EXPECT_NE(Node<std::string>("node"), Node<std::string>("mode"));
}

TEST(Node, Print_test_0) {
  Node<std::string> node("test");
  std::string want{"Node<data: test>"};
  std::stringstream ss;
  ss << node;
  std::string got{ss.str()};

  EXPECT_EQ(want, got);
}

}  // namespace
