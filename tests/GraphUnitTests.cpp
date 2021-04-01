#include <gtest/gtest.h>
#include <sstream>
#include <string>

#include "../utils/graph.hpp"

namespace  {
using utils::graph::DirectedGraph;
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

TEST(DirectedGraph, AddNode_test_0) {
  DirectedGraph<std::string> graph;
  Node<std::string> want("want");
  auto got = graph.addNode(want);

  EXPECT_EQ(want, got);
}

TEST(DirectedGraph, AddNode_test_1) {
  DirectedGraph<int> graph;
  Node<int> want(1);
  graph.addNode(want);
  auto got = graph.findNodeByData(want.getData());

  EXPECT_EQ(want, got);
}

TEST(DirectedGraph, AddEdge_test_0) {
  DirectedGraph<int> graph;

  for (int i{1}; i < 10; i++) {
    for (int j{i+1}; j < 10; j++) {
      auto lhs = graph.addNode(Node<int>(i));
      auto rhs = graph.addNode(Node<int>(j));
      if (j % i == 0) {
        graph.addEdge(lhs, rhs);
      }
    }
  }

  for (int i{1}; i < 10; i++) {
    for (int j{i+1}; j < 10; j++) {
        auto lhs = graph.findNodeByData(i);
        auto rhs = graph.findNodeByData(j);

        EXPECT_EQ(graph.areConnected(lhs, rhs), j % i == 0);
        EXPECT_EQ(graph.areConnected(lhs, rhs, true),
                  (j % i == 0) || (j % i == 0));
    }
  }
}

TEST(DirectedGraph, GetComponent_test_0) {
  DirectedGraph<int> graph;
  std::vector<Node<int>> nodes;

  for (int i{0}; i < 10; i++) {
    Node<int> temp(i);
    const Node<int>& node = graph.addNode(temp);
    nodes.push_back(node);
  }
  graph.addEdge(nodes[0], nodes[1]);
  graph.addEdge(nodes[1], nodes[2]);
  graph.addEdge(nodes[1], nodes[3]);
  graph.addEdge(nodes[3], nodes[4]);

  graph.addEdge(nodes[5], nodes[4]);
  graph.addEdge(nodes[5], nodes[7]);
  graph.addEdge(nodes[7], nodes[8]);
  graph.addEdge(nodes[7], nodes[9]);

  std::set<Node<int>> want;
  for (int i{0}; i < 5; i++) {
    want.insert(graph.findNodeByData(nodes[i].getData()));
  }
  auto got = graph.getComponent(graph.findNodeByData(nodes[0].getData()));

  EXPECT_EQ(want, got);
}

}  // namespace
