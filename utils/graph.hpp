#pragma once

#include <iostream>
#include <map>
#include <set>
#include <utility>

namespace utils {
namespace graph {

template <typename T>
class Node {
 private:
  T m_data;

 public:
  explicit Node(const T& data);

  Node(const Node<T>& node);

  bool operator<(const Node<T>& other) const;

  bool operator==(const Node<T>& other) const;

  bool operator!=(const Node<T>& other) const;

  const T& getData() const;

  T& getData();
  template<typename S>
  friend std::ostream& operator<<(std::ostream& os, const Node<S>& node);
};


template<typename T>
class DirectedGraph {
 private:
  std::set<Node<T>> m_vertices;
  std::set<std::pair<const Node<T>&, const Node<T>&>> m_edges;

 public:
  DirectedGraph();

  bool areConnected(const Node<T>& origin,
                    const Node<T>& destination,
                    bool ignore_direction = false) const;

  const Node<T>& findNodeByData(const T& data) const;

  const Node<T>& addNode(const Node<T>& node);

  void addEdge(const Node<T>& lhs, const Node<T>& rhs);

  // Returns number of vertices.
  size_t size() const;

};

}  // namespace graph
}  // namespace utils

#include "graph.tpp"
