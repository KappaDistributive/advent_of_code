#pragma once

#include <deque>
#include <list>
#include <iostream>
#include <iterator>
#include <vector>
#include <stack>
#include <type_traits>

namespace utils {

template<typename T>
class Node {
 private:
  T m_data;
  std::list<Node<T>> m_children;

  template <typename NodeType>
  class Iterator : public std::iterator<std::forward_iterator_tag, NodeType> {
   private:
    NodeType* m_node;
    std::stack<NodeType*> m_memory;

   public:
    explicit Iterator(NodeType* node);

    Iterator<NodeType>& operator++();
    Iterator operator++(int);

    bool operator==(const Iterator<NodeType>& other) const;
    bool operator!=(const Iterator<NodeType>& other) const;

    const T& operator*() const;
  };

 public:
  explicit Node(const T& data);

  const T& getData() const;

  void setData(const T& data);

  Node<T>* addChild(const Node<T>& child);

  Node<T>& getChild(const size_t& childIndex);

  bool operator==(const Node<T>& other) const;

  bool operator!=(const Node<T>& other) const;

  Iterator<Node<T>> begin();
  Iterator<Node<T>> end();

  friend std::ostream& operator<<(std::ostream& os, const Node<T>& node) {
    os << "Node<data: " << node.getData()
       << "; #children: " << node.m_children.size() << ">";

    return os;
  }
};


template <typename Node>
class Tree {
 private:
  Node m_root;

 public:
  explicit Tree(const Node& root);

  Node& getRoot();
  bool operator==(const Tree<Node>& other) const;

  bool operator!=(const Tree<Node>& other) const;
};

}  // namespace utils

#include "data.tpp"
