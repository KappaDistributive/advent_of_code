#pragma once

#include <list>
#include <iostream>
#include <iterator>
#include <vector>
#include <stack>

namespace utils {
namespace tree {
template<typename T>
class Node {
 private:
  T m_data;
  std::list<Node<T>> m_children;

  template <typename NodeType>
  class Iterator {
   private:
    NodeType* m_node;
    std::stack<NodeType*> m_memory;

   public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = NodeType;
    explicit Iterator(NodeType* node);

    Iterator<NodeType>& operator++();
    Iterator operator++(int);

    bool operator==(const Iterator<NodeType>& other) const;
    bool operator!=(const Iterator<NodeType>& other) const;

    NodeType* operator*() const;
  };

 public:
  explicit Node(const T& data);

  const T& getData() const;

  T& getData();

  void setData(const T& data);

  Node<T>* addChild(const Node<T>& child);

  Node<T>& getChild(const size_t& childIndex);

  std::vector<Node<T>*> getChildren();

  size_t size() const;

  bool operator==(const Node<T>& other) const;

  bool operator!=(const Node<T>& other) const;

  Iterator<Node<T>> begin();
  Iterator<Node<T>> end();

  template<typename S>
  friend std::ostream& operator<<(std::ostream& os, const Node<S>& node);
};


template <typename Node>
class Tree {
 private:
  Node m_root;

 public:
  explicit Tree(const Node& root);

  Node& getRoot();

  std::vector<Node*> findByData(const Node& reference);

  bool operator==(const Tree<Node>& other) const;

  bool operator!=(const Tree<Node>& other) const;
};

}  // namespace tree
}  // namespace utils

#include "tree.tpp"
