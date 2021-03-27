#include "data.hpp"

namespace utils {

template<typename T>
Node<T>::Node(const T& data)
  : m_data(data) {
}

template<typename T>
const T& Node<T>::getData() const {
  return m_data;
}

template<typename T>
void Node<T>::setData(const T& data) {
  m_data = data;
}

template<typename T>
Node<T>* Node<T>::addChild(const Node<T>& node) {
  m_children.emplace_back(node);

  return &m_children.back();
}

template<typename T>
Node<T>& Node<T>::getChild(const size_t& childIndex) {
  auto it = m_children.begin();
  std::advance(it, childIndex);
  return *it;
}

template<typename T>
bool Node<T>::operator==(const Node<T>& other) const {
  bool equal{true};
  if (this->getData() != other.getData()) {
    equal = false;
  } else if (this->m_children.size() != other.m_children.size()) {
    equal = false;
  } else {
    auto it_lhs = this->m_children.begin();
    auto it_rhs = other.m_children.begin();
    for (size_t childIndex{0}; childIndex < this->m_children.size(); childIndex++) {
      if (*it_lhs != *it_rhs) {
        equal = false;
        break;
      }
      it_lhs++;
      it_rhs++;
    }
  }

  return equal;
}

template<typename T>
bool Node<T>::operator!=(const Node<T>& other) const {
  return !(*this == other);
}

template<typename T>
typename Node<T>::template Iterator<Node<T>>
Node<T>::begin() {
  return Iterator<Node<T>>(this);
}

template<typename T>
typename Node<T>::template Iterator<Node<T>>
Node<T>::end() {
  return Iterator<Node<T>>(nullptr);
}

template<typename T>
template<typename NodeType>
Node<T>::Iterator<NodeType>::Iterator(NodeType* node)
  : m_node(node) {
  }

template<typename T>
template<typename NodeType>
typename Node<T>::template Iterator<NodeType>&
Node<T>::Iterator<NodeType>::operator++() {
  // perform depth-first search
  const size_t numChildren = this->m_node->m_children.size();
    if(numChildren > 0) {
      if(numChildren > 1) {
        for(auto it = this->m_node->m_children.rbegin();
            it != std::prev(this->m_node->m_children.rend(), 1);
            it++) {
          m_memory.push(&(*it));
        }
      }
      this->m_node = &this->m_node->m_children.front();
    } else if(m_memory.size() > 0) {
      this->m_node = m_memory.top();
      m_memory.pop();
    } else {
      this->m_node = nullptr;
    }
    return *this;
}

template<typename T>
template<typename NodeType>
typename Node<T>::template Iterator<NodeType>
Node<T>::Iterator<NodeType>::operator++(int) {
  auto it = *this;
  ++(*this);
  return it;
}

template<typename T>
template<typename NodeType>
bool
Node<T>::Iterator<NodeType>::operator==(const Node<T>::Iterator<NodeType>& other) const {
  bool temp = this->m_node == other.m_node;
  return temp;
}

template<typename T>
template<typename NodeType>
bool
Node<T>::Iterator<NodeType>::operator!=(const Node<T>::Iterator<NodeType>& other) const {
  return !(*this == other);
}

template<typename T>
template<typename NodeType>
const T&
Node<T>::Iterator<NodeType>::operator*() const {
  return this->m_node->getData();
}

template<typename Node>
Tree<Node>::Tree(const Node& root)
  : m_root(root) {
  }

template<typename Node>
Node& Tree<Node>::getRoot() {
  return m_root;
}

template<typename Node>
bool Tree<Node>::operator==(const Tree<Node>& other) const {
  return this->m_root == other.m_root;
}

template<typename Node>
bool Tree<Node>::operator!=(const Tree<Node>& other) const {
  return !(*this == other);
}

}  // namespace utils