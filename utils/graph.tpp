#include "graph.hpp"

namespace utils {
namespace graph {

template<typename T>
Node<T>::Node(const T& data)
  : m_data(data) {
}

template<typename T>
Node<T>::Node(const Node<T>& node)
  : m_data(node.m_data) {
}

template<typename T>
bool Node<T>::operator==(const Node<T>& other) const {
  return this->m_data == other.m_data;
}

template<typename T>
bool Node<T>::operator!=(const Node<T>& other) const {
  return !(*this == other);
}

template<typename S>
std::ostream& operator<<(std::ostream& os, const Node<S>& node) {
  os << "Node<data: " << node.m_data << ">";
  return os;
}

}  // namespace graph
}  // namespace utils
