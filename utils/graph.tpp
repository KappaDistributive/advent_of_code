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
const T& Node<T>::getData() const {
  return this->m_data;
}

template<typename T>
T& Node<T>::getData() {
  return this->m_data;
}

template<typename T>
bool Node<T>::operator<(const Node<T>& other) const {
  return this->m_data < other.m_data;
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


template<typename T>
DirectedGraph<T>::DirectedGraph()
  : m_vertices(std::set<Node<T>>()),
    m_edges(std::set<std::pair<const Node<T>&, const Node<T>&>>()) {
}

template<typename T>
bool DirectedGraph<T>::areConnected(const Node<T>& origin,
                                    const Node<T>& destination,
                                    bool ignore_direction) const {
  if (ignore_direction) {
  return (this->m_edges.count(std::make_pair(origin, destination)) > 0 ||
          this->m_edges.count(std::make_pair(destination, origin)) > 0);
  }
  return this->m_edges.count(std::make_pair(origin, destination)) > 0;
}

template<typename T>
const Node<T>& DirectedGraph<T>::findNodeByData(const T& data) const {
  Node<T> node(data);
  auto search = this->m_vertices.find(node);
  if (search != this->m_vertices.end()) {
    return *search;
  }
  std::stringstream ss;
  ss << "There's no node with data: " << data;
  throw std::out_of_range(ss.str());
}

template<typename T>
const Node<T>& DirectedGraph<T>::addNode(const Node<T>& node) {
  this->m_vertices.insert(node);
  return this->findNodeByData(node.getData());
}

template<typename T>
void DirectedGraph<T>::addEdge(const Node<T>& lhs, const Node<T>& rhs) {
  this->m_edges.insert({this->addNode(lhs), this->addNode(rhs)});
}

template<typename T>
size_t DirectedGraph<T>::size() const {
  return this->m_vertices.size();
}

}  // namespace graph
}  // namespace utils
