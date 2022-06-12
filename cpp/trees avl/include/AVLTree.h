#pragma once

#include <vector>

class AVLTree
{
public:
    bool contains(int value) const;
    bool insert(int value);
    bool remove(int value);

    std::size_t size() const;
    bool empty() const;

    std::vector<int> values() const;

    ~AVLTree();

private:
    class Node;
    static int getHeight(Node * current);
    static Node * rotateRight(Node * current);
    static Node * rotateLeft(Node * current);
    static Node * bigRotateLeft(Node * current);
    static Node * bigRotateRight(Node * current);
    static int getDifference(Node * current);
    Node * insertRecursive(Node * current, int value);
    void getValues(Node * current, std::vector<int> & container) const;
    static Node * removeRecursive(Node * current, int value);
    static Node * reBalance(Node * current, bool leftArgument, bool rightArgument);
    Node * head = nullptr;
    size_t sizeOfTree = 0;
};