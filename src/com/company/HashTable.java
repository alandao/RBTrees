package com.company;

/**
 * Created by alandao on 3/6/15.
 */
public class HashTable {
    private LinkedList[] arr;

    class LinkedList {
        Node head;
        public LinkedList(Node node) {
            head = node;
        }
    }
    class Node{
        Object key;
        Object value;
        Node next;
        public Node(Object key, Object value) {
            this.key = key;
            this.value = value;
        }
    }

    public HashTable(int size) {
        arr = new LinkedList[size];
    }

    public void Insert(Object key, Object value) {
        int location = key.hashCode() % arr.length;
        if (arr[location] == null) {
            arr[location] = new LinkedList(new Node(key, value));
        }
        else {
            //temp variable to not destroy our chained list
            Node temp = arr[location].head;
            while (temp.next != null) {
                temp = temp.next;
            }
            temp.next = new Node(key, value);
        }
    }

    public void Remove(Object key) {
        int location = key.hashCode() % arr.length;
        if (arr[location] == null) {
            //do nothing
        } else if (arr[location].head.next == null) { //first node is key
            arr[location] = null;
        } else { //encountered a chain, deal accordingly
            //temp variable to not destroy our chained list
            Node temp = arr[location].head;
            while (temp.next != key) {
                temp = temp.next;
            }
            temp.next = temp.next.next;
        }
    }
    /*
    public V Find(K Key) {

    }
    */
    private boolean containsKey(Object key) {
        int location = key.hashCode() % arr.length;
        if (arr[location] == null) {
            return false;
        }
        else {
            return true;
        }
    }

}

