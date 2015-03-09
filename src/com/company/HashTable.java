package com.company;

/**
 * Created by alandao on 3/6/15.
 */
import java.io.IOException;
import java.util.ArrayList;

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
        int location = Math.abs(key.hashCode()) % arr.length;
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
        int location = Math.abs(key.hashCode()) % arr.length;
        if (arr[location] == null) {
            //do nothing
        } else if (arr[location].head.key.equals(key)) { //first node is key
            arr[location].head = arr[location].head.next;
        } else { //encountered a chain, deal accordingly
            //temp variable to not destroy our chained list
            Node temp = arr[location].head;
            while (!temp.next.key.equals(key)) {
                temp = temp.next;
            }
            temp.next = temp.next.next;
        }
    }

    public Object Find(Object key) throws IOException {
        if (containsKey(key)) {
            int location = Math.abs(key.hashCode()) % arr.length;
            if (arr[location].head.key.equals(key)) {
                return arr[location].head.value;
            }
            Node temp = arr[location].head.next;
            while (temp != null) {
                if (temp.key.equals(key)) {
                    return temp.value;
                }
                temp = temp.next;
            }
        }
        throw new IOException();
    }

    private boolean containsKey(Object key) {
        int location = Math.abs(key.hashCode()) % arr.length;
        if (arr[location] == null) {
            return false;
        }
        else {
            return true;
        }
    }

    public int Count() {
        int count = 0;
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] != null) {
                Node temp = arr[i].head;
                while (temp != null) {
                    count +=1;
                    temp = temp.next;
                }
            }
        }
        return count;
    }

    public ArrayList<Object> keySet() {
        ArrayList<Object> setOfAllKeys = new ArrayList<Object>();
        for (LinkedList i : arr) {
            if (i != null) {
                Node temp = i.head;
                while (temp != null) {
                    setOfAllKeys.add(temp.key);
                    temp = temp.next;
                }
            }
        }
        return setOfAllKeys;
    }
}

