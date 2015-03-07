package com.company;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) throws FileNotFoundException {
	// write your code here
        File text = new File("players_homeruns.csv");
        HashTable<String, Integer> playersToScore = new HashTable<String, Integer>(10);
        Scanner scnr = new Scanner(text);

        while (scnr.hasNextLine()) {
            String line = scnr.nextLine();
            playersToScore.Insert("Hi", 0);

        }
    }
}
