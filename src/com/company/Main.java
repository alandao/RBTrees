package com.company;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.io.IOException;
import java.util.ArrayList;

public class Main {

    public static void main(String[] args) throws FileNotFoundException {
	// write your code here
        File text = new File("players_homeruns.csv");
        HashTable playersToScore = new HashTable(509);
        Scanner scnr = new Scanner(text);

        while (scnr.hasNextLine()) {
            String line = scnr.nextLine();
            String[] buffer = line.split(",");
            playersToScore.Insert(buffer[0], buffer[1]);
        }
        scnr.close();
        Scanner keyInput = new Scanner(System.in);
        System.out.println("Players in database: " +
                playersToScore.Count() + "\nEnter player name");
        String input = keyInput.nextLine();
        //ArrayList<Object> debugSet = playersToScore.keySet();
        while (!input.equals("exit")) {
            System.out.println("Searching for player " + input + "...");
            try {
                System.out.println("Player: " + input + "\nHome Run Total: " +
                        (String) playersToScore.Find(input));
            }
            catch (IOException e) {
                System.out.println("Player not found.");
            }
            System.out.println("Enter player name");
            input = keyInput.nextLine();
        }
        System.out.println("Goodbye!");
    }
}
