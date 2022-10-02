/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package lexico;

import java.awt.TextArea;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java_cup.runtime.Symbol;

/**
 *
 * @author
 */
public class AnalisisLexico {

    private static BufferedReader br;
    private static Lexer lexer;
           
    public void runLexer(File file, TextArea txt) throws IOException {

        try {
            file = new File("src/lexico/codigo.txt");
            br = new BufferedReader(new FileReader(file));
            lexer = new Lexer(br);
            while (true) {
            
                Symbol token = lexer.next_token();
                txt.append("Valor: " + token.toString()+ " contenido: '"+ token.value + "' status: " + token.parse_state + "\n");
                if (lexer.next_token().toString().equals("#0")){
                    break;
                }
            }
        } catch (FileNotFoundException ex) {
        }

    }
}
