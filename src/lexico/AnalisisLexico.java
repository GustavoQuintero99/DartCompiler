/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package lexico;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.awt.Color;
import java.awt.TextArea;
import java.io.BufferedReader;
import java.io.File;
import java.io.Reader;
import java.io.InputStreamReader;
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
        txt.setText("");

        FileInputStream inFile = new FileInputStream(file);
        Reader lector = new BufferedReader(new InputStreamReader(inFile));
        parser s = new parser(new Lexer(lector));
        try {
            br = new BufferedReader(new FileReader(file));
            lexer = new Lexer(br);
            while (true) {

                Symbol token = lexer.next_token();
                // se ven los tokens que se van generando
                // txt.append("Valor: " + token.toString()+ " contenido: '"+ token.value + "'
                // status: " + token.parse_state + "\n");
                if (token.toString().equals("#0")) {
                    break;
                }
            }
        } catch (FileNotFoundException ex) {
        }

        try {
            s.debug_parse();
            int dos = s.error_sym();
            txt.setForeground(new Color(25, 111, 61));
            if (s.recovery_message().equals(" ")) {
                txt.append("Analisis lexico y sintactico correcto");
            } else {
                txt.setForeground(new Color(255, 0, 0));
                txt.append(s.recovery_message());
                s.remove_message();
            }

        } catch (Exception e) {
            Symbol sym = s.getS();
        }
    }
}