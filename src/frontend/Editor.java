/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/GUIForms/JPanel.java to edit this template
 */
package frontend;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import javax.swing.JOptionPane;

/**
 *
 * @author Bryan.Ramos
 */
public class Editor extends javax.swing.JPanel {

    private  File archivo;

    /**
     * Creates new form Editor
     *
     * @param archivo
     */
    public Editor(File archivo) {
        this.archivo = archivo;
        if (!archivo.exists()) {
            try {
                if (!archivo.createNewFile()) {
                    JOptionPane.showMessageDialog(this, "No se pudo crear el archivo");
                } else {
                    initComponents();

                }
            } catch (IOException e) {
            }
        } else {
            initComponents();

        }
    }

    public void guardar() throws IOException {

        String str = txaContenido.getText();
        try ( FileOutputStream outputStream = new FileOutputStream(this.archivo)) {
            byte[] strToBytes = str.getBytes();
            outputStream.write(strToBytes);
        }

    }

    public void leerArchivo() throws FileNotFoundException, IOException {

        String result;
        DataInputStream reader = new DataInputStream(new FileInputStream(this.archivo));
        int nBytesToRead = reader.available();
        if (nBytesToRead > 0) {
            byte[] bytes = new byte[nBytesToRead];
            reader.read(bytes);
            result = new String(bytes);
            txaContenido.append(result);
        }
        txaContenido.repaint();
    }
        public String leerArchivo(File archivoo) throws FileNotFoundException, IOException {

        String result = null;
        DataInputStream reader = new DataInputStream(new FileInputStream(archivoo));
        int nBytesToRead = reader.available();
        if (nBytesToRead > 0) {
            byte[] bytes = new byte[nBytesToRead];
            reader.read(bytes);
            result = new String(bytes);
        }
        return result;
    }
        
        public void setearTexto(String texto){
         txaContenido.append(texto);
         txaContenido.repaint();
        }
        
        public String getText(){
            return txaContenido.getText();
        }
    
    

    public File getArchivo() {
        return archivo;
    }
    
    public void setArchivo(File archivo){
        this.archivo=archivo;
    }
    
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        txaContenido = new javax.swing.JTextArea();

        setLayout(new java.awt.BorderLayout());

        txaContenido.setColumns(20);
        txaContenido.setRows(5);
        jScrollPane1.setViewportView(txaContenido);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea txaContenido;
    // End of variables declaration//GEN-END:variables
}
