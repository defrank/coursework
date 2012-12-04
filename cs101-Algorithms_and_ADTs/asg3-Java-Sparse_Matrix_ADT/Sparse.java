/* $Id: Sparse.java,v 1.2 2010-10-31 07:26:38-07 dmfrank - $
 * Derek Frank, dmfrank@ucsc.edu
 * 
 * NAME
 *   Sparse
 * 
 * DESCRIPTION
 *   Sparse.java is the main class to implement the classes
 * Matrix.java and List.java.  It takes two arguments from the
 * command line: input_file output_file.  Sparse reads input from
 * specified input_file, then stores the information into two
 * Matrices.  Sparse then performs calculations on these Matrices
 * and writes the output to the specified output_file.
 */

import java.io.*;
import java.util.Scanner;

class Sparse {
  
  public static void main (String args[]) throws IOException {
    Scanner infile = null;
    PrintWriter outfile = null;
    String line = null;
    int lineNumber = 0;
    
    if(args.length < 2) {
      System.out.printf("Usage: Sparse infile outfile%n");
      System.exit(1);
    }
    
    infile = new Scanner(new File(args[0]));
    outfile = new PrintWriter(new FileWriter(args[1]));
    Matrix A = new Matrix(1);
    Matrix B = new Matrix(1);
    Matrix AplusB, AplusA, BminusA, AminusA;
    Matrix scalarA, Atranspose;
    Matrix AtimesB, BtimesB;
    int n = 0;
    int a = 0;
    int b = 0;
    int i, row, col, tl;
    double val;
    double scalar = 1.5;
    String[] token = null;
    
    // Read infile //////////////////////////////////////////////
    
    // Initialize Matrices A and B
    while( infile.hasNextLine() ) {
      ++lineNumber;
      line = infile.nextLine()+" "; // add extra space so split works right
      token = line.split("\\s+"); // split line around white space
      tl = token.length;
      
      if ( lineNumber <= 1 ) {
        n = (int)Integer.parseInt(token[0]);
        a = (int)Integer.parseInt(token[1]);
        b = (int)Integer.parseInt(token[2]);
        A = new Matrix(n);
        B = new Matrix(n);
      }
      // After first empty line skipped. Begin initializing
      // Matrix A.
      else if ( lineNumber > 2 ) {
        if ( lineNumber <= a+2 ) {
          row = (int)Integer.parseInt(token[0]);
          col = (int)Integer.parseInt(token[1]);
          val = (double)Double.parseDouble(token[2]);
          A.changeEntry(row,col,val);
        }
        // Aftert Matrix A has been initialized and the next
        // empty line has been skipped. Begin initializing
        // Matrix B.
        else if ( lineNumber >= a+4 ) {
          row = (int)Integer.parseInt(token[0]);
          col = (int)Integer.parseInt(token[1]);
          val = (double)Double.parseDouble(token[2]);
          B.changeEntry(row,col,val);
        }
      }
    }
    
    // Check A and B have specified number of non-zero values.
    // End program in error if not equivalent.
    if ( a != A.getNNZ() || b != B.getNNZ() ) {
      System.out.printf("Error: Incorrect number of non-zero values initialized%n");
      System.exit(1);
    }
    
    // Calculations /////////////////////////////////////////////
    
    // Multiply A by scalar = 1.5
    scalarA = A.scalarMult(scalar);
    // Add A to B
    AplusB = A.add(B);
    // Add A to A
    AplusA = A.add(A);
    // Subtract A from B
    BminusA = B.sub(A);
    // Subtract A from A
    AminusA = A.sub(A);
    // Transpose A
    Atranspose = A.transpose();
    // Product of A and B
    AtimesB = A.mult(B);
    // Product of A and A
    BtimesB = B.mult(B);
    
    // Print to file ////////////////////////////////////////////
    
    // Print A
    outfile.printf("A has %d non-zero entries:%n%s%n",A.getNNZ(),A);
    // Print B
    outfile.printf("B has %d non-zero entries:%n%s%n",B.getNNZ(),B);
    // Print (scalar)*A
    outfile.printf("((%s)*A =%n%s%n",scalar,scalarA);
    // Print A+B
    outfile.printf("A+B =%n%s%n",AplusB);
    // Print A+A
    outfile.printf("A+A =%n%s%n",AplusA);
    // Print B-A
    outfile.printf("B-A =%n%s%n",BminusA);
    // Print A-A
    outfile.printf("A-A =%n%s%n",AminusA);
    // Print transpose(A)
    outfile.printf("Transpose(A) =%n%s%n",Atranspose);
    // Print A*B
    outfile.printf("A*B =%n%s%n",AtimesB);
    // Print B*B
    outfile.printf("B*B =%n%s%n",BtimesB);
    
    // Close Scanner and PrintWriter ////////////////////////////
    infile.close();
    outfile.close();
  }
  
}