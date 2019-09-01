package flix.runtime.util;

public class AsciiTable {

    private String[] headers;
    private String[][] data;
    private int[] columns;

    public AsciiTable(String[] headers, String[][] data) {
        this.headers = headers;
        this.data = data;
        columns = new int[headers.length];

        // Compute the column width based on the headers.
        for (int i = 0; i < headers.length; i++) {
            String header = headers[i];
            columns[i] = header.length() + 1;
        }

        // Compute the column width based on the data.
        for (String[] row : data) {
            if (headers.length != row.length) {
                throw new RuntimeException("Mismatched lengths of header and row.");
            }

            // Update the maximum column width.
            for (int j = 0; j < row.length; j++) {
                String cell = row[j];
                columns[j] = Math.max(columns[j], cell.length() + 1);
            }
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        // Print top-most rule.
        newLine(sb);

        // Print the headers.
        for (int i = 0; i < headers.length; i++) {
            String header = headers[i];
            int width = columns[i];
            sb.append(" ");
            sb.append(padRight(header, width));
            if (i < headers.length - 1) {
                sb.append("|");
            }
        }
        sb.append("\n");
        newLine(sb);

        // Print the data.
        for (String[] row : data) {
            for (int i = 0; i < row.length; i++) {
                String cell = row[i];
                int width = columns[i];
                sb.append(" ");
                sb.append(padRight(cell, width));
                if (i < row.length - 1) {
                    sb.append("|");
                }
            }
            sb.append("\n");
            newLine(sb);
        }

        return sb.toString();
    }

    private void newLine(StringBuilder sb) {
        for (int i = 0; i < columns.length; i++) {
            int width = columns[i];
            sb.append("-".repeat(width + 1));

            if (i < columns.length - 1) {
                sb.append("+");
            }
        }
        sb.append("\n");
    }

    private static String padRight(String s, int n) {
        return String.format("%-" + n + "s", s);
    }

    private static String padLeft(String s, int n) {
        return String.format("%" + n + "s", s);
    }

}
