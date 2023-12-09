import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Window2 {
    public static void main(String[] args) throws IOException {
        var in = getIn("./D3/data/input2.txt");
        for (String line : in) System.out.println(line);

        List<Integer> nums = new ArrayList<>();
        List<Gears> gears = new ArrayList<>();

        for (int l = 0; l < in.length; l++) {
            var line = in[l];
            for (int x = 0; x < line.length(); x++) {
                if (!isDigit(line.charAt(x))) continue;
                int y = x;
                while (y < line.length() && isDigit(line.charAt(y))) {
                    y++;
                }
                int num = Integer.parseInt(line.substring(x, y));
                for (int i = - 1; i < (y-x+1); i++)
                    for (int j = -1; j <= 1; j++) {
                        if (i == 0 && j == 0) continue;
                        if (i + x < 0 || i + x >= line.length()) continue;
                        if (j + l < 0 || j + l >= in.length) continue;
                        char n = in[l + j].charAt(x + i);
                        if (check(n)) nums.add(num);
                        if (n == '*') gears.add(new Gears(x + i, l + j));
                        for (Gears gear : gears) {
                            if (gear.isAlready(x + i, l + j)) {
                                gear.add(num);
                            }
                        }
                    }
                x = y;
            }
        }
        System.out.println("---");
        nums.forEach(num -> System.out.print(num + " "));
        System.out.println("\n---");
        gears.stream().filter(gear -> gear.nums.size() == 2).map(gear -> gear.nums.get(0) * gear.nums.get(1)).reduce((a, b) -> a + b).ifPresent(System.out::println);
    }

    public static boolean check(char c) {
        // c is no digit and not a '.'
        return !isDigit(c) && c != '.';
    }

    public static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    public static String[] getIn(String path) throws IOException {
        ArrayList<String> in = new ArrayList<>();
        var br = new BufferedReader(new FileReader(path));
        br.lines().forEach(line -> {
            in.add(line);
        });
        br.close();
        String[] out = new String[in.size()];
        for (int i = 0; i < in.size(); i++) {
            out[i] = in.get(i);
        }
        return out;
    }
}