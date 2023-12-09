import java.util.ArrayList;
import java.util.List;

/**
 * Gears
 */
public class Gears {

    public int x,y;
    public List<Integer> nums;

    public Gears(int x, int y) {
        this.x = x;
        this.y = y;
        this.nums = new ArrayList<>();
    }

    public void add(int num) {
        this.nums.add(num);
    }

    public boolean isAlready(int x, int y) {
        return this.x == x && this.y == y;
    }
    
}