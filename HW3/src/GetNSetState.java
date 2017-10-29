import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    void initializeValue(byte[] v) {
        int[] temp = new int[v.length];
        for (int i = 0; i < v.length; i++) {
            temp[i] = v[i];
        }

        value = new AtomicIntegerArray(temp); 
    }

    GetNSetState(byte[] v) { 
        initializeValue(v);
        maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) { 
        initializeValue(v);
        maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() { 
        byte[] temp = new byte[size()];
        for (int i = 0; i < size(); i++) {
            temp[i] = ((byte) value.get(i));
        }

        return temp;
    }

    public boolean swap(int i, int j) {
	if (value.get(i) <= 0 || value.get(j) >= maxval) {
	    return false;
	}

	value.getAndDecrement(i);
	value.getAndIncrement(j);
	return true;
    }
}
