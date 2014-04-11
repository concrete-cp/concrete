package abscon.instance;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.StringTokenizer;

public class Toolkit {

	public static final Comparator<int[]> lexicographicComparator = new Comparator<int[]>() {
		public int compare(int[] t1, int[] t2) {
			for (int i = 0; i < t1.length; i++) {
				if (t1[i] < t2[i])
					return -1;
				if (t1[i] > t2[i])
					return +1;
			}
			return 0;
		}
	};

	public static int executeCommand(String completeCommand, PrintWriter out) {
		// System.out.println("command = " + completeCommand);
		try {
			Process p = Runtime.getRuntime().exec(completeCommand);
			if (out != null) {
				BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
				String firstLine = in.readLine();
				String line = firstLine;
				while (line != null) {
					out.println(line);
					line = in.readLine();
				}
				in.close();
				out.flush();
			}
			p.waitFor();
			int status = p.exitValue();
			p.destroy();
			return status;

		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
			return -1;
		}
	}

	public static String getMemoryInformation() {
		Runtime rt = Runtime.getRuntime();
		DecimalFormat df = new DecimalFormat("###,###,###,###");
		return "used = " + df.format(rt.totalMemory() - rt.freeMemory()) + " free = " + rt.freeMemory() + " total = " + rt.totalMemory() + " max = " + rt.maxMemory();
	}

	public static int[] buildArrayWithOnlyValue(int length, int value) {
		int[] t = new int[length];
		Arrays.fill(t, value);
		return t;
	}

	public static int[] buildIntArrayWithIncrementalValues(int length,int startingValue) {
		int[] t = new int[length];
		for (int i = 0; i < t.length; i++)
			t[i] = startingValue++;
		return t;
	}

	public static boolean isArrayOnlyContaining(int[] t, int value) {
		for (int i = 0; i < t.length; i++) {
			if (t[i] != value)
				return false;
		}
		return true;
	}

	public static int countNbOccurences(int[] t, int value) {
		int cpt = 0;
		for (int v : t)
			if (v == value)
				cpt++;
		return cpt;
	}

	public static void copy(String srcFileName, String dstFileName) {
		try {
			BufferedInputStream in = new BufferedInputStream(new FileInputStream(srcFileName));
			BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(dstFileName));
			byte[] bytes = new byte[1024];

			int nb = in.read(bytes, 0, bytes.length);
			while (nb > 0) {
				out.write(bytes, 0, nb);
				nb = in.read(bytes, 0, bytes.length);
			}
			in.close();
			out.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.exit(1);
		}
	}

	public static boolean selectInDirectory(List<String> list, File dir, int limit, FileFilter fileFilter) {
		String[] listDirectory = dir.list();
		Arrays.sort(listDirectory);
		for (int i = 0; i < listDirectory.length; i++) {
			File f = new File(dir, listDirectory[i]);
			if (f.isFile() && fileFilter.accept(f)) {
				list.add(f.getAbsolutePath());
				if (list.size() >= limit)
					return true;
			}
		}
		for (int i = 0; i < listDirectory.length; i++) {
			File f = new File(dir, listDirectory[i]);
			if (f.isDirectory() && selectInDirectory(list, f, limit, fileFilter))
				return true;
		}
		return false;
	}

	public static String getRawInstanceName(String s) {
		int first = (s.lastIndexOf(File.separator) != -1 ? s.lastIndexOf(File.separator) + 1 : 0);
		int last = (s.lastIndexOf(".") != -1 ? s.lastIndexOf(".") : s.length());
		return (first > last ? s.substring(first) : s.substring(first, last));
	}

	public static String getRelativeParentPackageNameOf(Class clazz) {
		String s = clazz.getName();
		int last = s.lastIndexOf(".");
		if (last == -1)
			return null;
		String path = s.substring(0, last);
		return path.substring(path.lastIndexOf(".") + 1);
	}

	public static String getRelativeClassNameOf(Class clazz) {
		String s = clazz.getName();
		return s.substring(s.lastIndexOf(".") + 1);
	}

	public static String getRelativeClassNameOf(Object object) {
		String s = object.getClass().getName();
		return s.substring(s.lastIndexOf(".") + 1);
	}

	public static String getSeparator(int mode) {
		return (mode == 0 ? "," : "\n");
	}

	public static String getXMLBaseNameOf(String s) {
		int start = s.lastIndexOf(File.separator);
		start = (start == -1 ? 0 : start + 1);
		int end = s.toLowerCase().lastIndexOf(".xml");
		end = (end == -1 ? s.length() : end);
		return s.substring(start, end);
	}

	public static boolean isInteger(String token) {
		try {
			Integer.parseInt(token);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	public static Integer parseInteger(String token) {
		try {
			int l = Integer.parseInt(token);
			return new Integer(l);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	public static Long parseLong(String token) {
		try {
			long l = Long.parseLong(token);
			return new Long(l);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	public static String getFormattedCurrentDate() {
		Calendar calendar = new GregorianCalendar();
		calendar.setTimeInMillis(System.currentTimeMillis());
		int year = calendar.get(Calendar.YEAR);
		int month = calendar.get(Calendar.MONTH);
		int day = calendar.get(Calendar.DAY_OF_MONTH);
		int hour = calendar.get(Calendar.HOUR_OF_DAY);
		int minute = calendar.get(Calendar.MINUTE);
		DecimalFormat df = new DecimalFormat("00");
		return year + "_" + df.format(month) + "_" + df.format(day) + "_" + df.format(hour) + "_" + df.format(minute);
	}

	public static String getFormattedMemorySize(long size) {
		long m = size / 1000000;
		long k = size / 1000;
		k = k - m * 1000;
		return m + "M" + k;
	}

	public static long getFreeMemory() {
		Runtime rt = Runtime.getRuntime();
		return rt.maxMemory() - rt.totalMemory() + rt.freeMemory();
	}

	public static long getUsedMemory() {
		Runtime rt = Runtime.getRuntime();
		return rt.totalMemory() - rt.freeMemory();
	}

	public static String getFormattedUsedMemorySize() {
		return getFormattedMemorySize(getUsedMemory());
	}

	public static String getAllocatedMemory() {
		Runtime rt = Runtime.getRuntime();
		return getFormattedMemorySize(rt.totalMemory());
	}

	public static boolean areIdentical(int[] t1, int[] t2) {
		for (int i = 0; i < t1.length; i++)
			if (t1[i] != t2[i])
				return false;
		return true;
	}

	public static void copy(int[] dstSupport, int[] srcSupport) {
		for (int i = 0; i < dstSupport.length; i++)
			dstSupport[i] = srcSupport[i];
	}

	public static int searchFirstStringOccurrenceIn(String s, String[] t) {
		for (int i = 0; i < t.length; i++)
			if (t[i].equals(s))
				return i;
		return -1;
	}
	
	public static int searchFirstObjectOccurrenceIn(Object object, Object[] objects) {
		for (int i = 0; i < objects.length; i++)
			if (object == objects[i])
				return i;
		return -1;
	}
	
	public static String buildStringFromInts(int[] t) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < t.length; i++) {
			sb.append(t[i]);
			if (i < t.length - 1)
				sb.append(' ');
		}
		return sb.toString();
	}

	public static String buildStringFromTokens(String[] t) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < t.length; i++) {
			sb.append(t[i]);
			if (i < t.length - 1)
				sb.append(' ');
		}
		return sb.toString();
	}

	public static String[] buildTokensFromString(String s) {
		StringTokenizer st = new StringTokenizer(s);
		String[] tokens = new String[st.countTokens()];
		for (int i = 0; i < tokens.length; i++)
			tokens[i] = st.nextToken();
		return tokens;
	}

	/**
	 * Returns a string built from the given first one by inserting (if necessary) a whitespace before and after each occurence of a character of the second one.
	 */
	public static String insertWhitespaceAround(String s, String t) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			boolean found = false;
			for (int j = 0; !found && j < t.length(); j++)
				if (t.charAt(j) == c)
					found = true;
			if (found) {
				sb.append(" ");
				sb.append(c);
				sb.append(" ");
			} else
				sb.append(c);
		}
		return sb.toString();
	}

}