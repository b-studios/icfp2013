package stats;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;


public class Stats {

	public static class ProblemMetadata {
		public String id;
		public Integer size;
		public List<String> operators;
		
		public ProblemMetadata(String id, Integer size, List<String> operators) {
			super();
			this.id = id;
			this.size = size;
			this.operators = operators;
		}

		@Override
		public String toString() {
			return "ProblemMetadata [id=" + id + ", size=" + size
					+ ", operators=" + operators + "]";
		}
	}
	
	static JSONArray myproblemsJSON;
	static List<ProblemMetadata> myproblems;
	
	public static void main(String[] args) throws Exception {
		myproblemsJSON = (JSONArray) (new JSONParser()).parse(new FileReader("../myproblems.txt"));
		//System.out.println(myproblemsJSON);
		
		myproblems = new ArrayList<>();
		for (Object o2 : myproblemsJSON) {
			JSONObject o = (JSONObject) o2;
			myproblems.add(new ProblemMetadata(
			                 o.get("id").toString(),
			                 Integer.parseInt(o.get("size").toString(), 10),
			                 (List) o.get("operators")));
		}
		
		System.out.format("Problem count: %d\n", myproblems.size());
		
		System.out.format("BySize: %s", bySize(myproblems));
		
		System.out.println("\n");
		
		System.out.println("all problems");
		System.out.println(printHistogram(bySize(myproblems)));
		
		System.out.println("\nall problems without fold");
		System.out.println(printHistogram(filterExcludesOp(bySize(myproblems), "fold")));
		
	}

	public static TreeMap<Integer, List<ProblemMetadata>> bySize(List<ProblemMetadata> l) {
		TreeMap<Integer, List<ProblemMetadata>> tm = new TreeMap<>();
		for (ProblemMetadata pm : l) {
		    List vals = tm.get(pm.size);
			if (vals == null) {
				vals = new LinkedList<>();
			}
			vals.add(pm);
			tm.put(pm.size, vals);
		}
		return tm;
	}
	
	
	public static String printHistogram(TreeMap<Integer, List<ProblemMetadata>> tm) {
		int max = -1;
		for (Integer i : tm.keySet()) {
			if (i > max) {
				max = i;
			}
		}
		
		int[] bins = new int[max+1];
		
		for (Entry<Integer, List<ProblemMetadata>> e : tm.entrySet()) {
			bins[e.getKey()] = e.getValue().size();
		}
		
		
		StringBuilder sb = new StringBuilder();
		
		sb.append(pad("size",5));
		sb.append(pad("#", 4));
		sb.append("\n");
		for (int i = 0; i < bins.length; i++) {
			sb.append(pad(""+ i, 5));
			sb.append(pad(""+bins[i], 4));
			sb.append(repeat('#', bins[i]));
			sb.append("\n");
		}
				
		return sb.toString();
	}
	
	private static String repeat(char symbol, int times) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < times; i++) {
			sb.append(symbol);
		}
		return sb.toString();
	}
	
	private static String pad(String string, int length) {
		StringBuilder sb = new StringBuilder(string);
		for (int i = string.length(); i < length; i++) {
			sb.append(' ');
		}
		return sb.toString();
	}
	
	private static TreeMap<Integer, List<ProblemMetadata>> filterExcludesOp(TreeMap<Integer, List<ProblemMetadata>> tm, String op) {
		TreeMap<Integer, List<ProblemMetadata>> tm2 = new TreeMap<>();
		
		for (Entry<Integer, List<ProblemMetadata>> e : tm.entrySet()) {
			List<ProblemMetadata> problems = e.getValue();
			List<ProblemMetadata> newProblems = new LinkedList<>();
			for (ProblemMetadata p : problems) {
				if (!p.operators.contains(op)) {
					newProblems.add(p);
				}
			}
			tm2.put(e.getKey(), newProblems);
		}
		
		return tm2;

	}
	
}
