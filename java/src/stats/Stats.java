package stats;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
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
		
		//System.out.format("BySize: %s", bySize(myproblems));
		printBySizeTreeMap(bySize(myproblems));
		
		//System.out.format("ByOperator: %s", byOperator(myproblems));
	}
	
	public static void printBySizeTreeMap(TreeMap<Integer, List<ProblemMetadata>> tm) {
		for (Integer size : tm.keySet()) {
			System.out.println("Size " + size);
			for (ProblemMetadata pm : tm.get(size)) {
				System.out.println(pm);
			}
		}
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
	
	public static TreeMap<String, List<ProblemMetadata>> byOperator(List<ProblemMetadata> l) {
		TreeMap<String, List<ProblemMetadata>> tm = new TreeMap<>();
		for (ProblemMetadata pm : l) {
			for (String operator : pm.operators) {
				List vals = tm.get(operator);
				if (vals == null) {
					vals = new ArrayList<>();
				}
				vals.add(pm);
				tm.put(operator, vals);
			}
		}
		return tm;
	}
}