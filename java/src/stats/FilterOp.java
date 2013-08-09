package stats;

import stats.Stats.ProblemMetadata;

public interface FilterOp {
	public boolean apply(String op, ProblemMetadata p);
}
