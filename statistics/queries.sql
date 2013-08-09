CREATE TABLE problems (problem   TEXT PRIMARY KEY NOT NULL,
                       size      INT              NOT NULL);

CREATE TABLE operators (id       INT  PRIMARY KEY NOT NULL,
                        problem  TEXT             NOT NULL,
                        operator TEXT             NOT NULL);

-- Problem count
select COUNT(*) from problems;

-- All data
select problems.problem, size, operator
from problems, operators
where problems.problem = operators.problem
order by size, problems.problem
;

