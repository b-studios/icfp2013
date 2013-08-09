CREATE TABLE problems (problem   TEXT PRIMARY KEY NOT NULL,
                       size      INT              NOT NULL);

CREATE TABLE operators (id       INT  PRIMARY KEY NOT NULL,
                        problem  TEXT             NOT NULL,
                        operator TEXT             NOT NULL);

-- and       
-- if0       
-- or        
-- plus      
-- shl1      
-- shr1      
-- shr4      
-- xor       
-- fold      
-- not       
-- shr16     
-- tfold

-- Problem count
select COUNT(*) from problems;

-- All data
select problems.problem, size, operator
from problems, operators
where problems.problem = operators.problem
order by size, problems.problem;

-- only problems with only &|!
select p2.size, p2.problem, o2.operator
from problems p2, operators o2
where p2.problem in (select p.problem from problems p where not exists (select * from operators o where p.problem = o.problem and o.operator not in ('and', 'or', 'not')))
and p2.problem = o2.problem
order by size, p2.problem;

-- order problems by distinct operator count
