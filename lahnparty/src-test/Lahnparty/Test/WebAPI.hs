--
-- testing guess
--


-- guess is syntactically malformed. Just think of bad syntax like `(lambda (x) `
-- should return an error code (400)

-- guess is on non existing challange. DOUBLE check whether challange does not exist before
-- running this test!
-- should return error code (404)

-- guess should return "time over" for a old (real) challange
-- (Do this after we failed answering something)
-- should return error code (410)

-- guess should tell us, that a challange has been already solved
-- should return error code (412)

-- guess should raise an error if the request is too big
-- should return error code (413)