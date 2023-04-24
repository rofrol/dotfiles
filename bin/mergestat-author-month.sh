mergestat "SELECT count(DISTINCT commits.hash) AS commits, SUM(additions) AS additions, SUM(deletions) AS deletions, author_email \
		FROM commits, stats('', commits.hash) \
		WHERE commits.parents < 2 \
		GROUP BY author_email ORDER BY commits"
