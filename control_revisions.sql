select "problemId", iteration, string_agg(value, ', ') from "Execution"
natural join "Statistic"
where "Statistic".name = 'solver.filter.revisions'
group by "problemId", iteration
having count(*) > 1