with Pending as (
	select '3.0-B4-SNAPSHOT' as version, "configId", iteration, "problemId"
	from unnest(array[41, 42, 44, 45]) as "configId"
		cross join unnest(array[0,1,2,3,4]) as iteration
		cross join "Problem"
	except
	select version, "configId", iteration, "problemId" from "Execution"
)
select iteration, name, regexp_replace(regexp_replace(config, '\ =\ ', '=', 'g'), ',\ ', ':', 'g')
from Pending 
	join "Problem" using ("problemId")
	join "Config" using ("configId")
order by random()
limit 1