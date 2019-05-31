with Pending as (
    select "configId", iteration, "problemId"
    from "Config"
      cross join unnest(array[0,1,2]) as iteration
      cross join "Problem"
    where "configId" >= 0
    except
    select "configId", iteration, "problemId" from "Execution"
)
select'-iteration=' || iteration, config, name --'-P', regexp_replace(regexp_replace(config, '\ =\ ', '=', 'g'), ',\ ', ':', 'g')
from Pending
    join "Problem" using ("problemId")
    join "Config" using ("configId")
order by iteration, random()
limit 1
