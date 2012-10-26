<?php

require ("mysql.inc.php");


$result = mysql_query("SELECT executions.*, st1.value AS cpu, st2.value AS assgn,
	st3.value AS nps FROM executions
	LEFT JOIN statistics as st1 ON st1.executionId = executions.executionId
		AND st1.name = 'search-cpu'
	LEFT JOIN statistics as st2 ON st2.executionId = executions.executionId
		AND st2.name = 'total-assgn'  
	LEFT JOIN statistics as st3 ON st3.executionId = executions.executionId
		AND st3.name = 'search-nps'  
		
	WHERE st1.value IS NOT NULL AND st3.value IS NULL") or die(mysql_error());

while ($row = mysql_fetch_array($result)) {
	$cpu = $row['cpu'];
	if ($cpu == 0) {
		continue;
	}

	$assgn = $row['assgn'];
	
	$nps = $assgn/$cpu;
	
	echo "$row[executionId] ($row[result]) : $assgn / $cpu = $nps<br/>"; 
	mysql_query("INSERT INTO statistics (executionId, name, value) VALUES
		($row[executionId], 'search-nps', $nps)");
	
}

?>