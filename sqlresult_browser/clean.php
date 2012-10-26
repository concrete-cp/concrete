<?php

require "mysql.inc.php" ;

$result = mysql_query("SELECT * , count( executionId ) AS count
FROM `executions`
GROUP BY versionId, configId, problemId
HAVING count >1");

while ($row = mysql_fetch_array($result)) {
	$r2 = mysql_query("SELECT * FROM executions
	WHERE versionId = $row[versionId] AND configId=$row[configId] AND problemId=$row[problemId]
	ORDER BY executionId DESC");
	
	$keep1=false;
	while ($row2 = mysql_fetch_array($r2)) {
		if ($keep1) {
			echo "Deleting execution $row2[executionId]<br/>";
			mysql_query("DELETE FROM executions WHERE executionId=$row2[executionId]");
		} else {
			$keep1=true;
		}
	}
}

$result = mysql_query("SELECT DISTINCT statistics.executionId
FROM statistics
LEFT JOIN executions ON statistics.executionId = executions.executionId
WHERE executions.executionId IS NULL");

while ($row=mysql_fetch_array($result)) {
	mysql_query("DELETE FROM statistics WHERE statistic.executionId=$row[executionId]");
}

?>