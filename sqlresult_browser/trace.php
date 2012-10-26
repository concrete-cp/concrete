<?php
require "mysql.inc.php";
require "head.php";
?>
<p><a href="index.php">Index</a></p>
<?php
$result = mysql_query("
	SELECT versions.*, configs.* AS nb
	FROM executions
		LEFT JOIN versions ON versions.versionId=executions.versionId
		LEFT JOIN configs ON configs.configId=executions.configId
		INNER JOIN trace ON trace.executionId=executions.executionId
	GROUP BY executions.versionId, executions.configId") or die (mysql_error());

?>
<?php while ($row = mysql_fetch_array($result)) { ?>
<h1><?= $row['version'] ?> - <?=$row['config'] ?></h1>
<ul>
<?php

	$r2 = mysql_query("
		SELECT problems.name, executions.executionId, 
			count(ts.sequenceNumber) AS nbTs, 
			count(tw.sequenceNumber) AS nbTw
		FROM executions
			INNER JOIN problems ON problems.problemId=executions.problemId
			LEFT JOIN trace AS ts ON ts.executionId = executions.executionId AND ts.level='SEVERE'
			LEFT JOIN trace AS tw ON tw.executionId = executions.executionId AND tw.level='WARNING'
		WHERE versionId = $row[versionId]
			AND configId = $row[configId]
		GROUP BY executions.executionId 
		HAVING nbTs > 0 OR nbTw > 0
		ORDER BY nbTs DESC, nbTw DESC, name
	") or die (mysql_error());

	while ($l2 = mysql_fetch_array($r2)) {
?>
	<li><a href="viewTrace.php?exec=<?=$l2['executionId']?>"><?=$l2['name'] ?></a> (<?=$l2['nbTs']?> SEVERE, <?=$l2['nbTw']?> WARNING)</li>	
<?php } ?>
</ul>
<?php
}
require "foot.php";

?>