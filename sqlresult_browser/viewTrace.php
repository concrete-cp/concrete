<?php

require "mysql.inc.php";

require "head.php";

$result = mysql_query("
	SELECT * 
	FROM executions 
		INNER JOIN problems ON problems.problemId = executions.problemId
		INNER JOIN configs ON configs.configId = executions.configId
		INNER JOIN versions ON versions.versionId = executions.versionId
	WHERE executionId = ".$_GET['exec']) or die (mysql_error());

$row = mysql_fetch_array($result);

?>
<a href="trace.php">Traces</a>
<h1><?=$row['version']?></h1>
<h2><?=nl2br($row['config'])?></h2>
<h3><?=$row['name']?></h3>
<?php

$result = mysql_query("SELECT * FROM trace WHERE executionId = ".$_GET['exec']. " ORDER BY sequenceNumber")
	or die(mysql_error());
?> 

<ul>

	<?php while ($row = mysql_fetch_array($result)) { ?>
	<li>
		<?=$row['date']?> - <?=$row['source'] ?><br />
		<?=$row['level'] ?>: <?=nl2br($row['content']) ?>
	</li>
	<?php } ?>
</ul>

<?php
require "foot.php";
?>