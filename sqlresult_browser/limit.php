<?php

require("mysql.inc.php");

$result = mysql_query("SELECT * FROM executions LEFT JOIN statistics ON 
executions.executionId = statistics.executionId AND statistics.name='total-cpu'
WHERE value > 900");

while($row=mysql_fetch_array($result)) {
	echo "$row[executionId] : $row[value]<br>";
	mysql_query("UPDATE executions SET result='UNKNOWN', solution=NULL WHERE executionId=$row[executionId]");
	mysql_query("DELETE FROM statistics WHERE executionId=$row[executionId]");
}


?>