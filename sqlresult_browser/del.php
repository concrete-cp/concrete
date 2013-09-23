<?php

require("mysql.inc.php");

if (!is_numeric($_GET['id'])) {
	die("Numeric id !");
}

// Cast integer data type to prevent SQL injection
$id = (int)$_GET['id'];

mysql_query("DELETE FROM executions WHERE executionId=$id");
mysql_query("DELETE FROM statistics WHERE executionId=$id");
