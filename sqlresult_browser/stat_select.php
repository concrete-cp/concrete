<?php

require('functions.inc.php');
require('mysql.inc.php');

if ($_COOKIE['show']) {
	$show = $_COOKIE['show'];
} else {
	$show = array();
}

if ($_COOKIE['min']) {
	$min = $_COOKIE['min'] ;
} else {
	$min = array();
}

foreach ($show as $k=>$v) {
	setcookie("show[$k]", false);
}

if (isset($_GET['show'])) {
	$show[] = $_GET['show'];
}

if (isset($_GET['min'])) {
	$min[$_GET['min']] ^= true ;
	setcookie('min['.$_GET['min'].']', $min[$_GET['min']]);
}

if (isset($_GET['hide'])) {
	unset($show[array_search($_GET['hide'], $show)]);
}

$i=0;
foreach ($show as $stat) {
	setcookie('show['.$i++.']', $stat);
}

require('head.php');


$result = mysql_query('SELECT DISTINCT name FROM statistics') ;



?>
<table>
<tr>
<?php for ($i = 0 ; $i <= 1 ; $i+=.01) { ?>
	<td style="background-color: <?=color($i)?>">&nbsp;</td>
<?php } ?>
</tr>
</table>
<p><a href="index.php">Index</a></p>
<table>
	<tr><th>Show</th><th>Hide</th><th>Min/Max</th></tr>
	<?php 
		while ($row = mysql_fetch_array($result)) {	
			$name = $row['name'];
	?>
	<tr>
		<td><?=in_array($name, $show)?"<a href='$GLOBALS[PHP_SELF]?hide=$name'>$name</a>":'' ?></td>
		<td><?=in_array($name, $show)?'':"<a href='$GLOBALS[PHP_SELF]?show=$name'>$name</a>" ?></td>
		<td><a href="<?=$GLOBALS[PHP_SELF]?>?min=<?=$name?>"><?=$min[$name]?'-':'+' ?></a></td>
	</tr>
	<?php } ?>
</table>
<?php
require('foot.php');
?>