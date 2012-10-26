<?php
require("db.inc.php");
require("head.php");

$result = $dbh->query("SELECT version, count(*) FROM executions GROUP BY version");

?>
<p><a href="stat_select.php">Select stats</a> | <a href="trace.php">Traces</a></p>
<table>
<?php
foreach ($result as $line) {
?>
	<tr>
	  <th>
	    <a href="tags.php?version=<?php echo $line['version']?>">
	      <?php echo $line['version']?>
	    </a>
	  </th>
	  <td><?php echo $line['count']?></td>
	</tr>
<?php
}
?>
</table>

<?php
require("foot.php");
?>