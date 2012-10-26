<?php
require("mysql.inc.php");
require("functions.inc.php");
require("head.php");

$result = mysql_query('
	SELECT DISTINCT configs.config, configs.display, executions.configId 
	FROM executions LEFT JOIN configs ON configs.configId=executions.configId 
	WHERE executions.versionId='.$_GET['version'].'
	ORDER BY configs.display, configs.config')
	or die (mysql_error());
$configs = array();
$configIds = array();
while ($row = mysql_fetch_array($result)) {
	$configs[$row['configId']] = $row['display']? $row['display']:$row['config'];
	$configIds[]=$row['configId'];
}
?>

<table>
	<tr>
	<?php foreach($_COOKIE['show'] as $stat) { ?>
	<td><img src="results_graph.php?statistic=<?=$stat?>&tag=<?=$_GET['tag']?>&version=<?=$_GET['version']?>" /></td>
	<?php } ?>
	</tr>
</table>

<table>
	<tr>
	<?php foreach($_COOKIE['show'] as $stat) { ?>
	<td><img src="results_graph.php?log=1&statistic=<?=$stat?>&tag=<?=$_GET['tag']?>&version=<?=$_GET['version']?>" /></td>
	<?php } ?>
	</tr>
</table>

<table class="scrollTable">
	<thead class="fixedHeader"> 
	<tr>
		<th>Problem</th>
		<th/>
		<?php for ($i=0;$i<count($configIds)-1;$i++) { ?>
		<th><?=$configs[$configIds[$i]]?></th>
		<?php } ?>
		<th class="last"><?=$configs[$configIds[count($configIds)-1]]?></th>
	</tr>
	</thead>
	<tbody class="scrollContent">
	<?php
		$result = mysql_query('
			SELECT DISTINCT problems.name, problems.problemId 
			FROM problems 
				LEFT JOIN problemTags ON problems.problemId = problemTags.problemId
				LEFT JOIN executions ON executions.problemId = problems.problemId
			WHERE tag = \''.$_GET['tag'].'\'
				AND executions.problemId IS NOT NULL
				AND executions.versionId = '.$_GET['version'].'
			ORDER BY problems.name') or die(mysql_error());
		$alt=0;
		while ($row = mysql_fetch_array($result)) {
	?>
	<tr class="<?=$alt++%2?'normalRow':'alternateRow' ?>">
		<td><?=$row['problemId']?>. <?=$row['name']?></td>
		<td>
			<br/>
			<ul>
				<?php foreach ($_COOKIE['show'] as $stat) { ?>
				<li><?=$stat ?></li>
				<?php } ?>
			</ul>
		</td>
		<?php
			$query = 'SELECT executions.*, statistics.name, statistics.value';
			
//			for ($i = count($_COOKIE['show']) ; --$i>=0 ;) {
//				 $query .= ",st$i.value AS s$i";
//			}
			
			$query .= ' FROM executions LEFT JOIN 
				statistics ON executions.executionId=statistics.executionId AND
					(0 ';
			
			foreach ($_COOKIE['show'] as $stat) {
				$query .= " OR statistics.name='$stat'";
			}
			
			
			
//			for ($i = count($_COOKIE['show']) ; --$i >=0 ;) {
//				$query .= "LEFT JOIN statistics AS st$i 
//					ON executions.executionId=st$i.executionId 
//					AND st$i.name='".$_COOKIE['show'][$i].'\' ' ;
//			}
			
			$query .= ')
				WHERE problemId='.$row['problemId'].' 
					AND versionId='.$_GET['version'] ;
			

	
			$result2 = mysql_query($query) or die(mysql_error());

			$resultat = array();
			$min = array();
			$max = array();
			while($row2=mysql_fetch_array($result2)) {
				$resultat[$row2['configId']]['executionId'] = $row2['executionId'];
				$resultat[$row2['configId']]['result'] = $row2['result'];
				$resultat[$row2['configId']]['solution'] = $row2['solution'];
				if (!is_null($row2['value'])) {
					$resultat[$row2['configId']]['stats'][$row2['name']] = $row2['value'];
					if (!isset($min[$row2['name']]) ||
						$row2['value'] < $min[$row2['name']]) {
							
						$min[$row2['name']]=$row2['value'] ;
						
					}
					if (!isset($max[$row2['name']]) ||
						$row2['value'] > $max[$row2['name']]) {
							
						$max[$row2['name']]=$row2['value'] ;
						
					}
					
				}
			}
//				for ($i = count($_COOKIE['show']) ; --$i>=0;) {
//					$resultat[$row2['configId']]['statistics'][$i] = $row2["s$i"];
//					if (!is_null($row2["s$i"])) {
//						if (!isset($min[$i]) || $row2["s$i"] < $min[$i]) {
//							$min[$i] = $row2["s$i"];
//						}
//						if (!isset($max[$i]) || $row2["s$i"] > $max[$i]) {
//							$max[$i] = $row2["s$i"];
//						}
//					}
//					
//				}
//				
//			}
			
			foreach ($configs as $c => $config)  { 
		?>
		<td>
			<?php if (!isset($resultat[$c])) { ?>
			<i>NO DATA</i>
			<?php } else if (is_null($resultat[$c]['result'])) { ?>
			<i><abbr title="<?=$resultat[$c]['solution']?>">FAILED</abbr> (<?=$resultat[$c]['executionId']?> - <a href="del.php?id=<?= $resultat[$c]['executionId']?>">del</a>)</i>
			
			<?php } else { ?>
			<abbr title="<?=$resultat[$c]['solution']?>">
			<?=$resultat[$c]['result']?></abbr>  (<?=$resultat[$c]['executionId']?> <a href="del.php?id=<?= $resultat[$c]['executionId']?>">del</a>)
			
			<?php if (!is_null($resultat[$c]['solution'])) { /*?>
			: <i><?=$resultat[$c]['solution']?></i>
			<?php */} ?>
			
			
			<ul> 
				<?php if (is_null($resultat[$c]['stats'])) { ?>
				<li>-</li>
				<?php } else {
					foreach ($_COOKIE['show'] as $name) {
						$value = $resultat[$c]['stats'][$name]; 
				?>
				<li style="background: 
					<?= is_null($value)?"clear":color(
						($_COOKIE['min'][$name]?
						($value-$min[$name])/($max[$name]-$min[$name]):
						($max[$name]-$value)/($max[$name]-$min[$name])
					)/3)?>">
					<?= engineer(significant($value, 3))?>
				</li>
				<?php
					} 
				}
				?>
			</ul>
			<?php } ?>
		</td>
		<?php } ?>
	</tr>
	<?php } ?>
	</tbody>
</table>


<?php
require("foot.php");
?>