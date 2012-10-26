<?php
ini_set('max_execution_time', '-1');

require("db.inc.php");
require('functions.inc.php');
require("head.php");

$result = $dbh->query('
	SELECT configs.config, executions.configId 
	FROM executions 
		LEFT JOIN configs ON configs.configId=executions.configId 
	WHERE executions.version='.$_GET['version']
	);
	
$configs = array();
foreach ($result as $row) {
	$configs[$row['configid']] = $row['config'];
}

?>
<table class="scrollTable">
	<thead class="fixedHeader">
	<tr>
		<th>Problem</th>
		<th/>
		<?php foreach ($configs as $config) { ?>
		<th><?=$config?> <br/> Avg - StDev - Med</th>
		<?php } ?>
		<th class="last">Med</th>
	</tr>
	</thead>
	<tbody class="scrollContent">
	<?php
		$result = $dbh->query("
			SELECT tag, count(problemTags.problemId) AS nb 
			FROM problemTags
				LEFT JOIN executions ON executions.problemId = problemTags.problemId
					AND executions.version=$_GET[version]
			WHERE executions.executionId IS NOT NULL
			GROUP BY tag") ;
		$alt = 0;
		foreach ($result as $row) {
			$medians=array();
	?>
	<tr class="<?=$alt++%2?'normalRow':'alternateRow' ?>">
	    <td>
	    	<a href="results.php?version=<?=$_GET['version']?>&tag=<?=$row['tag']?>">
	    		<?=$row['tag']?>
	    	</a>
	    	(<?=$row['nb']?> pbs)
	    </td>
	    <td>
			<ul>
				<li>&nbsp;</li>
				<li>&nbsp;</li>
				<li>&nbsp;</li>
			</ul>
			<ul>
				<?php for ($i = count($_COOKIE['show']) ; --$i>=0;) { ?>
				<li><?=$_COOKIE['show'][$i] ?></li>
				<?php } ?>
			</ul>
		</td>
	    <?php foreach ($configs as $configId=>$config) { ?>
		<td>
			<?php
				$query = 'SELECT result' ;
				for ($i = count($_COOKIE['show']) ; --$i>=0 ;) {
				 	$query .= ",st$i.value AS s$i";
				}
				$query .= '
					FROM executions 
						LEFT JOIN problems ON executions.problemId = problems.problemId
						LEFT JOIN problemTags ON problemTags.problemId = problems.problemId
					';
				for ($i = count($_COOKIE['show']) ; --$i >=0 ;) {
					$query .= "LEFT JOIN statistics AS st$i 
						ON executions.executionId=st$i.executionId 
						AND st$i.name='".$_COOKIE['show'][$i].'\' ' ;
				}
				$query .= 'WHERE tag=\''.$row['tag'].'\'
						AND configId = '.$configId.'
						AND versionId = '.$_GET['version'].'
					ORDER BY result' ;
					
				$r2 = mysql_query($query) or die(mysql_error());
				$count=array();
				$statistics=array();
				$missing=array();
				
				for ($i = count($_COOKIE['show']) ; --$i >=0 ;) {
					$missing[$i] = 0;
					$statistics[$i] = array();
				}
				while ($row2=mysql_fetch_array($r2)) {
					$count[$row2['result']]++;
					for ($i = count($_COOKIE['show']) ; --$i >=0 ;) {
						if (is_null($row2["s$i"])) {
							$missing[$i]++;
						} else {
							$statistics[$i][] = $row2["s$i"];
						}
					}
				}

			?>
			<ul style="font-weight: bold">
	 			<?php foreach ($count AS $res=>$nb) {?>
	 			<li><?=$res?$res:'<i>FAILED</i>'?> : <?=$nb?></li>
	 			<?php } ?>
	 			<?php for ($i = 3-count($count); --$i>=0;) { ?>
	 			<li>&nbsp;</li>
	 			<?php } ?>
			</ul>
			<ul>
				<?php
					
					for($i = count($_COOKIE['show']) ; --$i >=0;) {
						$values = $statistics[$i];
						$tot = array_sum($values);
						
					
						if (count($values) > 0) { 
							$avg = $tot/count($values);
							$median = median($values, $missing[$i]);
							$medians[$i][] = $median;
				?>
				<li>
					<?=$missing[$i]>0?'<span class="failures">':''?><?= engineer(significant($avg, 3)) ?><?=$missing[$i]>0?'</span>':''?> 
					- <?=$missing[$i]>0?'<span class="failures">':''?><?= engineer(significant(stdev($values, $avg), 3)) ?><?=$missing[$i]>0?'</span>':''?> 
					- <?=is_null($median)?'<span class="failures">UND</span>':engineer(significant($median, 3)) ?> 
				</li>
				<?php 	
						} else {
				?>
				<li> - </li>
				<?php			
						} 
					}
				?>
			</ul>
		</td>
		<?php } ?>
		<td>
			<ul>
				<?php 
					for ($i = count($_COOKIE['show']) ; --$i>=0;) {
						if (count($medians[$i])>1 && !in_array(NULL, $medians[$i])) { ?> 
				<li><img src="mini_hist.php?<?php
					for ($j=0; $j<count($medians[$i]);$j++) {
						echo "v$j=".$medians[$i][$j]."&";
					}
				?>" /></li>
			<?php
						}
					}
			?>
			</ul>
		</td>
	</tr>
	<?php } ?>
	</tbody>
</table>


<?php
require("foot.php");

function stdev($array, $average) {
	if (count($array) <= 1) {
		return "UND";
	}
	$sum=0;
	foreach($array AS $value) {
		$sum += pow($value - $average, 2);
	}
	
	return sqrt($sum/(count($array)-1));
}


?>