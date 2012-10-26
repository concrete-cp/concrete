<?php
require("mysql.inc.php");
require("functions.inc.php");
require("artichow/BarPlot.class.php");

$colors = array (new DarkBlue, new DarkRed, new DarkGreen, new DarkYellow, new Orange);
$c=0;
   // Il est toujours nécessaire de donner une taille à la création de votre graphique.
   // Ici, le graphique mesurera 400 x 400 pixels.
   $graph = new Graph(500, 400);
   
   $graph->title->set($_GET['statistic']);
   
   $group = new PlotGroup;
$group->setBackgroundGradient(
    new LinearGradient(
        new VeryLightGray,
        new Color(245, 245, 245),
        0
    )
);

$group->setPadding(55, 20, 40, 15);
$group->setSpace(5, 5, 5, 5);

$group->legend->setAlign(Legend::CENTER);
$group->legend->setPosition(.5,.1);

$result = mysql_query('
	SELECT DISTINCT configs.config, executions.configId 
	FROM executions LEFT JOIN configs ON configs.configId=executions.configId 
	WHERE executions.versionId='.$_GET['version'])
	or die (mysql_error());
	
	
while ($row = mysql_fetch_array($result)) {

	$query = '
			SELECT executions.executionId, configs.config, statistics.value, statistics.name 
			FROM executions 
				LEFT JOIN configs ON executions.configId = configs.configId
				LEFT JOIN problems ON problems.problemId = executions.problemId 
				LEFT JOIN problemTags ON problems.problemId = problemTags.problemId
				LEFT JOIN statistics ON statistics.executionId = executions.executionId
					AND statistics.name = \''.$_GET['statistic'].'\'';
	
			
	$query .= '		
					
			WHERE tag = \''.$_GET['tag'].'\'
				AND executions.versionId = '.$_GET['version'].'
				AND executions.configId = '.$row['configId'] ;
				
	$result2 = mysql_query($query) or die(mysql_error());
  
    
  $missing = 0;
    $stats = array();
  
  while ($row2 = mysql_fetch_array($result2)) {
	if (is_null($row2['value'])) {
		
		$missing++;
		
	} else {
  		$stats[] = $row2['value'];
  	}
  }
  $configs[] = rewrite_config($row['config']);
  
  $medians[] = median($stats, $missing);
 }
    // On créé la courbe
   $plot = new BarPlot($medians);
	   
   $plot->setBarColor($colors[$c++]);

//Une fois votre courbe correctement paramétré, il est nécessaire de l'ajouter au graphique. Pour cela, une simple ligne suffit :

   $group->add($plot);


$group->axis->bottom->setLabelText($configs);
	$graph->add($group);
   $graph->draw();

?>