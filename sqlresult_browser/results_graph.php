<?php
require("mysql.inc.php");
require("functions.inc.php");
require("artichow/ScatterPlot.class.php");

$colors = array (new Blue, new Red, new DarkGreen, new MidYellow, new LightOrange);
$c=0;
   // Il est toujours nécessaire de donner une taille à la création de votre graphique.
   // Ici, le graphique mesurera 400 x 400 pixels.
   $graph = new Graph(500, 400);
   
$graph->title->set($_GET['statistic'] . (isset($_GET['log'])?' (log)':''));
   
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

$group->legend->setAlign(Legend::RIGHT, Legend::BOTTOM);
$group->legend->setPosition(.95, .95);
$group->grid->hide(TRUE);


$group->axis->bottom->setLabelNumber(5);


$result = mysql_query('
	SELECT DISTINCT configs.config, configs.display, executions.configId 
	FROM executions LEFT JOIN configs ON configs.configId=executions.configId 
	WHERE executions.versionId='.$_GET['version'].'
	ORDER BY  configs.display, configs.config')
	or die (mysql_error());
	
while ($row = mysql_fetch_array($result)) {
	$result2 = mysql_query('
			SELECT configs.config, statistics.value, executions.result
			FROM executions 
				LEFT JOIN configs ON executions.configId = configs.configId
				LEFT JOIN problems ON problems.problemId = executions.problemId 
				LEFT JOIN problemTags ON problems.problemId = problemTags.problemId
				LEFT JOIN statistics ON statistics.executionId = executions.executionId
					AND statistics.name = \''.$_GET['statistic'].'\'
					
				
			WHERE tag = \''.$_GET['tag'].'\'
				AND executions.versionId = '.$_GET['version'].'
				AND executions.configId = '.$row['configId']) or die(mysql_error());
  
    
  $values = array();
  $numb = array();
  $i=0;
  while ($row2 = mysql_fetch_array($result2)) {
  	if ($row2['result'] != 'UNSAT' && $row2['result'] != 'SAT') {
  		continue; $row2['value'] = 1000;
  	}
  	if (!is_null($row2['value'])) {
  		if (isset($_GET['log'])) {
  			if ($row2['value']<=1e-3) {
  				$values[] = -3;
  			} else {
  				$values[] =  log($row2['value']);
  			}
  		} else {
  		
  			$values[] = $row2['value'];
  		}
  		$numb[] = ++$i;
  	}
  }
 
 if (count($values)>0) {
  
  sort($values);
  
 
   // On créé la courbe
   $plot = new ScatterPlot($values, $numb);

$plot->link(TRUE, $colors[$c]);

$plot->mark->setFill($colors[$c++]);
$plot->mark->setType(Mark::CIRCLE);
$plot->mark->setSize(2);
$plot->mark->border->hide(TRUE);



//Une fois votre courbe correctement paramétré, il est nécessaire de l'ajouter au graphique. Pour cela, une simple ligne suffit :

	$group->legend->add($plot, $row['display']?$row['display']:$row['config'], Legend::MARK);
	
   $group->add($plot);
   }

}
	$group->axis->left->label->setCallbackFunction('yLabel');
	$graph->add($group);
   $graph->draw();

   function yLabel($value) {
   	return engineer(significant($value, 3));
   }
   
?>