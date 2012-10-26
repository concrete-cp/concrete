<?php
require("mysql.inc.php");
require("functions.inc.php");
require("artichow/BarPlot.class.php");

   // Il est toujours nécessaire de donner une taille à la création de votre graphique.
   // Ici, le graphique mesurera 400 x 400 pixels.
   $graph = new Graph(100, 50);
    
    $graph->border->hide();
    
    
    $i = 0;
    $medians=array();
    while (isset($_GET["v$i"])) {
    	$medians[] = $_GET["v$i"];
    	$i++;
    }
    
    
    // On créé la courbe
   $plot = new BarPlot($medians);

	$plot->grid->hide(TRUE);	
	$plot->setSpace(1,1,1,1);
	$plot->setPadding(1,1,1,1);   
   $plot->setBarColor(new DarkBlue);

//Une fois votre courbe correctement paramétré, il est nécessaire de l'ajouter au graphique. Pour cela, une simple ligne suffit :

   $graph->add($plot);


//$group->axis->bottom->setLabelText($configs);
//	$graph->add($group);
   $graph->draw();

?>