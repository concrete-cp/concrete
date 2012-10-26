<?php

require_once "../ScatterPlot.class.php";

$graph = new Graph(400, 400);

$center = 5;

$x = array();
$y = array();

for($i = 0; $i <= 30; $i++) {
	$rad = ($i / 30) * 2 * M_PI;
	$x[] = $center + cos($rad) * $center;
	$y[] = $center + sin($rad) * $center;
}

$plot = new ScatterPlot($y, $x);
$plot->setBackgroundColor(new VeryLightGray);
$plot->setPadding(30, 30, 30, 30);
$plot->setSpace(5, 5, 5, 5);

$plot->link(TRUE, new DarkGreen);

$plot->mark->setFill(new DarkOrange);
$plot->mark->setType(Mark::SQUARE, 4);

$plot->setXAxis(Plot::BOTH);
$plot->setXAxisZero(FALSE);
$plot->setYAxis(Plot::BOTH);

$plot->legend->add($plot, 'A circle', Legend::MARK);
$plot->legend->setPosition(0.5, 0.5);
$plot->legend->setAlign(Legend::CENTER, Legend::MIDDLE);

$graph->add($plot);
$graph->draw();

?>