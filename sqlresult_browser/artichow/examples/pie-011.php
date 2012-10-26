<?php
/*
 * This work is hereby released into the Public Domain.
 * To view a copy of the public domain dedication,
 * visit http://creativecommons.org/licenses/publicdomain/ or send a letter to
 * Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
 *
 */

require_once "../Pie.class.php";

function createPie($values, $title, $x, $y) {
	
	$plot = new Pie($values, Pie::EARTH);
	$plot->title->set($title);
	$plot->title->setFont(new TuffyBold(8));
	$plot->title->move(NULL, -12);
	
	$plot->label->setFont(new Tuffy(7));
	$plot->legend->hide(TRUE);
	$plot->setLabelPosition(5);
	$plot->setSize(0.45, 0.45);
	$plot->setCenter($x, $y);
	$plot->set3D(10);
	$plot->setBorderColor(new Color(230, 230, 230));
	
	return $plot;

}

$graph = new Graph(400, 300);

$plot = createPie(array(1, 4, 5, 2, 3), "Cowléoptère", 0.22, 0.25);
$graph->add($plot);

$plot = createPie(array(1, 9, 1, 2, 1), "Asticow", 0.68, 0.25);
$graph->add($plot);

$plot = createPie(array(5, 7, 8, 6, 3), "Cowlibri", 0.22, 0.75);
$graph->add($plot);

$plot = createPie(array(6, 4, 6, 5, 6), "Bourricow", 0.68, 0.75);
$plot->legend->hide(FALSE); // We print only one legend
$plot->legend->setPosition(1.18, 0); 
$graph->add($plot);

$graph->draw();
?>