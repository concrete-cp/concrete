<?php
/*
 * This work is hereby released into the Public Domain.
 * To view a copy of the public domain dedication,
 * visit http://creativecommons.org/licenses/publicdomain/ or send a letter to
 * Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
 *
 */

require_once "../../BarPlot.class.php";

$graph = new Graph(400, 400);

$graph->setAntiAliasing(TRUE);

$values = array(19, 42, 15, -25, 3);
$plot = new BarPlot($values);
$plot->setBarColor(
	new Color(250, 230, 180)
);
$plot->setSpace(5, 5, NULL, NULL);

$plot->barShadow->setSize(4);
$plot->barShadow->setPosition(Shadow::RIGHT_TOP);
$plot->barShadow->setColor(new Color(180, 180, 180, 10));
$plot->barShadow->smooth(TRUE);

$graph->add($plot);
$graph->draw();

?>