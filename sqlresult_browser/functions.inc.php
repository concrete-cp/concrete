<?php
function significant($value, $nbchiffres) {
	if ($value == 0) { 
		return 0;
	}
	$level = ceil(log10(abs($value)));
	
	return round($value * pow(10, $nbchiffres-$level)) * pow(10, $level-$nbchiffres);
}



function engineer($value) {
	if ($value == 0) { 
		return 0;
	}
	$CONSTANTS = array(3 => 'G',  2 => 'M', 1 => 'k', 0 => '', -1 => 'm', -2 => '&mu;', -3 => 'n'); 
	
	$level = floor(log10(abs($value)) / 3);
	
	return $value * pow(10, -3*$level) . $CONSTANTS[$level];
}

function color($h, $s=1, $v=1) {
	$hi = floor(6*$h)%6;
	$f = 6*$h - floor(6*$h);
	$p = $v*(1-$s);
	$q = $v*(1-$f*$s);
	$t = $v*(1-(1-$f)*$s);
	switch($hi) {
	case 0:
		$r=$v;
		$g=$t;
		$b=$p;
		break;
	case 1:
		$r=$q;
		$g=$v;
		$b=$p;
		break;
	case 2:
		$r=$p;
		$g=$v;
		$b=$t;
		break;
	case 3:
		$r=$p;
		$g=$q;
		$b=$v;
		break;
	case 4:
		$r=$t;
		$g=$p;
		$b=$v;
		break;
	case 5:
		$r=$v;
		$g=$p;
		$b=$q;
		break;
	}

	$r = base_convert(floor(255*$r), 10, 16);
	$g = base_convert(floor(255*$g), 10, 16);
	$b = base_convert(floor(255*$b), 10, 16);
	
	if (strlen($r)<2) $r="0$r";
	if (strlen($g)<2) $g="0$g";
	if (strlen($b)<2) $b="0$b";
	
	
	return "#$r$g$b";
}

function median($array, $missing = 0) {
	$sorted = $array;
	for ($i = $missing ; --$i>=0;) {
		$sorted[] = -1;
	}
	usort($sorted, 'missingend');

	$second = $sorted[ceil((count($sorted)-1)/2)];
	
	if ($second < 0) {
		return NULL;
	}
	
	return ($sorted[floor((count($sorted)-1)/2)]+$second)/2;
}

function missingend($a, $b) {
	if ($a < 0) {
		return 1;
	}
	if ($b < 0) {
		return -1;
	}
	return $a - $b;
}
?>