<?php

// Purpose: Convert sound header
// ==============================

require "lib/common.php";

if (!file_exists("tempconv.txt")) {
	die("can't find tempconv.txt");
}

const DELIM = "L";
const DELIMN = 0;

print "Converting data...".PHP_EOL;

$bit_status = [
	0 => "SIS_PAUSE",
	1 => "SIS_LOCKNRx2",
	2 => "SIS_USEDBYSFX",
	3 => "SIS_SFX",
	4 => "SIS_UNK_4",
	5 => "SIS_SLIDE",
	6 => "SIS_VIBRATO",
	7 => "SIS_ENABLED",
];
$sndptr_map = [
	'13' => "SND_CH1_PTR",
	'18' => "SND_CH2_PTR",
	'1D' => "SND_CH3_PTR",
	'22' => "SND_CH4_PTR",
];
$sndptrlbl_map = [
	'13' => ".ch1",
	'18' => ".ch2",
	'1D' => ".ch3",
	'22' => ".ch4",
];

$sndprefix_map = [
	'13' => "_Ch1",
	'18' => "_Ch2",
	'1D' => "_Ch3",
	'22' => "_Ch4",
];

$reps = [];
$result = "";

for ($i = 0, $lines = file("tempconv.txt"); $i < count($lines);) {
	
	if (strpos($lines[$i], "SndHeader_") !== 0){
		$result .= $lines[$i];
		++$i;
		continue;
	}
	
	$label = get_label($lines[$i]);
	$chcount = get_db($lines[$i]);
	
	$b = "{$label}:
	db \${$chcount} ; Number of channels";
	

	++$i;
	for ($j = 0; $j < $chcount; $j++) {
		$bank = substr(get_label($lines[$i]), 1, 2);
		
		$status = get_db($lines[$i++]);
		$soundptr = get_db($lines[$i++]);
		$dataptr_low = get_db($lines[$i++]);
		$dataptr_high = get_db($lines[$i++]);
		$freqbase = hexdec(get_db($lines[$i++]));
		$unused = get_db($lines[$i++]);
		
		$notedef = $freqbase ? "dnote ".mknote($freqbase) : "db \$00";
		
		$data_from = "L{$bank}{$dataptr_high}{$dataptr_low}";
		$data_to = str_replace("SndHeader_", "SndData_", $label).$sndprefix_map[$soundptr];
		if (isset($reps[$data_from])) die("shit");
		$reps[$data_from] = $data_to;
		
		$b .= "
".$sndptrlbl_map[$soundptr].":
	db ".generate_const_label($status, $bit_status)." ; Initial playback status
	db ".$sndptr_map[$soundptr]." ; Sound channel ptr
	dw {$data_from} ; Data ptr
	{$notedef} ; Base note
	db \${$unused} ; Unused";
	}
	
//$b .= "
//; END {$label} at ".get_label($lines[$i]);
	$result .= $b."\r\n";
}


file_put_contents("tempconv.asm", strtr($result, $reps));
