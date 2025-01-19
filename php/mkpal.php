<?php

// Purpose: GBC Palette generator

const MAKE_PAL_FILE = true;
const BINARY_FILE = true;

if ($argc < 4) {
	usage_error("Bad arguments.");
}

$mode = strtolower($argv[1]);

if ($mode != 'e' && $mode != 'd')
	usage_error("Invalid mode.");

if (!file_exists($argv[2])) 
	die("Can't find {$argv[2]}");

if ($mode == 'e') {
	// png -> asm
	$len = $argc > 4 ? $argv[4] : 4;
	$in = imagecreatefrompng($argv[2]);
	$data = encode($in, $len);
	if (MAKE_PAL_FILE) {
		file_put_contents(substr($argv[3], 0, -4).".pal", $data[1]);
		file_put_contents($argv[3], $data[0]);
	} else {
		file_put_contents($argv[3], $data);
	}
} else {
	// asm -> png
	$in = file($argv[2]);
	$data = decode($in);
	imagepng($data, $argv[3]);
}

die;


function encode($in, $len = 4) {
	if (imagesx($in) < $len) {
		die("The image must be at least {$len}px in width");
	}
	
	// Grab the colors in a loop
	$ymax = imagesy($in);
	$res = "";
	$extragens = [];
	for ($y = 0; $y < $ymax; ++$y) {
		
		// Grab the colors in this palette line
		$pal = [];
		$bin = "";
		$sumchk = 0;
		$extragen = [];
		for ($x = 0; $x < $len; ++$x) {
			// Grab the pixel color at the location
			$rgb = imagecolorat($in, $x, $y);
			
			// Add to the sum of the rgb colors for the line (for the later end marker check)
			$sumchk += $rgb;
			
			// Get the individual components 
			$r = ($rgb >> 16) & 0xFF;
			$g = ($rgb >> 8) & 0xFF;
			$b = $rgb & 0xFF;
			
			if (MAKE_PAL_FILE) {
				$extragen[$x] = [$r, $g, $b];
			}
			
			// and convert them to 5bit color
			$r = floor($r/8);
			$g = floor($g/8);
			$b = floor($b/8);
			
			// Store them one after the other
			$rgb = $r + ($g << 5) + ($b << 10);
			
			IF (BINARY_FILE) {
				// Add the two bytes
				$bin .= chr($rgb & 0xFF).chr($rgb >> 8); // LE
			} else {
				// Convert them to gbchex format
				$pal[$x] = '$'.str_pad(dechex($rgb), 4, "0", STR_PAD_LEFT);
			}

		}
		
		// Grab an additional pixel for the end marker verification
		$sumchk += imagecolorat($in, $x, $y);
		
		// if we have *5* blacks in the palette (end marker), exit prematurely
		if (!$sumchk)
			break;
		
		if (MAKE_PAL_FILE) {
			$extragens[] = $extragen;
		}
		
		$res .= BINARY_FILE ? $bin : "\r\n\tdw ".implode(",", $pal);
	}
	
	if (MAKE_PAL_FILE) {
		$colcount = count($extragens) * $len;
		$palettesize = $colcount * 4;
		$totalsize = $palettesize + 24;
		
		$extrafile = "RIFF".pack("I", $totalsize)."PAL data".pack("I", $palettesize).chr(0).chr(3).pack("v", $colcount);
		foreach ($extragens as $y) {
			foreach ($y as $x)
				$extrafile .= chr($x[0]).chr($x[1]).chr($x[2]).chr(0);
		}
		return [$res, $extrafile];
	}
	
	return $res;
}

function decode($lines) {

	// Generate palette table
	$pals = [];
	foreach ($lines as $ln) {
		$x = trim($ln);
		if (!$x || strlen($x) < 8)
			continue;
		// requires no labels before 'dw', but whatever
		if ($x[0] != 'd' || $x[1] != 'w')
			continue;
		
		// Split after first space
		$p1 = strpos($x, " ")+1;
		$p2 = strpos($x, ";"); // and optionally remove comments
		if ($p2 === false)
			$p2 = null;
		else
			$p2 = $p2 - $p1;
		
		$x = trim(substr($x, $p1, $p2));
		
		
		
		$srcpal = explode(",", $x);
		$pal = [];
		for ($i = 0, $c = count($srcpal); $i < $c; ++$i) {
			// Convert to number
			//print $srcpal[$i].PHP_EOL;
			$rgb = hexdec(rtrim(substr(ltrim($srcpal[$i]), 1))); // skip "$"
			$pal[$i] = $rgb;
		}			
		$pals[] = $pal;
	}
	
	// Determine largest color entry
	$maxnum = 0;
	foreach ($pals as $x) {
		$maxnum = max($maxnum, count($x));
	}
	
	
	// Convert them to normal pixels
	$img = imagecreatetruecolor($maxnum, count($pals)+1); // + extra black row for optional marker
	
	for ($y = 0, $ymax = imagesy($img) - 1; $y < $ymax; ++$y) {
		for ($x = 0, $xmax = count($pals[$y]); $x < $xmax; ++$x) {
			// Get the decimal color
			$rgb = $pals[$y][$x];
			
			// Extract the GBC color components
			$r = (($rgb) & 0x1F) * 8;
			$g = (($rgb >> 5) & 0x1F) * 8;
			$b = (($rgb >> 10) & 0x1F) * 8;
			
			// Write it over as a standard RGB color
			$col = imagecolorallocate($img, $r, $g, $b);
			imagesetpixel($img, $x, $y, $col);
		}			
	}
	// Add an additional black line
	imageline($img, 0, $ymax, $maxnum, $ymax, imagecolorallocate($img, 0,0,0));
	
	return $img;
}

function usage_error($err) {
	print $err.PHP_EOL;
	print "Usage: mkpal.php e/d [infile] [outfile] [length=4]".PHP_EOL."\t- e: Encode palette".PHP_EOL."\t- d: Decode palette".PHP_EOL."\t- [length]: Width of palette table, in pixels. If not specified, it's 4";
	die;
}