
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p>The purpose of the 'raster' package is to provide easy to use functions for raster type spatial data manipulation and analysis. The functions in this package include high level functions such as overlay, merge, aggregate, projection, resample, distance, and polygon to raster conversion. All these functions work for very large raster datasets that cannot be loaded into RAM memory. In addition, the package provides lower level functions such as row by row reading and writing (to many formats via rgdal) for building other functions.</p>
</br>
<p>The package is build around a number of S4 classes of which the RasterLayer, RasterStack, and RasterBrick classes are the most important. Many generic methods are implemented for RasterLayers (arithmic, logical, plot, hist, ...). All code is written in R. 
</p>
</br>
<p>The raster package is used by other packages, including 'dismo' <a href="http://gdistance.r-forge.r-project.org/">gdistance</a> for matrix based (cost, resistance) distance calculations.</p>
</br>
<p>One of the main reasons for developing this package is that we would like to build a bridge to - the powerful statistics and modeling of - R for the "GIS" community.</p>
</br>
We welcome your feedback and suggestions.
</br></br>
For more information on spatial data analysis with R seen the <a href="http://cran.r-project.org/web/views/Spatial.html">CRAN task view</a> on that subject.
</br>
</p>

<p>Go to the <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>development pages</strong></a> to download the package, to report bugs, ask for features, and more. </p>

</body>
</html>
