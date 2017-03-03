<?
$csvfile = "settings.csv";

$json = file_get_contents('php://input');
$data = json_decode($json, true);
//var_dump($data);

// sanitize data for csv
$data = array_map(function($item) {
  $return = $item;
  if(is_array($item) || is_bool($item)) $return = json_encode($item);
  return $return;
}, $data);

// write csv header if file does not exist
if(!file_exists($csvfile)) {
  $fp = fopen($csvfile, "w");
  fputcsv($fp, array_keys($data));
  fclose($fp);
}

// append one line of csv data
$fp = fopen($csvfile, "a");
fputcsv($fp, $data);
fclose($fp);

?>
