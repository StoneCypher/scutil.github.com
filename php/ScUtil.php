<?php

  // $Revision$
  // Since Apr 2, 2009
  // $Date$
  // $HeadURL$
  // $Id$





  require_once('ScPhpTest.php');





  // can't use class wrapped version enforcement in case the class breaks from too-old php before it gets there, lol
  // always use the class method version instead plskthx

  $ScPhpUtil_MinPhpVer = '5.1.6';

  if (!(version_compare(phpversion(), $ScPhpUtil_MinPhpVer) === 1)) {
      die("<html><head><title>Needs newer PHP</title></head><body><p>Requires a minimum PHP version of <tt>$ScPhpUtil_MinPhpVer</tt>.</p></body></html>");
  }





  class Sc implements ScPhpTestableInterface {





      public static function WriteIf($Clause, $Text) {
          return (($Clause)? $Text : '');
      }





      public static function EchoIf($Clause, $Text) {
          echo WriteIf($Clause, $Text);
      }





      public static function UseStartPoint($StartPoint) {

          if (!(is_array($StartPoint))) {
              $StartPoint = array($StartPoint);
          }

          foreach ($StartPoint as $sp) {
              self::$StartPointsUsed[$sp] = true;
          }

      }





      public function TestHooks() {

          return array('ScPhpUtil Master Test' => array('file'=>'ScPhpUtil_Testset.php', 'class'=>'ScPhpUtil_Testset', 'args'=>array()) );

      }





      // Unfortunately since PHP scans and lexes a file before executing it, neither this code nor anything like it can
      // prevent syntax errors from too-old PHP editions.  But hey, it's a start.
      public static function EnforceMinimumPhpVersion($ver, $message) {

          if (!(version_compare(phpversion(), $ver) === 1)) {
              die($message);
          }

      }





      public static function rgb_to_hsv($R, $G, $B) { // RGB Values:Number 0-255, // HSV Results:Number 0-1

          // The hue, saturation and lightness are an empty array
          $HSL = array();

          // The real value for each channel is the color value for that channel divided by 255
          $vR = ($R / 255);
          $vG = ($G / 255);
          $vB = ($B / 255);

          // Get the highest and lowest real value, to direct the hue wheel
          $vMin = min($vR, $vG, $vB);
          $vMax = max($vR, $vG, $vB);

          // and the range between them
          $range = $vMax - $vMin;

          // The value is the maximum value found from the three color channels; other channels are described as fractions of this, one of which should be 1.0
          $V = $vMax;

          // if there's no difference between the minimum and maximum range, then we're looking at a grayscale, and can skip a bunch of computation
          if ($range == 0) {

              $H = 0;
              $S = 0;

          // or not
          } else {

              // The saturation is (the difference between minimum and maximum value) divided by maximum value - that is, the ratio of the value range to the max value
              $S      = $range / $vMax;
              $hRange = $range / 2;

              // Delete range edge lengths to get hue wheel position, which will be on the range [ -1/6, 1/6 ]
              $del_R = ( ( ( $vMax - $vR ) / 6 ) + $hRange ) / $range;
              $del_G = ( ( ( $vMax - $vG ) / 6 ) + $hRange ) / $range;
              $del_B = ( ( ( $vMax - $vB ) / 6 ) + $hRange ) / $range;

              // Hue is the delete range plus N/3
              if      ($vR == $vMax) { $H =         $del_B - $del_G; }
              else if ($vG == $vMax) { $H = (1/3) + $del_R - $del_B; }
              else if ($vB == $vMax) { $H = (2/3) + $del_G - $del_R; }

              // This can result in an over-range by 1/6 in either direction, whereupon it's safe to return a full increment/decrement to re-range
              if ($H < 0) {
                  $H++;
              }

              if ($H > 1) {
                  $H--;
              }

          }

          // return array h,s,v => 0.0 .. 1.0 inclusive
          return array(
              'h' => $H,
              's' => $S,
              'v' => $V
          );

      }





      public static function hsv_to_rgb($H, $S, $V) { // HSV Values:Number 0-1, RGB Results:Number 0-255

          $RGB = array();

          if ($S == 0) {

              $R =
              $G =
              $B = $V * 255;

          } else {

              $var_H = $H * 6;
              $var_i = floor( $var_H );

              $var_1 = $V * ( 1 - $S );
              $var_2 = $V * ( 1 - $S *      ( $var_H - $var_i )   );
              $var_3 = $V * ( 1 - $S * (1 - ( $var_H - $var_i ) ) );

              if      ($var_i == 0) { $vR = $V;     $vG = $var_3; $vB = $var_1; }
              else if ($var_i == 1) { $vR = $var_2; $vG = $V;     $vB = $var_1; }
              else if ($var_i == 2) { $vR = $var_1; $vG = $V;     $vB = $var_3; }
              else if ($var_i == 3) { $vR = $var_1; $vG = $var_2; $vB = $V;     }
              else if ($var_i == 4) { $vR = $var_3; $vG = $var_1; $vB = $V;     }
              else                  { $vR = $V;     $vG = $var_1; $vB = $var_2; }

              $R = $vR * 255;
              $G = $vG * 255;
              $B = $vB * 255;

          }

          return array('r'=>$R, 'g'=>$G, 'b'=>$B);

      }





      public static function any($items) {

          foreach ($items as $i) {
              if ($i == true) { 
                  return true; 
              }
          }

          return false;

      }





      public static function strong_any($items) {

          foreach ($items as $i) {
              if ($i === true) { 
                  return true; 
              }
          }

          return false;

      }





      public static function all($items) {

          foreach ($items as $i) {
              if ($i != true) { 
                  return false; 
              }
          }

          return true;

      }





      public static function strong_all($items) {

          foreach ($items as $i) {
              if ($i !== true) { 
                  return false; 
              }
          }

          return true;

      }





      public static function has_each_key($target, $keylist) {

          foreach ($keylist as $key) {
              if (!(isset($target[$key]))) { 
                  return false; 
              }
          }

          return true;

      }





      // potentially very expensive!

//      public static function has_each_member($target, $memberlist) {
//
//          // TODO look for a non-brute-iterative approach to this, as this is too slow
//          // in C++ it's easy to make this o(lg n)
//          foreach ($memberlist as $m) {
//              if (
//          }
//
//      }





      public static function member_set_or($target, $key, $default) {

          return ((isset($target[$key]))? ($target[$key]) : ($default));

      }





      function flatten_once($Array) {

          $out = array();
          foreach ($Array as $val) {
              if (is_array($val)) {
                  foreach ($val as $v) {
                      $out[] = $v;
                  }
              } else {
                  $out[] = $val;
              }
          }

          return $out;

      }





      function flatten($Array) {

          $out = array();
          foreach ($Array as $val) {
              if (is_array($val)) {
                  foreach (flatten($val) as $v) {
                      $out[] = $v;
                  }
              } else {
                  $out[] = $val;
              }
          }

          return $out;

      }





      // unfortunately this only works with non-temporaries, due to the reference; PHP has no mechanism
      // for doing this that will work with temporaries of which I am aware (not even macros)

      public static function val_if_set_or(&$var, $default) {

          if (isset($var)) {
              return $var;
          }

          return $default;

      }





      function zip($inputs) {

          $nArgs = func_num_args();

          if ($nArgs < 1) {
              return false;
          }

          $inputs   = array(NULL);
          $matchLen = false;

          for ($i=0; $i<$nArgs; ++$i) {

              $inputs[$i+1] = func_get_arg($i);

              if ($i == 0) {
                  $matchLen = count($inputs[$i+1]);
              }

              if (!(is_array($inputs[$i+1]))) {
                  return false;
              }

              if (count($inputs[$i+1]) != $matchLen) {
                  return false;
              }

          }

          return call_user_func_array('array_map', $inputs);

      }

// echo "1)\n";
// print_r(zip(array(1,2,3),array(4,5,6)));
//
// echo "2)\n";
// print_r(zip(array(1,2,3),array(4,5)));
//
// echo "3)\n";
// print_r(zip(array(1,2,3),5));
//
// echo "4)\n";
// print_r(zip(array(1,2,3,4),array(5,6,7,8),array(9,10,11,12)));





      private static $StartPointsUsed = array();





  };

?>
