<?php

  // $Revision$

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




      public static function EnforceMinimumPhpVersion($ver, $message) {

          if (!(version_compare(phpversion(), $ver) === 1)) {
              die($message);
          }

      }





      private static $StartPointsUsed = array();





  };

?>