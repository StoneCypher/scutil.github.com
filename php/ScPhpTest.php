<?php





  require_once('ScPhpTestableInterface.php');





  class ScPhpTest {





      public static function run($HookClass) {

          $output       = array();
          $TestInstance = new $HookClass();

          foreach ($TestInstance->Hooks() as $SetName => $Settings) {
              $output[] = array('name' => $SetName, 'result_type' => 'fail', 'reason' => 'Not yet loading hooks');
          }

          return $output;

      }





  };





?>