<?php

  // $Revision$





  require_once('ScPhpTestInterface.php');
  require_once('ScPhpTestableInterface.php');





  class ScPhpTest {





      public function run($HookClass) {

          $output       = array();
          $TestInstance = new $HookClass();

          foreach ($TestInstance->TestHooks() as $SetName => $Settings) {
              $output[] = array('name' => $SetName, 'result_type' => 'fail', 'reason' => 'Not yet loading hooks', 'contains'=>array());
          }

          return $output;

      }





  };





?>