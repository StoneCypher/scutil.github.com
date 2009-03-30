<?php

  // $Revision$





  require_once('ScPhpTestInterface.php');
  require_once('ScPhpTestableInterface.php');

  require_once('ScPhpTest_Result.php');





  class ScPhpTest {





      public function run($HookClass) {

          $output       = array();
          $TestInstance = new $HookClass();

          foreach ($TestInstance->TestHooks() as $SetName => $Settings) {
            
              require_once($Settings['file']);

              $ThisTest = new $Settings['class']();
              $output[] = $ThisTest->run();

          }

          return $output;

      }





  };





?>