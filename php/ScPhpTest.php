<?php

  // $Revision$





  require_once('ScPhpTestInterface.php');
  require_once('ScPhpTestableInterface.php');





  class ScPhpTest {





      public function run($HookClass) {

          $output       = array();
          $TestInstance = new $HookClass();

          foreach ($TestInstance->TestHooks() as $SetName => $Settings) {
            
              require_once($Settings['file']);
              $ThisTest = new $Settings['class']();
              $result   = $ThisTest->run();

              $output[] = array('name' => $SetName, 'result_type' => 'fail', 'reason' => 'Not yet loading hooks', 'contains'=>$result);

          }

          return $output;

      }





  };





?>