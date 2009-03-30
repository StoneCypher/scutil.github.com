<?php





  class ScPhpUtil_Testset implements ScPhpTestInterface {





      public function run() {

          return array(
              new ScPhpTest_Result('Running', 'pass')
          );

      }





  };





?>