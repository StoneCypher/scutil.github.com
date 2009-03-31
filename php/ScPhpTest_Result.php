<?php

  // $Revision$

  require_once('ScPhpTest_ResultBase.php');





  class ScPhpTest_Result extends ScPhpTest_ResultBase {





      private $TestType;
      private $ResultName;
      private $ResultState;
      private $ResultReason;





      public function ResultType() {
          return $this->ResultState;
      }





      public function TestType() {
          return $this->TestType;
      }





      public function __construct($testtype, $name, $state, $reason='No reason given') {

          $this->TestType     = $testtype;
          $this->ResultName   = $name;
          $this->ResultReason = $reason;

          switch ($state) {

              case 'pass' : $this->ResultState = 'pass'; break;
              case 'warn' : $this->ResultState = 'warn'; break;
              case 'fail' : $this->ResultState = 'fail'; break;

              default     : die("Illegal result state: $state in constructing ScPhpTest_Result for $name");

          }

      }





  };





?>