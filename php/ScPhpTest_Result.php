<?php





  class ScPhpTest_Result {





      private $ResultName;
      private $ResultState;
      private $ResultReason;





      public function ResultType() {

          if ($contents === false) {
              return $ThisPassed;
          }

      }





      public function isPass() {
          return (ResultType() === 'pass');
      }





      public function isWarn() {
          return (ResultType() === 'warn');
      }




      public function isFail() {
          return (ResultType() === 'fail');
      }





      public function __construct($name, $state, $reason='No reason given') {

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