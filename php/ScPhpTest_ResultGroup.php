<?php

  // $Revision$

  require_once('ScPhpTest_ResultBase.php');





  class ScPhpTest_ResultGroup extends ScPhpTest_ResultBase {





      private $Contents;





      public function ResultType() {
          // todo
          return 'fail';
      }





      public function TestType() {
          return 'group';
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





      public function __construct($name, $children) {

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