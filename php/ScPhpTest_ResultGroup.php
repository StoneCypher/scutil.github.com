<?php

  // $Revision: 286 $

  require_once('ScPhpTest_ResultBase.php');





  class ScPhpTest_ResultGroup extends ScPhpTest_ResultBase {





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





  };





?>