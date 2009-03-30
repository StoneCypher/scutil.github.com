<?php

  // $Revision: 286 $

  require_once('ScPhpTest_ResultBase.php');





  abstract class ScPhpTest_ResultBase {





      abstract public function ResultType();





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