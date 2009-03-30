<?php





  final class ScPhpTest_Result_Pass extends ScPhpTest_Result {





      public function isPass() { return true;  }
      public function isWarn() { return false; }
      public function isFail() { return false; }





      public function __construct($cReason = 'No reason given') {
        $reason = $cReason;
      }





      public $reason = false;





  };





?>