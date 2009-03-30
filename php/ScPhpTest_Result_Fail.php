<?php





  final class ScPhpTest_Result_Fail extends ScPhpTest_Result {





      public function isPass() { return false; }
      public function isWarn() { return false; }
      public function isFail() { return true;  }





      public function __construct($cReason = 'No reason given') {
        $reason = $cReason;
      }





      public $reason = false;





  };





?>